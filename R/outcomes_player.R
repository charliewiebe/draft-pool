# Player-level outcomes, control-period approximation, censoring; intermediate-outcome panel.
# Data through the 2026 season (complete as of the 2026-10-20 pull).
#
# Control period (approximation): the first six distinct seasons with an MLB appearance, starting with the debut season.
# MLB season = Stats API MLB games > 0 or a B-Ref WAR row with PA > 0 or IP > 0.

player_outcomes <- function(p, seasons, rankings, last_season = 2026L, exit_gap = 2L) {
  sg <- p |> dplyr::filter(signing_pick, !is.na(mlbam_id)) |>
    dplyr::select(year, pick_overall, mlbam_id, school_type, role, bonus, people_debut)
  s <- seasons |> dplyr::filter(mlbam_id %in% sg$mlbam_id) |>
    dplyr::mutate(mlb = dplyr::coalesce(mlb_games, 0L) > 0 | dplyr::coalesce(pa, 0) > 0 | dplyr::coalesce(ipouts, 0) > 0,
                  pro = mlb | !is.na(milb_top_level), bwar = dplyr::coalesce(bwar, 0))
  mlb <- s |> dplyr::filter(mlb) |> dplyr::arrange(mlbam_id, season) |> dplyr::group_by(mlbam_id) |>
    dplyr::mutate(k = dplyr::row_number()) |>
    dplyr::summarise(first_mlb_season = min(season), last_mlb_season = max(season), mlb_seasons = dplyr::n(),
      v_war_control = sum(bwar[k <= 6]), control_seasons_obs = sum(k <= 6), control_last_season = max(season[k <= 6]),
      war_to_date = sum(bwar), peak_war = max(bwar), .groups = "drop")
  pro <- s |> dplyr::filter(pro) |> dplyr::group_by(mlbam_id) |>
    dplyr::summarise(first_pro_season = min(season), last_pro_season = max(season), .groups = "drop")
  t100 <- rankings |> dplyr::filter(list == "top100") |> dplyr::group_by(mlbam_id) |>
    dplyr::summarise(top100_best_rank = min(rank), top100_lists = dplyr::n(), top100_first_year = min(list_year), .groups = "drop")
  o <- sg |> dplyr::left_join(mlb, by = "mlbam_id") |> dplyr::left_join(pro, by = "mlbam_id") |> dplyr::left_join(t100, by = "mlbam_id")
  o |> dplyr::mutate(
    debut_season = dplyr::coalesce(as.integer(format(people_debut, "%Y")), first_mlb_season),
    debuted = !is.na(debut_season),
    seasons_observed = last_season - year,
    out_of_pro = !is.na(last_pro_season) & last_pro_season <= last_season - exit_gap | is.na(last_pro_season) & year <= last_season - exit_gap,
    arrival_status = dplyr::case_when(debuted ~ "arrived", out_of_pro ~ "never_arrived", TRUE ~ "censored"),
    seasons_to_debut = debut_season - year,
    mlb_seasons = dplyr::coalesce(mlb_seasons, 0L), v_war_control_obs = dplyr::coalesce(v_war_control, 0),
    control_status = dplyr::case_when(
      !debuted & out_of_pro ~ "no_debut_resolved", !debuted ~ "no_debut_censored",
      mlb_seasons >= 6 ~ "complete",
      out_of_pro ~ "exited_before_6", TRUE ~ "censored"),
    v_war_resolved = control_status %in% c("complete", "exited_before_6", "no_debut_resolved"),
    v_war = ifelse(v_war_resolved, v_war_control_obs, NA_real_),
    v_war_lower_bound = v_war_control_obs,
    ever_top100 = !is.na(top100_best_rank))
}

player_progress <- function(p, seasons, rankings, last_season = 2026L) {
  sg <- p |> dplyr::filter(signing_pick, !is.na(mlbam_id)) |> dplyr::select(year, pick_overall, mlbam_id)
  grid <- sg |> dplyr::rowwise() |> dplyr::reframe(year = year, pick_overall = pick_overall, mlbam_id = mlbam_id, season = year:last_season) |> dplyr::ungroup()
  t100 <- rankings |> dplyr::filter(list == "top100") |> dplyr::select(mlbam_id, season = list_year, top100_rank = rank) |>
    dplyr::distinct(mlbam_id, season, .keep_all = TRUE)
  grid |> dplyr::left_join(seasons |> dplyr::select(mlbam_id, season, milb_top_level, mlb_games, bwar), by = c("mlbam_id", "season")) |>
    dplyr::left_join(t100, by = c("mlbam_id", "season")) |>
    dplyr::mutate(years_since_draft = season - year, mlb_games = dplyr::coalesce(mlb_games, 0L))
}
