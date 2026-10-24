# Assembly helpers for Stage 2 outputs.

write_csv_out <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x, path)
  path
}

observed_rounds_by_year <- function(sa) {
  r <- sa |> dplyr::mutate(rn = suppressWarnings(as.integer(round_label))) |>
    dplyr::group_by(year) |> dplyr::summarise(max_round = max(rn, na.rm = TRUE), .groups = "drop")
  stats::setNames(r$max_round, r$year)
}

age_years <- function(birth, on) as.numeric(on - birth) / 365.25

build_pick_table <- function(pj, slots, draft_dates, rules, chad, people, rankings) {
  j <- pj
  # B-Ref-only picks have no Stats API round label; B-Ref files supplemental picks under the preceding round
  j$round_label <- dplyr::coalesce(j$round_label, j$br_round)
  j <- dplyr::left_join(j, dplyr::select(slots, year, pick_overall, slot, slot_status, slot_source), by = c("year", "pick_overall"))
  j$pool_round <- is_top10_label(j$round_label)

  # Stats API people: current debut date; first affiliated pro season (signed-status evidence)
  j <- dplyr::left_join(j, people$people |> dplyr::select(mlbam_id, people_debut), by = "mlbam_id")
  fps <- people$seasons |> dplyr::filter(sport_id %in% c(1L, 11:16, 5442L)) |> dplyr::group_by(mlbam_id) |>
    dplyr::summarise(first_pro_season = min(season), .groups = "drop")
  j <- dplyr::left_join(j, fps, by = "mlbam_id")
  j$pro_by_next_season <- !is.na(j$first_pro_season) & j$first_pro_season <= j$year + 1

  # repeat draftees
  j$player_key <- ifelse(!is.na(j$mlbam_id), paste0("mlbam:", j$mlbam_id),
                  ifelse(!is.na(j$bbref_minors_id), paste0("br:", j$bbref_minors_id), paste0("pick:", j$year, "-", j$pick_overall)))
  j <- j |> dplyr::arrange(player_key, year) |> dplyr::group_by(player_key) |>
    dplyr::mutate(draft_instance = dplyr::row_number(), n_times_drafted = dplyr::n()) |> dplyr::ungroup()
  j$later_drafted <- j$draft_instance < j$n_times_drafted

  j <- j |> reconcile_bonus() |> reconcile_signed() |> derive_types()

  # pool accounting fields
  thr <- rules |> dplyr::filter(rule == "r11_threshold_usd") |> dplyr::transmute(year = as.integer(year), r11_threshold = suppressWarnings(as.numeric(value)))
  j <- dplyr::left_join(j, thr, by = "year")
  j$ratio_to_slot <- ifelse(j$pool_round & !is.na(j$slot), j$bonus / j$slot, NA_real_)
  j$r11_at_or_below_threshold <- ifelse(!j$pool_round & !is.na(j$bonus) & !is.na(j$r11_threshold), j$bonus <= j$r11_threshold, NA)
  j$r11_excess_counts <- ifelse(!j$pool_round & !is.na(j$bonus) & !is.na(j$r11_threshold), pmax(0, j$bonus - j$r11_threshold), NA_real_)

  # ages: draft start date (MLB.com, cited per year) and June 5 of draft year (Rule 5 reference date)
  j <- dplyr::left_join(j, draft_dates |> dplyr::select(year, draft_start), by = "year")
  j$age_at_draft <- age_years(j$birth_date, j$draft_start)
  j$age_june5 <- age_years(j$birth_date, as.Date(sprintf("%d-06-05", j$year)))
  j$rule5_young <- floor(j$age_june5) <= 18

  # pre-draft rank (MLB Pipeline draft list of the draft year); supply modeling only
  dr <- rankings |> dplyr::filter(list == "draft") |> dplyr::select(year = list_year, mlbam_id, pipeline_draft_rank = rank)
  j <- dplyr::left_join(j, dplyr::distinct(dr, year, mlbam_id, .keep_all = TRUE), by = c("year", "mlbam_id"))
  j <- crosswalk_link(j, chad)

  # A player signs at most once as a Rule 4 draftee; n_signed_picks > 1 would be a data error
  j |> dplyr::group_by(player_key) |> dplyr::mutate(n_signed_picks = sum(signed %in% TRUE)) |> dplyr::ungroup() |>
    dplyr::mutate(signing_pick = signed %in% TRUE) |>
    dplyr::arrange(year, pick_overall)
}

reconciliation_table <- function(p) {
  p |> dplyr::transmute(year, pick_overall, round_label, mlbam_id, sa_name = full_name, br_name = br_player, name_agree,
    sa_team = team_name, br_team, slot, sa_signing_bonus, br_bonus, br_signed, bonus_status, bonus, bonus_diff, bonus_source_reason,
    first_pro_season, later_drafted, signed, signed_basis, signed_conflict)
}
