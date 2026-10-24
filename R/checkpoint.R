# Stage 2 checkpoint diagnostics. Returns named list of tibbles; also writes them to data/derived/checkpoint/.

checkpoint_summary <- function(sa, br, pj, p, slots, slot_check, pools, rules, chad, people, war, rankings,
                               pull_date = as.Date("2026-10-20"), out_dir = "data/derived/checkpoint") {
  pull_year <- as.integer(format(pull_date, "%Y"))
  rgrp <- function(lbl) ifelse(is_top10_label(lbl), "R1-10 (pool)", "R11+")
  out <- list()

  out$raw_inventory <- {
    log <- data.table::fread(file.path("data", "raw", "request_log.csv"))
    log$source <- sub("^data/raw/([^/]+/?[^/]*)/.*$", "\\1", log$dest)
    log[, .(requests = .N, ok = sum(status == 200), non_200 = sum(status != 200), mb = round(sum(bytes, na.rm = TRUE) / 1e6, 1)), by = source]
  }

  out$join_counts <- pj |> dplyr::group_by(year) |> dplyr::summarise(
    sa_rows = sum(in_sa), br_rows = sum(in_br), joined = dplyr::n(), both = sum(in_sa & in_br),
    sa_only = sum(in_sa & !in_br), br_only = sum(!in_sa & in_br), name_agree = sum(name_agree),
    name_disagree = sum(in_sa & in_br & !name_agree), .groups = "drop")

  out$bonus_status <- p |> dplyr::mutate(grp = rgrp(round_label)) |> dplyr::count(year, grp, bonus_status) |>
    tidyr::pivot_wider(names_from = bonus_status, values_from = n, values_fill = 0)

  out$bonus_coverage <- p |> dplyr::filter(signed %in% TRUE) |> dplyr::mutate(grp = rgrp(round_label)) |>
    dplyr::group_by(year, grp) |> dplyr::summarise(signed = dplyr::n(), bonus_known = sum(!is.na(bonus)),
      coverage = round(mean(!is.na(bonus)), 3), .groups = "drop") |>
    tidyr::pivot_wider(names_from = grp, values_from = c(signed, bonus_known, coverage))

  out$bonus_missing_pattern <- p |> dplyr::filter(signed %in% TRUE, !is_top10_label(round_label)) |>
    dplyr::mutate(round_num = suppressWarnings(as.integer(round_label)), rband = cut(round_num, c(10, 15, 20, 30, 40))) |>
    dplyr::group_by(era = ifelse(year <= 2016, "2012-16", ifelse(year <= 2019, "2017-19", "2021-25")), rband,
                    school_type) |>
    dplyr::summarise(signed = dplyr::n(), missing = round(mean(is.na(bonus)), 3), .groups = "drop")

  out$bonus_disagreements <- p |> dplyr::filter(bonus_status == "disagree") |>
    dplyr::mutate(abs_diff = abs(bonus_diff), rel = abs(bonus_diff) / pmax(sa_bonus_pos, br_bonus)) |>
    dplyr::group_by(year) |> dplyr::summarise(n = dplyr::n(), med_abs = stats::median(abs_diff), max_abs = max(abs_diff),
      n_rel_gt_10pct = sum(rel > 0.10), .groups = "drop")

  out$signed_status <- p |> dplyr::mutate(grp = rgrp(round_label)) |> dplyr::group_by(year, grp) |>
    dplyr::summarise(picks = dplyr::n(), n_signed = sum(signed %in% TRUE), n_unsigned = sum(signed %in% FALSE),
      n_unknown = sum(is.na(signed)), n_conflict = sum(signed_conflict), signing_rate = round(mean(signed %in% TRUE), 3), .groups = "drop")

  out$bonus_reason <- dplyr::count(p, bonus_source_reason)
  out$signed_basis <- dplyr::count(p, signed_basis)
  out$signed_vs_pro <- p |> dplyr::filter(year <= pull_year - 2, n_times_drafted == 1) |>
    dplyr::count(signed_basis, pro_by_next_season) |>
    tidyr::pivot_wider(names_from = pro_by_next_season, values_from = n, names_prefix = "pro_by_next_", values_fill = 0)

  out$repeat_draftees <- p |> dplyr::distinct(player_key, n_times_drafted, n_signed_picks) |>
    dplyr::count(n_times_drafted, n_signed_picks)

  out$slots_by_year <- slots |> dplyr::count(year, slot_status, slot_source)
  out$slot_crosscheck <- slot_check
  out$pool_reconstruction <- pools |> dplyr::group_by(year) |> dplyr::summarise(teams = dplyr::n(),
    exact = sum(diff == 0), within_500 = sum(abs(diff) <= 500), mismatches = sum(abs(diff) > 500),
    max_abs_diff = max(abs(diff)), league_reconstructed = sum(pool_reconstructed), league_published = sum(pool_published), .groups = "drop")
  out$pool_mismatches <- dplyr::filter(pools, abs(diff) > 500)

  out$rules_status <- rules |> dplyr::count(status)

  # Data check of the rounds 11+ threshold: modal R11+ bonus vs the rules-table threshold
  out$r11_bunching <- p |> dplyr::filter(!pool_round, !is.na(bonus), !is.na(r11_threshold)) |>
    dplyr::group_by(year) |> dplyr::summarise(r11_bonuses = dplyr::n(), threshold = dplyr::first(r11_threshold),
      modal_bonus = as.numeric(names(which.max(table(bonus)))), at_threshold = sum(bonus == r11_threshold),
      above_threshold = sum(bonus > r11_threshold), share_at = round(mean(bonus == r11_threshold), 3), .groups = "drop") |>
    dplyr::mutate(mode_matches_threshold = modal_bonus == threshold)

  out$types <- p |> dplyr::group_by(year) |> dplyr::summarise(
    school_type_known = round(mean(!is.na(school_type)), 3), st_agree_rate = round(mean(school_type_agree, na.rm = TRUE), 3),
    n_st_compared = sum(!is.na(school_type_agree)), HS = sum(school_type %in% "HS"), four_yr = sum(school_type %in% "4yr"),
    JUCO = sum(school_type %in% "JUCO"), role_known = round(mean(!is.na(role)), 3),
    role_agree_rate = round(mean(role_agree, na.rm = TRUE), 3), two_way = sum(two_way_flag),
    age_known = round(mean(!is.na(age_at_draft)), 3), college_class_known = round(mean(!is.na(college_class)), 3), .groups = "drop")

  out$crosswalk <- p |> dplyr::group_by(year) |> dplyr::summarise(picks = dplyr::n(),
    chad_linked = round(mean(chad_linked), 3), fangraphs_id = round(mean(!is.na(chad_fangraphs)), 3),
    bbref_mlb_id = round(mean(!is.na(chad_bbref)), 3),
    bbref_minors_agree = round(mean(chad_bbref_minors_agree, na.rm = TRUE), 3), .groups = "drop")

  # outcome maturity (signed picks only)
  ps <- people$seasons |> dplyr::filter(sport_id %in% c(1L, 11:16, 5442L)) |> dplyr::group_by(mlbam_id) |>
    dplyr::summarise(last_pro = max(season), .groups = "drop")
  mlbs <- people$seasons |> dplyr::filter(sport_id == 1L) |> dplyr::group_by(mlbam_id) |>
    dplyr::summarise(mlb_seasons = dplyr::n_distinct(season), .groups = "drop")
  out$outcome_maturity <- p |> dplyr::filter(signing_pick) |> dplyr::left_join(ps, by = "mlbam_id") |>
    dplyr::left_join(mlbs, by = "mlbam_id") |>
    dplyr::mutate(debuted = !is.na(people_debut), mlb_seasons = dplyr::coalesce(mlb_seasons, 0L),
      out_of_pro = !is.na(last_pro) & last_pro <= pull_year - 2,
      arrival_resolved = debuted | out_of_pro,
      control_resolved = (debuted & mlb_seasons >= 6) | (out_of_pro)) |>
    dplyr::group_by(year) |> dplyr::summarise(signed = dplyr::n(), debuted = round(mean(debuted), 3),
      arrival_resolved = round(mean(arrival_resolved), 3), control_period_resolved = round(mean(control_resolved), 3),
      years_observed = pull_year - dplyr::first(year), .groups = "drop")

  out$war_link <- p |> dplyr::filter(signing_pick, !is.na(people_debut)) |>
    dplyr::mutate(has_bwar = mlbam_id %in% war$mlbam_id) |> dplyr::group_by(year) |>
    dplyr::summarise(debuted = dplyr::n(), with_bwar_rows = sum(has_bwar), .groups = "drop")

  out$rankings <- rankings |> dplyr::group_by(list, list_year) |> dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = list, values_from = n) |>
    dplyr::left_join(p |> dplyr::filter(!is.na(pipeline_draft_rank)) |> dplyr::group_by(list_year = year) |>
      dplyr::summarise(draft_ranked_and_drafted = dplyr::n(), ranked_signed = sum(signed %in% TRUE), .groups = "drop"), by = "list_year")

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (n in names(out)) data.table::fwrite(out[[n]], file.path(out_dir, paste0(n, ".csv")))
  out
}
