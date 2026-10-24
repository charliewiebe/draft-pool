checkpoint_stage3 <- function(p, yi, imp, udfa, o, prog, out_dir = "data/derived/checkpoint3") {
  out <- list()
  out$year_index <- yi |> dplyr::mutate(dplyr::across(c(sched_index, cpi_to_base, sched_to_2025), ~ round(.x, 4)))

  # normalization breaks: slot at fixed positions divided by each index, year-over-year change
  s <- p |> dplyr::filter(pick_overall %in% c(3, 50, 100, 150), !is.na(slot)) |> dplyr::distinct(year, pick_overall, slot, slot1, league_pool, sched_index)
  out$normalization_breaks <- s |> dplyr::arrange(pick_overall, year) |> dplyr::group_by(pick_overall) |>
    dplyr::mutate(yoy_p1 = round((slot / slot1) / dplyr::lag(slot / slot1) - 1, 3),
                  yoy_pool = round((slot / league_pool) / dplyr::lag(slot / league_pool) - 1, 3),
                  yoy_sched = round((slot / sched_index) / dplyr::lag(slot / sched_index) - 1, 3)) |>
    dplyr::ungroup() |> dplyr::filter(year %in% c(2013, 2016, 2017, 2018, 2020, 2021, 2023)) |>
    dplyr::select(pick_overall, year, yoy_p1, yoy_pool, yoy_sched)

  sg <- dplyr::filter(p, signed %in% TRUE, !is.na(bonus))
  base <- sg$log_bonus_rel_o1000
  out$offset_sensitivity <- dplyr::bind_rows(lapply(c(500, 1000, 5000, 10000), function(o) {
    v <- sg[[paste0("log_bonus_rel_o", o)]]
    tibble::tibble(offset_usd = o, n_bonus_below_offset = sum(sg$bonus < o), share_below = round(mean(sg$bonus < o), 4),
                   sd_log = round(stats::sd(v), 3), p01_log = round(stats::quantile(v, 0.01), 3), median_log = round(stats::median(v), 3))
  }))

  out$censoring <- p |> dplyr::mutate(era = ifelse(year <= 2016, "2012-16", ifelse(year <= 2019, "2017-19", ifelse(year == 2020, "2020", "2021-25"))),
                                      grp = ifelse(pool_round, "R1-10", "R11+")) |>
    dplyr::count(era, grp, bonus_cens) |> tidyr::pivot_wider(names_from = bonus_cens, values_from = n, values_fill = 0)
  out$senior_nominal <- sg |> dplyr::group_by(grp = ifelse(pool_round, "R1-10", "R11+")) |>
    dplyr::summarise(signed_with_bonus = dplyr::n(), bonus_le_10k = sum(bonus <= 10000), bonus_le_1k = sum(bonus <= 1000), .groups = "drop")

  out$imp_reporting_by_year <- imp$reporting |> dplyr::mutate(dplyr::across(c(fbar, expected_n, pi_raw, pi), ~ round(.x, 3)))
  out$imp_reporting_2017_check <- imp$reporting_2017_check |> dplyr::mutate(dplyr::across(c(fbar, expected_n, pi_raw, pi), ~ round(.x, 3)))
  out$imp_validation_2017 <- imp$validation_2017 |> dplyr::mutate(dplyr::across(-c(set, n), ~ round(.x, 3)))
  im <- imp$imputations |> dplyr::left_join(dplyr::distinct(p, year, pick_overall, r11_threshold, pool_round), by = c("year", "pick_overall"))
  out$imp_summary <- im |> dplyr::group_by(method) |> dplyr::summarise(picks = dplyr::n_distinct(paste(year, pick_overall)), draws = dplyr::n(),
      median = stats::median(bonus_imp), share_at_thr = round(mean(!pool_round & bonus_imp == r11_threshold), 3),
      share_below_thr = round(mean(!pool_round & bonus_imp < r11_threshold), 3), share_le_10k = round(mean(bonus_imp <= 10000), 3), .groups = "drop")
  out$imp_vs_observed_2012_16 <- dplyr::bind_rows(
    sg |> dplyr::filter(year <= 2016, !pool_round) |> dplyr::summarise(set = "observed (reported) 2012-16", n = dplyr::n(),
      share_le_10k = mean(bonus <= 10000), share_at_thr = mean(bonus == r11_threshold), share_gt_thr = mean(bonus > r11_threshold)),
    im |> dplyr::filter(year <= 2016, !pool_round) |> dplyr::summarise(set = "imputed (missing) 2012-16", n = dplyr::n(),
      share_le_10k = mean(bonus_imp <= 10000), share_at_thr = mean(bonus_imp == r11_threshold), share_gt_thr = mean(bonus_imp > r11_threshold)),
    sg |> dplyr::filter(year %in% 2017:2019, !pool_round) |> dplyr::summarise(set = "observed 2017-19 (near-complete)", n = dplyr::n(),
      share_le_10k = mean(bonus <= 10000), share_at_thr = mean(bonus == r11_threshold), share_gt_thr = mean(bonus > r11_threshold))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("share"), ~ round(.x, 3)))

  u <- udfa$candidates
  out$udfa_2020 <- tibble::tibble(step = c("SFA transactions 2020-06-14..09-30 (distinct players)", "no affiliated pro season before 2021",
      "not signed out of an earlier Rule 4 draft", "age 17-25 on 2020-06-14", "minor league contract (of final)"),
    n = c(nrow(u), sum(!u$prior_pro), sum(!u$prior_pro & !u$signed_rule4_before), sum(u$udfa_2020), sum(u$udfa_2020 & u$minor_league_contract)))
  out$pv_2020 <- p |> dplyr::filter(year == 2020, signed %in% TRUE, !is.na(bonus)) |>
    dplyr::summarise(n = dplyr::n(), pv_ratio_r06 = mean(pv_2020_bonus(bonus, 0.06) / bonus), pv_ratio_r08 = mean(pv_2020_bonus(bonus, 0.08) / bonus),
                     pv_ratio_r10 = mean(pv_2020_bonus(bonus, 0.10) / bonus)) |> dplyr::mutate(dplyr::across(-n, ~ round(.x, 4)))

  out$signing_status <- p |> dplyr::count(signing_status, pool = pool_round) |> tidyr::pivot_wider(names_from = pool, values_from = n, names_prefix = "pool_round_", values_fill = 0)
  out$region <- p |> dplyr::count(region) |> dplyr::mutate(share = round(n / sum(n), 3))

  out$outcomes_by_class <- o |> dplyr::group_by(year) |> dplyr::summarise(signed = dplyr::n(),
      arrived = sum(arrival_status == "arrived"), never = sum(arrival_status == "never_arrived"), arr_censored = sum(arrival_status == "censored"),
      ctrl_complete = sum(control_status == "complete"), ctrl_exited = sum(control_status == "exited_before_6"),
      ctrl_censored = sum(control_status == "censored"), v_war_resolved = round(mean(v_war_resolved), 3),
      mean_v_war_resolved = round(mean(v_war, na.rm = TRUE), 3), share_vwar_gt5 = round(mean(v_war > 5, na.rm = TRUE), 3),
      ever_top100 = sum(ever_top100), .groups = "drop")
  out$seasons_to_debut <- o |> dplyr::filter(debuted) |> dplyr::count(school_type, seasons_to_debut) |>
    tidyr::pivot_wider(names_from = school_type, values_from = n, values_fill = 0) |> dplyr::arrange(seasons_to_debut)
  out$control_approx_check <- o |> dplyr::filter(mlb_seasons >= 6) |>
    dplyr::summarise(n = dplyr::n(), median_calendar_span = stats::median(control_last_season - debut_season + 1),
                     share_span_gt6 = round(mean(control_last_season - debut_season + 1 > 6), 3))
  out$progress_rows <- tibble::tibble(rows = nrow(prog), players = dplyr::n_distinct(prog$mlbam_id),
    rows_with_milb_level = sum(!is.na(prog$milb_top_level)), rows_top100 = sum(!is.na(prog$top100_rank)))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (n in names(out)) data.table::fwrite(out[[n]], file.path(out_dir, paste0(n, ".csv")))
  out
}
