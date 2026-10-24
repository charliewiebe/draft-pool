team_table <- function(p) p |> dplyr::arrange(dplyr::desc(year)) |> dplyr::distinct(team_id, .keep_all = TRUE) |> dplyr::select(team_id, team_name)

org_conditional_grid <- function(fit_arr, fit_ord, orgs, basis, sched_2025, tv, pct = 0.9) {
  o <- orgs[order(orgs$arrival_logit), ]; pick <- o[ceiling(pct * nrow(o)), ]
  gc <- grid_cells(basis, sched_2025, age_c = 0)
  oa <- dmat(fit_arr$draws, "z_org")[, pick$org_idx] * fit_arr$draws$sigma_org
  ov <- dmat(fit_ord$draws, "zo")[, pick$org_idx] * fit_ord$draws$so
  g <- compute_grid_ord(fit_arr, fit_ord, gc, tv, org_fixed_arr = oa, org_fixed_val = ov)
  s <- summarise_grid(g); s$org <- pick$team; s$org_percentile <- pct
  s
}

variant_summaries <- function(gr, vp = valuation_params(), max_draw = 1000L) {
  keep <- gr$cells$cell[gr$cells$age_c == 0]
  gr$expect <- gr$expect[gr$expect$cell %in% keep & gr$expect$draw <= max_draw, ]
  combos <- expand.grid(variant = vp$variants, r = vp$r, dpw = vp$dpw, stringsAsFactors = FALSE)
  dplyr::bind_rows(lapply(seq_len(nrow(combos)), function(i) {
    s <- summarise_grid(gr, variant = combos$variant[i], r = combos$r[i], dpw = combos$dpw[i])
    s |> dplyr::filter(age_c == 0) |> dplyr::select(type, bonus, ev_dollar, e_surplus, e_surplus_lo, e_surplus_hi, return, dS_dB, p_convex) |>
      dplyr::mutate(variant = combos$variant[i], r = combos$r[i], dpw = combos$dpw[i])
  }))
}

write_stage4_parquet <- function(gr, gs, vs, dir = "data/derived/stage4") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- c(file.path(dir, "grid_expectations.parquet"), file.path(dir, "grid_predictive.parquet"), file.path(dir, "grid_cells.parquet"),
         file.path(dir, "grid_summary_default.parquet"), file.path(dir, "grid_summary_variants.parquet"))
  arrow::write_parquet(gr$expect, f[1]); arrow::write_parquet(gr$predictive, f[2]); arrow::write_parquet(gr$cells, f[3])
  arrow::write_parquet(gs, f[4]); arrow::write_parquet(vs, f[5])
  f
}

checkpoint_stage4 <- function(diag, prior_check, loo, ppc_v, arr_chk, v_oot, v_oot_ord, orgs, recent, gs, org_grid, vs, imp_sens, wt, tv, newyear,
                              out_dir = "data/derived/checkpoint4") {
  out <- list(diagnostics = diag, prior_check = prior_check, loo_models = loo$models, loo_compare_tier = loo$compare_tier,
    ppc_value_by_decile = ppc_v$by_decile, ppc_value_by_type = ppc_v$by_type,
    ppc_arrival_by_decile = arr_chk$ppc$by_decile, ppc_arrival_by_type = arr_chk$ppc$by_type,
    oot_arrival_scores = arr_chk$oot$scores, oot_arrival_by_pred_decile = arr_chk$oot$by_pred_decile,
    oot_arrival_by_bonus_decile = arr_chk$oot$by_bonus_decile, oot_arrival_by_type = arr_chk$oot$by_type, oot_value_tiers_hurdle = v_oot, oot_value_tiers_ordinal = v_oot_ord, imputation_sensitivity = imp_sens,
    within_tier_check = wt, value_new_year_check = newyear, tier_donors = tibble::tibble(tier = 1:5, n = tv$tier_n, mean_v = as.numeric(tv$tier_mean_v),
      mean_pv_per_dollar_war_t4 = tv$tier_E1[, 5, "a", "0.08"]),
    org_effects = orgs, recent_class_effect = recent,
    curves_default = gs |> dplyr::filter(age_c == 0),
    org_conditional = org_grid |> dplyr::select(type, bonus, org, org_percentile, p_arr, e_surplus, e_surplus_lo, e_surplus_hi) |>
      dplyr::left_join(gs |> dplyr::filter(age_c == 0) |> dplyr::select(type, bonus, p_arr_avg = p_arr, e_surplus_avg = e_surplus), by = c("type", "bonus")),
    surplus_by_variant = vs |> dplyr::filter(bonus %in% unique(vs$bonus)[c(6, 18, 30, 42)]))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (n in names(out)) data.table::fwrite(out[[n]], file.path(out_dir, paste0(n, ".csv")))
  out
}
