# Stage 5 checkpoint (plus the Stage 4 value refit on 2012-21 classes)
checkpoint_stage5 <- function(out, out_dir = "data/derived/checkpoint5") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (n in names(out)) if (is.data.frame(out[[n]])) data.table::fwrite(out[[n]], file.path(out_dir, paste0(n, ".csv")))
  names(out)
}

refit_compare <- function(g_old, g_new) {
  k <- c("type", "bonus", "age_c")
  a <- g_old |> dplyr::filter(age_c == 0) |> dplyr::select(dplyr::all_of(k), p_arr, e_surplus_old = e_surplus, lo_old = e_surplus_lo, hi_old = e_surplus_hi,
                                                     dS_dB_old = dS_dB, p_gt10_old = p_gt10)
  b <- g_new |> dplyr::filter(age_c == 0) |> dplyr::select(dplyr::all_of(k), e_surplus_new = e_surplus, lo_new = e_surplus_lo, hi_new = e_surplus_hi,
                                                     dS_dB_new = dS_dB, p_gt10_new = p_gt10, p_arr_new = p_arr)
  dplyr::left_join(a, b, by = k) |> dplyr::mutate(ratio_new_old = e_surplus_new / e_surplus_old)
}

era_effects <- function(fit) {
  zy <- dmat(fit$draws, "zy") * fit$draws$sy
  tibble::tibble(class = 2011 + seq_len(ncol(zy)), mean = colMeans(zy), lo = apply(zy, 2, stats::quantile, 0.05), hi = apply(zy, 2, stats::quantile, 0.95)) |>
    dplyr::filter(class <= 2021) |>
    dplyr::mutate(era = ifelse(class <= 2016, "2012-16", "2017-21"))
}

# New-draft-year check for the multiply-imputed value fit: predicted tier shares (draft-year RE integrated, own org RE)
# vs completed tiers, by era. For 2017-21 the "observed" tiers pool the M imputations.
value_new_year_check_mi <- function(fit_ord, md, basis, imp, n_draws = 1000L) {
  a <- md[md$event & md$year <= 2021, ]
  im <- imp$imputations
  rows <- dplyr::bind_rows(lapply(sort(unique(im$m)), function(m) {
    v <- ifelse(a$mlbam_id %in% imp$open_ids, im$v_imp[im$m == m][match(a$mlbam_id, im$mlbam_id[im$m == m])], a$v_war_control_obs)
    dplyr::mutate(a, v = v, m = m)
  }))
  ii <- round(seq(1, nrow(fit_ord$draws), length.out = n_draws)); dr <- fit_ord$draws[ii, ]
  out <- list()
  for (era in c("2012-16", "2017-21")) {
    r <- rows[if (era == "2012-16") rows$year <= 2016 & rows$m == 1 else rows$year >= 2017, ]
    B <- spline_basis(r$x, knots = basis$knots, boundary = basis$boundary)$B
    ol <- ordinal_lp(dr, B, r$group, r$school_idx, r$age_c, td = r$t_event)
    ol$eta <- ol$eta + dmat(dr, "zo")[, r$org_idx] * dr$so; ol$sd <- dr$sy
    tp <- ordinal_tier_probs(ol); pred <- sapply(1:5, function(k) rowMeans(tp[, , k]))
    out[[era]] <- tibble::tibble(era = era, tier = c("<=0", "(0,2]", "(2,5]", "(5,10]", ">10"),
      obs_share = as.numeric(table(factor(v_tier(r$v), 1:5))) / nrow(r), pred_share = colMeans(pred),
      pred_lo = apply(pred, 2, stats::quantile, 0.05), pred_hi = apply(pred, 2, stats::quantile, 0.95), n_players = nrow(r) / ifelse(era == "2017-21", length(unique(im$m)), 1))
  }
  dplyr::bind_rows(out)
}
