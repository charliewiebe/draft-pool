# Stage 4 checks: posterior predictive by bonus decile, out-of-time calibration, LOO, organization effects,
# with/without recent classes.

ord_probs <- function(eta, cut) {               # eta: D x R; cut: D x 4 -> D x R x 5
  D <- nrow(eta); R <- ncol(eta); pr <- array(0, c(D, R, 5)); prev <- 0
  for (k in 1:4) { cur <- plogis(cut[, k] - eta); pr[, , k] <- cur - prev; prev <- cur }
  pr[, , 5] <- 1 - prev; pr
}

value_rows <- function(md) {
  a <- md[md$event, ]; a$cens <- !(a$v_war_resolved %in% TRUE); a$v <- ifelse(a$cens, a$v_war_control_obs, a$v_war); a
}

# per-player hurdle components including the player's own draft-year and org effects
hurdle_player <- function(dr, rows, basis) {
  B <- spline_basis(rows$x, knots = basis$knots, boundary = basis$boundary)$B
  hl <- hurdle_lp(dr, B, rows$group, rows$school_idx, rows$age_c, td = rows$t_event)
  hl$eta_p <- hl$eta_p + dmat(dr, "zy_p")[, rows$year_idx] * dr$sy_p + dmat(dr, "zo_p")[, rows$org_idx] * dr$so_p
  hl$mu <- hl$mu + dmat(dr, "zy_m")[, rows$year_idx] * dr$sy_m + dmat(dr, "zo_m")[, rows$org_idx] * dr$so_m
  hl
}

ppc_value <- function(fit_h, fit_o, md, basis, n_draws = 1000L, seed = 5L) {
  set.seed(seed)
  a <- value_rows(md); a <- a[!a$cens, ]
  dec <- dplyr::ntile(a$x, 10); ii <- seq_len(min(n_draws, nrow(fit_h$draws)))
  hl <- hurdle_player(fit_h$draws[ii, ], a, basis); D <- length(ii); R <- nrow(a)
  pos <- matrix(stats::runif(D * R) < plogis(hl$eta_p), D, R)
  vsim <- ifelse(pos, stats::rlnorm(D * R, hl$mu, hl$sigma), -stats::rexp(D * R, hl$lambda_neg))
  B <- spline_basis(a$x, knots = basis$knots, boundary = basis$boundary)$B
  do <- fit_o$draws[ii, ]; G <- 6L; K <- ncol(B)
  eta <- dmat(do, "c") %*% t(B); dd <- darr(do, "d", G, K)
  for (gg in unique(a$group)) { j <- which(a$group == gg); eta[, j] <- eta[, j] + dd[, gg, ] %*% t(B[j, , drop = FALSE]) }
  eta <- eta + dmat(do, "ba")[, a$school_idx] * matrix(a$age_c, D, R, byrow = TRUE) + td_term(do, "b_td", "b_td2", a$school_idx, a$t_event) +
    (if ("a_g[1]" %in% names(do)) dmat(do, "a_g")[, a$group] else 0) +
    dmat(do, "zy")[, a$year_idx] * do$sy + dmat(do, "zo")[, a$org_idx] * do$so
  pr <- ord_probs_role(eta, dmat(do, "cut"), a$group %% 2 == 1, dsc(do, "dtop_p"))
  cpr <- pr; for (k in 2:5) cpr[, , k] <- cpr[, , k - 1] + pr[, , k]
  u <- matrix(stats::runif(D * R), D, R); tsim <- 1L + (u > cpr[, , 1]) + (u > cpr[, , 2]) + (u > cpr[, , 3]) + (u > cpr[, , 4])
  stat <- function(grp) dplyr::bind_rows(lapply(sort(unique(grp)), function(gv) {
    j <- which(grp == gv); obs <- a$v[j]
    q <- function(m) stats::quantile(m, c(0.05, 0.95))
    tibble::tibble(group = gv, n = length(j), bonus_rel_median = stats::median(exp(a$x[j])),
      obs_p_gt5 = mean(obs > 5), hurdle_p_gt5_lo = q(rowMeans(vsim[, j, drop = FALSE] > 5))[1], hurdle_p_gt5_hi = q(rowMeans(vsim[, j, drop = FALSE] > 5))[2],
      obs_p_gt10 = mean(obs > 10), hurdle_p_gt10_lo = q(rowMeans(vsim[, j, drop = FALSE] > 10))[1], hurdle_p_gt10_hi = q(rowMeans(vsim[, j, drop = FALSE] > 10))[2],
      ord_p_gt10_lo = q(rowMeans(tsim[, j, drop = FALSE] == 5))[1], ord_p_gt10_hi = q(rowMeans(tsim[, j, drop = FALSE] == 5))[2],
      obs_mean = mean(obs), hurdle_mean_lo = q(rowMeans(vsim[, j, drop = FALSE]))[1], hurdle_mean_hi = q(rowMeans(vsim[, j, drop = FALSE]))[2],
      obs_p_le0 = mean(obs <= 0), hurdle_p_le0_lo = q(rowMeans(vsim[, j, drop = FALSE] <= 0))[1], hurdle_p_le0_hi = q(rowMeans(vsim[, j, drop = FALSE] <= 0))[2])
  }))
  list(by_decile = stat(dec), by_type = stat(as.character(a$type)))
}

arrival_player <- function(dr, rows, basis, integrate_year = FALSE) {
  B <- spline_basis(rows$x, knots = basis$knots, boundary = basis$boundary)$B
  lp <- arrival_lp(dr, B, rows$group, rows$school_idx, rows$age_c, rows$x)
  orgfx <- dmat(dr, "z_org")[, rows$org_idx] * dr$sigma_org
  if (!integrate_year) {
    lp$eta <- lp$eta + orgfx + dmat(dr, "z_yr")[, rows$year_idx] * dr$sigma_yr
    p <- plogis(lp$eta); surv <- 1; pdeb <- 0
    pt <- array(0, c(nrow(p), ncol(p), ncol(lp$alpha)))
    for (t in seq_len(ncol(lp$alpha))) { h <- plogis(lp$alpha[, t] + lp$lat); pt[, , t] <- p * surv * h; surv <- surv * (1 - h) }
    return(list(p_arr = p, p_t = pt))
  }
  gh <- gh_nodes(10); p <- 0; pt <- 0
  for (a in seq_along(gh$z)) {
    pa <- plogis(lp$eta + orgfx + dr$sigma_yr * gh$z[a]); surv <- 1; ptt <- array(0, c(nrow(pa), ncol(pa), ncol(lp$alpha)))
    for (t in seq_len(ncol(lp$alpha))) { h <- plogis(lp$alpha[, t] + lp$lat); ptt[, , t] <- pa * surv * h; surv <- surv * (1 - h) }
    p <- p + gh$w[a] * pa; pt <- pt + gh$w[a] * ptt
  }
  list(p_arr = p, p_t = pt)
}

calib_table <- function(pred, obs, grp) dplyr::bind_rows(lapply(sort(unique(grp)), function(gv) {
  j <- which(grp == gv)
  tibble::tibble(group = gv, n = length(j), pred_mean = mean(colMeans(pred[, j, drop = FALSE])),
                 pred_lo = stats::quantile(rowMeans(pred[, j, drop = FALSE]), 0.05), pred_hi = stats::quantile(rowMeans(pred[, j, drop = FALSE]), 0.95),
                 obs = mean(obs[j]))
}))

# In-sample arrival PPC (mature fit) on resolved players; out-of-time: fit 2012-15 -> classes 2016-17, debut by 2026
arrival_checks <- function(fit_mature, fit_oot, md, basis, n_draws = 1000L, last_season = 2026L) {
  ii <- seq_len(min(n_draws, nrow(fit_mature$draws)))
  r1 <- md[md$year <= 2017 & (md$event | md$resolved_non), ]
  ap <- arrival_player(fit_mature$draws[ii, ], r1, basis)
  ppc <- list(by_decile = calib_table(ap$p_arr, r1$event, dplyr::ntile(r1$x, 10)),
              by_type = calib_table(ap$p_arr, r1$event, as.character(r1$type)))
  r2 <- md[md$year %in% 2016:2017, ]
  jj <- seq_len(min(n_draws, nrow(fit_oot$draws)))
  ao <- arrival_player(fit_oot$draws[jj, ], r2, basis, integrate_year = TRUE)
  tmax <- pmin(last_season - r2$year, dim(ao$p_t)[3] - 1)
  by26 <- matrix(0, length(jj), nrow(r2))
  for (i in seq_len(nrow(r2))) by26[, i] <- rowSums(ao$p_t[, i, seq_len(tmax[i] + 1), drop = FALSE])
  pbar <- colMeans(by26)
  oot <- list(
    by_pred_decile = calib_table(by26, r2$event, dplyr::ntile(pbar, 10)),
    by_bonus_decile = calib_table(by26, r2$event, dplyr::ntile(r2$x, 10)),
    by_type = calib_table(by26, r2$event, as.character(r2$type)),
    scores = tibble::tibble(n = nrow(r2), brier = mean((pbar - r2$event)^2), brier_base = mean((mean(r2$event) - r2$event)^2),
                            log_score = mean(r2$event * log(pbar) + (1 - r2$event) * log(1 - pbar)),
                            pred_rate = mean(pbar), obs_rate = mean(r2$event)))
  list(ppc = ppc, oot = oot)
}

value_oot <- function(fit_oot, md, basis, n_draws = 1000L) {
  a <- value_rows(md); a <- a[a$year %in% (value_oot_train_max() + 1):value_max_year() & !a$cens, ]
  ii <- seq_len(min(n_draws, nrow(fit_oot$draws))); dr <- fit_oot$draws[ii, ]
  B <- spline_basis(a$x, knots = basis$knots, boundary = basis$boundary)$B
  hl <- hurdle_lp(dr, B, a$group, a$school_idx, a$age_c, td = a$t_event)
  hl$eta_p <- hl$eta_p + dmat(dr, "zo_p")[, a$org_idx] * dr$so_p; hl$mu <- hl$mu + dmat(dr, "zo_m")[, a$org_idx] * dr$so_m
  hl$sd_p <- dr$sy_p; hl$sd_m <- dr$sy_m                     # new draft year: integrate year RE only
  vg <- seq(-4, 40, by = 0.25); hm <- hurdle_mass(hl, vg)
  tiers <- v_tier(vg)
  pred <- sapply(1:5, function(k) apply(hm[, , tiers == k, drop = FALSE], 1, sum) / nrow(a))
  tibble::tibble(tier = c("<=0", "(0,2]", "(2,5]", "(5,10]", ">10"), obs_share = as.numeric(table(factor(v_tier(a$v), 1:5))) / nrow(a),
                 pred_share = colMeans(pred), pred_lo = apply(pred, 2, stats::quantile, 0.05), pred_hi = apply(pred, 2, stats::quantile, 0.95),
                 n = nrow(a))
}

loo_table <- function(fit_hurdle, fit_ordinal, fit_mature) {
  cmp <- loo::loo_compare(list(hurdle = fit_hurdle$loo_log_lik_tier, ordinal = fit_ordinal$loo_log_lik_tier))
  k <- function(l, nm) tibble::tibble(model = nm, elpd = l$estimates["elpd_loo", 1], se = l$estimates["elpd_loo", 2],
                                      p_loo = l$estimates["p_loo", 1], k_gt_0.7 = sum(loo::pareto_k_values(l) > 0.7),
                                      k_0.5_0.7 = sum(loo::pareto_k_values(l) > 0.5 & loo::pareto_k_values(l) <= 0.7), n = length(loo::pareto_k_values(l)))
  list(compare_tier = tibble::as_tibble(cmp, rownames = "model"),
       models = dplyr::bind_rows(k(fit_hurdle$loo_log_lik_tier, "hurdle (tier scale)"), k(fit_ordinal$loo_log_lik_tier, "ordinal (tier scale)"),
                                 k(fit_hurdle$loo_log_lik, "hurdle (continuous)"), k(fit_mature$loo_log_lik, "arrival 2012-17 (no u)")))
}

diagnostics_table <- function(...) {
  fits <- list(...)
  dplyr::bind_rows(lapply(names(fits), function(n) { d <- fits[[n]]$diagnostics
    tibble::tibble(fit = n, draws = d$draws, minutes = round(d$minutes, 1), divergent = d$divergent, max_treedepth = d$max_treedepth,
                   min_ebfmi = round(min(d$ebfmi), 3), max_rhat = round(d$max_rhat, 4), min_ess_bulk = round(d$min_ess_bulk), min_ess_tail = round(d$min_ess_tail)) }))
}

org_effects <- function(fit_arr, fit_val, md, teams) {
  lv <- sort(unique(md$team_id))
  a <- dmat(fit_arr$draws, "z_org") * fit_arr$draws$sigma_org
  m <- dmat(fit_val$draws, "zo") * fit_val$draws$so
  q <- function(x, p) apply(x, 2, stats::quantile, p)
  tibble::tibble(org_idx = seq_along(lv), team_id = lv, team = teams$team_name[match(lv, teams$team_id)],
    arrival_logit = colMeans(a), arrival_lo = q(a, .05), arrival_hi = q(a, .95),
    value_tier_logit = colMeans(m), value_lo = q(m, .05), value_hi = q(m, .95), p_arrival_gt0 = colMeans(a > 0)) |>
    dplyr::arrange(dplyr::desc(arrival_logit))
}

recent_class_effect <- function(fit_mature, fit_joint, basis, sched_2025) {
  gc <- grid_cells(basis, sched_2025, bonus_grid = exp(seq(log(2e4), log(8e6), length.out = 12)), age_c = 0)
  f <- function(fit, lab) {
    ii <- seq_len(min(1000, nrow(fit$draws))); dr <- fit$draws[ii, ]
    ap <- predict_arrival(dr, gc$B, gc$cells$g, gc$cells$s, gc$cells$age_c, gc$cells$x)
    tibble::tibble(fit = lab, type = gc$cells$type, bonus = gc$cells$bonus, p_arr = colMeans(ap$p_arr),
                   p_lo = apply(ap$p_arr, 2, stats::quantile, .05), p_hi = apply(ap$p_arr, 2, stats::quantile, .95))
  }
  dplyr::bind_rows(f(fit_mature, "2012-17 only, no intermediate outcomes"), f(fit_joint, "2012-25 joint with intermediate outcomes"))
}

imputation_sensitivity <- function(fit_m1, fit_m2, basis, sched_2025) {
  r <- recent_class_effect(fit_m1, fit_m2, basis, sched_2025)
  r$fit <- ifelse(grepl("^2012-17", r$fit), "imputation m=1", "imputation m=2")
  r
}

value_oot_ordinal <- function(fit_oot, md, basis, n_draws = 1000L) {
  a <- value_rows(md); a <- a[a$year %in% (value_oot_train_max() + 1):value_max_year() & !a$cens, ]
  ii <- seq_len(min(n_draws, nrow(fit_oot$draws))); dr <- fit_oot$draws[ii, ]
  B <- spline_basis(a$x, knots = basis$knots, boundary = basis$boundary)$B
  ol <- ordinal_lp(dr, B, a$group, a$school_idx, a$age_c, td = a$t_event)
  ol$eta <- ol$eta + dmat(dr, "zo")[, a$org_idx] * dr$so; ol$sd <- dr$sy       # new draft year: integrate year RE
  tp <- ordinal_tier_probs(ol)
  pred <- sapply(1:5, function(k) rowMeans(tp[, , k]))
  tibble::tibble(tier = c("<=0", "(0,2]", "(2,5]", "(5,10]", ">10"), obs_share = as.numeric(table(factor(v_tier(a$v), 1:5))) / nrow(a),
                 pred_share = colMeans(pred), pred_lo = apply(pred, 2, stats::quantile, 0.05), pred_hi = apply(pred, 2, stats::quantile, 0.95),
                 n = nrow(a))
}

# within-tier V by bonus: does the donor (within-tier) distribution depend on bonus? (assumption check)
within_tier_check <- function(md) {
  a <- value_rows(md); a <- a[!a$cens, ]; a$tier <- v_tier(a$v); a$bonus_half <- ifelse(a$x > stats::median(a$x), "upper half", "lower half")
  a |> dplyr::group_by(tier, bonus_half) |> dplyr::summarise(n = dplyr::n(), mean_v = mean(v), median_v = stats::median(v), .groups = "drop")
}

# Grid-consistency check: resolved arrivals in the value-model classes, own covariates/debut timing/org, draft-year RE integrated as for a
# new class (exactly how the decision grid predicts). Catches year effects that absorb selection rather than class strength.
value_new_year_check <- function(fit_ord, md, basis, n_draws = 1000L) {
  a <- value_rows(md); a <- a[a$year <= value_max_year() & !a$cens, ]
  ii <- seq_len(min(n_draws, nrow(fit_ord$draws))); dr <- fit_ord$draws[ii, ]
  B <- spline_basis(a$x, knots = basis$knots, boundary = basis$boundary)$B
  ol <- ordinal_lp(dr, B, a$group, a$school_idx, a$age_c, td = a$t_event)
  ol$eta <- ol$eta + dmat(dr, "zo")[, a$org_idx] * dr$so; ol$sd <- dr$sy
  tp <- ordinal_tier_probs(ol); pred <- sapply(1:5, function(k) rowMeans(tp[, , k]))
  tibble::tibble(tier = c("<=0", "(0,2]", "(2,5]", "(5,10]", ">10"), obs_share = as.numeric(table(factor(v_tier(a$v), 1:5))) / nrow(a),
                 pred_share = colMeans(pred), pred_lo = apply(pred, 2, stats::quantile, 0.05), pred_hi = apply(pred, 2, stats::quantile, 0.95),
                 n = nrow(a), sy = mean(dr$sy))
}
