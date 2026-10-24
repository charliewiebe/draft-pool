# Prior predictive checks by direct simulation from the Stan priors (exact; no MCMC funnel issues).

hn <- function(n, s) abs(stats::rnorm(n, 0, s))
rw_sim <- function(n, K, first_mean, first_sd, step_sd) {
  z <- matrix(stats::rnorm(n * K), n, K); out <- z
  out[, 1] <- first_mean + first_sd * z[, 1]
  for (k in 2:K) out[, k] <- out[, k - 1] + step_sd * z[, k]
  out
}

prior_draws_arrival <- function(n, K, G = 6L, S = 3L, Tn = 15L) {
  dr <- data.frame(sigma_rw = hn(n, 0.5), tau_d = hn(n, 0.5), sigma_yr = hn(n, 0.5), sigma_org = hn(n, 0.5),
                   sigma_alpha = hn(n, 1), b_lat_x = stats::rnorm(n), lambda_inc = 0, lambda_lat = 0)
  c <- rw_sim(n, K, -2, 2, dr$sigma_rw); al <- rw_sim(n, Tn, -3, 2, dr$sigma_alpha)
  for (k in 1:K) dr[[sprintf("c[%d]", k)]] <- c[, k]
  for (t in 1:Tn) dr[[sprintf("alpha[%d]", t)]] <- al[, t]
  for (gg in 1:G) { d <- rw_sim(n, K, 0, 1, 1) * dr$tau_d; for (k in 1:K) dr[[sprintf("d[%d,%d]", gg, k)]] <- d[, k] }
  for (j in 1:S) { dr[[sprintf("beta_age[%d]", j)]] <- stats::rnorm(n, 0, 0.5); dr[[sprintf("b_lat_age[%d]", j)]] <- stats::rnorm(n, 0, 0.5) }
  for (gg in 1:G) dr[[sprintf("b_lat_g[%d]", gg)]] <- stats::rnorm(n)
  dr
}

prior_draws_hurdle <- function(n, K, G = 6L, S = 3L) {
  dr <- data.frame(sig_rw_p = hn(n, 0.5), sig_rw_m = hn(n, 0.5), tau_p = hn(n, 0.3), tau_m = hn(n, 0.3),
                   sy_p = hn(n, 0.3), sy_m = hn(n, 0.3), so_p = hn(n, 0.3), so_m = hn(n, 0.3),
                   lambda_neg = abs(stats::rnorm(n, 2, 1)))
  cp <- rw_sim(n, K, 0, 1.5, dr$sig_rw_p); cm <- rw_sim(n, K, 0.5, 1.5, dr$sig_rw_m)
  for (k in 1:K) { dr[[sprintf("cp[%d]", k)]] <- cp[, k]; dr[[sprintf("cm[%d]", k)]] <- cm[, k] }
  for (gg in 1:G) {
    dp <- rw_sim(n, K, 0, 1, 1) * dr$tau_p; dm <- rw_sim(n, K, 0, 1, 1) * dr$tau_m
    for (k in 1:K) { dr[[sprintf("dp[%d,%d]", gg, k)]] <- dp[, k]; dr[[sprintf("dm[%d,%d]", gg, k)]] <- dm[, k] }
    dr[[sprintf("sigma[%d]", gg)]] <- abs(stats::rnorm(n, 1, 0.5))
  }
  for (j in 1:S) { dr[[sprintf("ba_p[%d]", j)]] <- stats::rnorm(n, 0, 0.5); dr[[sprintf("ba_m[%d]", j)]] <- stats::rnorm(n, 0, 0.5) }
  dr
}

# Implied distributions on a small grid (4yr-H and HS-P at three bonus levels), summarized over prior draws
prior_predictive_check <- function(basis, sched_2025, n = 2000L, seed = 11L) {
  set.seed(seed)
  gc <- grid_cells(basis, sched_2025, bonus_grid = c(2e4, 2e5, 2e6, 8e6), age_c = 0)
  keep <- gc$cells$type %in% c("4yr-H", "HS-P"); cl <- gc$cells[keep, ]; B <- gc$B[keep, , drop = FALSE]
  pa <- prior_draws_arrival(n, ncol(B)); ph <- prior_draws_hurdle(n, ncol(B))
  lp <- arrival_lp(pa, B, cl$g, cl$s, cl$age_c, cl$x); ap <- arrival_predict(pa, lp)
  tdeb <- apply(ap$p_t, c(1, 2), function(pr) sum((seq_along(pr) - 1) * pr) / max(sum(pr), 1e-12))
  hl <- hurdle_lp(ph, B, cl$g, cl$s, cl$age_c); tp <- tail_probs(hl)
  pp <- plogis(hl$eta_p); ev_pos <- exp(hl$mu + hl$sigma^2 / 2)
  q <- function(m) t(apply(m, 2, stats::quantile, c(0.05, 0.25, 0.5, 0.75, 0.95)))
  pa_q <- q(ap$p_arr); colnames(pa_q) <- paste0("p_arr_q", c(5, 25, 50, 75, 95))
  td_q <- q(tdeb); colnames(td_q) <- paste0("mean_t_debut_q", c(5, 25, 50, 75, 95))
  v10 <- q(tp$p_gt10); colnames(v10) <- paste0("p_v_gt10_given_arr_q", c(5, 25, 50, 75, 95))
  pos <- q(pp); colnames(pos) <- paste0("p_v_pos_given_arr_q", c(5, 25, 50, 75, 95))
  evp <- q(pmin(ev_pos, 1e3)); colnames(evp) <- paste0("mean_v_given_pos_q", c(5, 25, 50, 75, 95))
  cbind(cl[, c("type", "bonus")], round(pa_q, 3), round(td_q, 2), round(pos, 3), round(v10, 3), round(evp, 2))
}
