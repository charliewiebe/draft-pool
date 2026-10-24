# Season-level accrual model for unfinished control windows (Stage 4 refit).
# Every post-debut calendar season of every arrival is a row until the control window closes (6 MLB seasons) or the
# player leaves pro ball (2 consecutive seasons with no MLB or affiliated MiLB games, the exit rule used in outcomes).
# Row outcome: state in {none, MiLB only, MLB}; if MLB, that season's bWAR. Censoring at 2026 is by calendar time,
# so every observed season contributes without requiring the career to be finished.

accrual_panel <- function(md, seasons, last_season = 2026L) {
  d <- md |> dplyr::filter(debuted, debut_season <= last_season) |>
    dplyr::select(mlbam_id, year, debut_season, age_at_draft, role, school, group, out_of_pro, control_status)
  s <- seasons |> dplyr::filter(mlbam_id %in% d$mlbam_id) |>
    dplyr::mutate(mlb = dplyr::coalesce(mlb_games, 0L) > 0 | dplyr::coalesce(pa, 0) > 0 | dplyr::coalesce(ipouts, 0) > 0,
                  milb = !is.na(milb_top_level), bwar = dplyr::coalesce(bwar, 0)) |>
    dplyr::select(mlbam_id, season, mlb, milb, bwar)
  full <- d |> dplyr::select(mlbam_id, debut_season) |> dplyr::rowwise() |>
    dplyr::reframe(mlbam_id = mlbam_id, season = debut_season:last_season) |> dplyr::ungroup() |>
    dplyr::left_join(s, by = c("mlbam_id", "season")) |>
    dplyr::mutate(mlb = dplyr::coalesce(mlb, FALSE), milb = dplyr::coalesce(milb, FALSE), bwar = ifelse(mlb, dplyr::coalesce(bwar, 0), 0),
                  state = ifelse(mlb, 3L, ifelse(milb, 2L, 1L)))
  # the debut season is an MLB season by definition (some debuts have no Stats API game row in that season)
  full <- full |> dplyr::left_join(d |> dplyr::select(mlbam_id, debut_season), by = "mlbam_id") |>
    dplyr::mutate(state = ifelse(season == debut_season, 3L, state), mlb = state == 3L) |> dplyr::select(-debut_season)
  full <- full |> dplyr::arrange(mlbam_id, season) |> dplyr::group_by(mlbam_id) |>
    dplyr::mutate(w_adj = ifelse(season == 2020, bwar * 162 / 60, bwar),
                  k_prev = dplyr::lag(cumsum(mlb), default = 0L),
                  cumw_prev = dplyr::lag(cumsum(ifelse(mlb & cumsum(mlb) <= 6, bwar, 0)), default = 0),
                  last_mlb_prev = dplyr::lag(cummax(ifelse(mlb, season, 0L)), default = 0L),
                  prev_state = dplyr::lag(state, default = 3L),
                  none_run = { z <- integer(dplyr::n()); cnt <- 0L; for (i in seq_len(dplyr::n())) { z[i] <- cnt; cnt <- if (state[i] == 1L) cnt + 1L else 0L }; z }) |>
    dplyr::ungroup()
  # last MLB season's (2020-prorated) WAR and mean per MLB season before s
  lw <- full |> dplyr::filter(mlb) |> dplyr::select(mlbam_id, last_mlb_prev = season, lastw = w_adj)
  full <- full |> dplyr::left_join(lw, by = c("mlbam_id", "last_mlb_prev")) |>
    dplyr::left_join(d |> dplyr::select(mlbam_id, year, age_at_draft, role, school, group, out_of_pro, control_status), by = "mlbam_id") |>
    dplyr::mutate(meanw = ifelse(k_prev > 0, cumw_prev / k_prev, 0), gap = season - last_mlb_prev,
                  age = age_at_draft + (season - year) - 26)
  rows <- full |> dplyr::filter(k_prev >= 1, k_prev <= 5, none_run < 2)
  # state at the end of the last observed season (for forward simulation of open windows)
  end <- full |> dplyr::group_by(mlbam_id) |> dplyr::slice_max(season, n = 1) |> dplyr::ungroup() |>
    dplyr::mutate(k_end = k_prev + mlb, cumw_end = cumw_prev + ifelse(mlb & k_prev < 6, bwar, 0),
                  last_mlb_end = ifelse(mlb, season, last_mlb_prev),
                  none_run_end = ifelse(state == 1L, none_run + 1L, 0L))
  end <- end |> dplyr::left_join(lw |> dplyr::rename(last_mlb_end = last_mlb_prev, lastw_end = lastw), by = c("mlbam_id", "last_mlb_end")) |>
    dplyr::select(mlbam_id, year, role, school, group, age_at_draft, out_of_pro, control_status, k_end, cumw_end, last_mlb_end,
                  lastw_end, state_end = state, none_run_end)
  list(rows = rows, end = end)
}

accrual_stan_data <- function(rows, season_min = 2013L, season_max = 2026L) {
  rows <- rows[rows$season <= season_max, ]
  w <- which(rows$state == 3L)
  list(N = nrow(rows), state = rows$state, prev = rows$prev_state, kp = rows$k_prev, gapc = pmin(rows$gap, 4L),
       lastw = rows$lastw, meanw = rows$meanw, age = rows$age, hit = as.integer(rows$role == "H"),
       Y = season_max - season_min + 1L, yr = rows$season - season_min + 1L,
       NW = length(w), iw = w, war = rows$bwar[w], y2020 = as.integer(rows$season[w] == 2020), y20idx = 2020L - season_min + 1L)
}

# Forward simulation of open control windows from posterior draws of the accrual model.
# end: one row per player (state after last_obs); returns matrix [n_draws x n_players] of simulated control-period V,
# and optionally the WAR accrued between last_obs+1 and stop_season (for out-of-time validation).
accrual_simulate <- function(dr, end, draws_idx, last_obs = 2026L, stop_season = last_obs + 12L, seed = 1L) {
  set.seed(seed)
  n <- nrow(end); out <- matrix(NA_real_, length(draws_idx), n); kfin <- out
  M <- posterior::as_draws_matrix(posterior::subset_draws(dr, draw = draws_idx))
  hit <- as.integer(end$role == "H")
  for (j in seq_along(draws_idx)) {
    p <- function(nm) as.numeric(M[j, nm])
    P <- lapply(1:2, function(c) list(a0 = p(sprintf("a0[%d]", c)), prev = c(0, p(sprintf("a_milb[%d]", c)), 0),
      k = c(0, sapply(1:4, function(i) p(sprintf("a_k[%d,%d]", c, i)))), gap = c(0, sapply(1:3, function(i) p(sprintf("a_gap[%d,%d]", c, i)))),
      l = p(sprintf("b_l[%d]", c)), m = p(sprintf("b_m[%d]", c)), age = p(sprintf("b_age[%d]", c)), age2 = p(sprintf("b_age2[%d]", c)),
      hit = p(sprintf("b_hit[%d]", c)), sig = p(sprintf("sig_s[%d]", c))))
    ck <- c(0, sapply(1:4, function(i) p(sprintf("c_k[%d]", i))))
    k <- end$k_end; cw <- end$cumw_end; lastm <- end$last_mlb_end; lastw <- dplyr::coalesce(end$lastw_end, 0)
    st <- end$state_end; nr <- end$none_run_end
    open <- k < 6 & nr < 2 & !(end$out_of_pro %in% TRUE)
    for (s in (last_obs + 1L):stop_season) {
      if (!any(open)) break
      age <- end$age_at_draft + (s - end$year) - 26
      gc <- pmin(s - lastm, 4L); meanw <- ifelse(k > 0, cw / k, 0); kk <- pmin(pmax(k, 1L), 5L)
      eta <- matrix(0, n, 3)
      for (c in 1:2) {
        q <- P[[c]]
        eta[, c + 1L] <- q$a0 + q$prev[st] + q$k[kk] + q$gap[gc] + q$l * lastw + q$m * meanw + q$age * age + q$age2 * age^2 +
          q$hit * hit + q$sig * stats::rnorm(1)
      }
      pr <- exp(eta - apply(eta, 1, max)); pr <- pr / rowSums(pr)
      u <- stats::runif(n); ns <- ifelse(u < pr[, 1], 1L, ifelse(u < pr[, 1] + pr[, 2], 2L, 3L))
      mu <- p("c0") + ck[kk] + p("c_l") * lastw + p("c_m") * meanw + p("c_age") * age + p("c_age2") * age^2 + p("c_hit") * hit + p("c_gap") * (gc > 1)
      sg <- exp(p("s0[1]") * (1 - hit) + p("s0[2]") * hit + p("s_m") * pmax(meanw, 0) + p("s_l") * pmax(lastw, 0))
      # season WAR draws are clipped to [-4, 11] (observed range of single control seasons: -3.7 to 9.6, 2020 prorated);
      # without the clip, upper-tail draws feed back through the history-dependent scale and a few paths explode
      em <- exp(p("e0[1]") * (1 - hit) + p("e0[2]") * hit + p("e_m") * pmax(meanw, 0) + p("e_l") * pmax(lastw, 0))
      w <- pmin(pmax(mu + sg * stats::rnorm(n) + stats::rexp(n, 1 / em), -4), 11)
      play <- open & ns == 3L
      cw <- cw + ifelse(play, w, 0); k <- k + play
      lastw <- ifelse(play, w, lastw); lastm <- ifelse(play, s, lastm)
      nr <- ifelse(open, ifelse(ns == 1L, nr + 1L, 0L), nr); st <- ifelse(open, ns, st)
      open <- open & k < 6 & nr < 2
    }
    out[j, ] <- cw; kfin[j, ] <- k
  }
  list(v = out, k = kfin)
}

# Out-of-time validation: fit on seasons <= cut, simulate cut+1..2026 for windows open at cut, compare with what happened.
accrual_oot_check <- function(fit_cut, md, seasons, cut = 2021L, n_draws = 200L, seed = 7L) {
  pn <- accrual_panel(md, seasons, last_season = cut)
  full <- accrual_panel(md, seasons, last_season = 2026L)$end
  e <- pn$end |> dplyr::filter(k_end < 6, none_run_end < 2) |> dplyr::mutate(out_of_pro = FALSE)
  idx <- round(seq(1, posterior::ndraws(fit_cut$draws), length.out = n_draws))
  sim <- accrual_simulate(fit_cut$draws, e, idx, last_obs = cut, stop_season = 2026L, seed = seed)
  obs <- full[match(e$mlbam_id, full$mlbam_id), ]
  obs_v <- obs$cumw_end; obs_k <- obs$k_end
  tiers <- function(v) factor(v_tier(v), levels = 1:5, labels = c("<=0", "(0,2]", "(2,5]", "(5,10]", ">10"))
  pt <- t(apply(sim$v, 1, function(v) prop.table(table(tiers(v)))))
  tier_tab <- tibble::tibble(tier = levels(tiers(0)), observed = as.numeric(prop.table(table(tiers(obs_v)))),
    predicted = colMeans(pt), lo = apply(pt, 2, stats::quantile, 0.05), hi = apply(pt, 2, stats::quantile, 0.95))
  ks <- t(apply(sim$k - e$k_end[col(sim$k)], 1, function(x) prop.table(table(factor(pmin(x, 5), levels = 0:5)))))
  obs_k <- pmin(obs_k, 6L)                                   # seasons beyond the six-season control window do not count
  k_tab <- tibble::tibble(added_mlb_seasons = 0:5, observed = as.numeric(prop.table(table(factor(pmin(obs_k - e$k_end, 5), levels = 0:5)))),
    predicted = colMeans(ks), lo = apply(ks, 2, stats::quantile, 0.05), hi = apply(ks, 2, stats::quantile, 0.95))
  gain <- tibble::tibble(group = c("all", "k_at_cut<=2", "k_at_cut>=3", "pitchers", "hitters"),
    sel = list(rep(TRUE, nrow(e)), e$k_end <= 2, e$k_end >= 3, e$role == "P", e$role == "H")) |>
    dplyr::rowwise() |> dplyr::mutate(n = sum(sel), observed_mean_added_war = mean(obs_v[sel] - e$cumw_end[sel]),
      predicted = mean(rowMeans(sim$v[, sel, drop = FALSE]) - mean(e$cumw_end[sel])),
      lo = stats::quantile(rowMeans(sim$v[, sel, drop = FALSE]) - mean(e$cumw_end[sel]), 0.05),
      hi = stats::quantile(rowMeans(sim$v[, sel, drop = FALSE]) - mean(e$cumw_end[sel]), 0.95)) |> dplyr::select(-sel) |> dplyr::ungroup()
  list(n_open = nrow(e), tiers = tier_tab, added_seasons = k_tab, added_war = gain)
}

# Multiple imputation of control-period V for open windows; resolved players keep their observed V.
impute_control_v <- function(fit_acc, md, seasons, M = 5L, seed = 11L) {
  pn <- accrual_panel(md, seasons)
  e <- pn$end
  open <- e$k_end < 6 & e$none_run_end < 2 & !(e$out_of_pro %in% TRUE)
  idx <- round(seq(1, posterior::ndraws(fit_acc$draws), length.out = M + 2))[2:(M + 1)]
  sim <- accrual_simulate(fit_acc$draws, e[open, ], idx, seed = seed)
  imp <- lapply(seq_len(M), function(m) tibble::tibble(mlbam_id = e$mlbam_id[open], v_imp = sim$v[m, ], k_imp = sim$k[m, ], m = m))
  list(imputations = dplyr::bind_rows(imp), n_open = sum(open), open_ids = e$mlbam_id[open], end = e)
}

# Value-model data on completed/imputed V (no censoring) for classes <= max_year
value_stan_data_mi <- function(d, basis, imp, m, max_year = 2021L) {
  d <- d[d$year <= max_year, ]
  a <- d[d$event, ]
  im <- imp$imputations[imp$imputations$m == m, ]
  vi <- im$v_imp[match(a$mlbam_id, im$mlbam_id)]
  open <- a$mlbam_id %in% imp$open_ids
  v <- ifelse(open, vi, a$v_war_control_obs)
  B <- spline_basis(a$x, knots = basis$knots, boundary = basis$boundary)$B
  list(N = nrow(a), K = ncol(B), G = length(type_levels()), S = 3L, Y = 14L, O = 30L, B = B, g = a$group, s = a$school_idx,
       age_c = a$age_c, yr = a$year_idx, org = a$org_idx, cens = rep(0L, nrow(a)), v = v, tier = v_tier(v),
       td = a$t_event - td_center(), prior_only = 0L)
}

imputation_summary <- function(imp, md) {
  a <- md |> dplyr::filter(debuted, year <= 2021)
  im <- imp$imputations |> dplyr::left_join(a |> dplyr::select(mlbam_id, year, v_war_control_obs, mlb_seasons), by = "mlbam_id") |>
    dplyr::filter(!is.na(year))
  im |> dplyr::group_by(year) |> dplyr::summarise(n_open = dplyr::n_distinct(mlbam_id), mean_obs_partial = mean(v_war_control_obs),
    mean_imputed_v = mean(v_imp), sd_between_m = stats::sd(tapply(v_imp, m, mean)),
    share_tier5 = mean(v_tier(v_imp) == 5), share_tier1 = mean(v_tier(v_imp) == 1), .groups = "drop") |>
    dplyr::left_join(a |> dplyr::group_by(year) |> dplyr::summarise(n_arrivals = dplyr::n(), .groups = "drop"), by = "year")
}
