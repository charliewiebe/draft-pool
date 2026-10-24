# Stage 7 experiments. Each runs a set of policies on the shared scenario bank (common random numbers), so policy
# differences are paired; per-block (posterior draw) means give credible intervals; SEs are Monte Carlo errors.

positions7 <- function() c(1L, 3L, 5L, 8L, 10L, 13L, 16L, 20L, 24L, 28L, 32L, 36L, 40L)

policy_grid7 <- function(fs) {
  pols <- list(base_policy(fs))
  for (s1 in c(0.1, 0.2, 0.3, 0.4)) for (al in c("R2", "R2-R3", "R3-R5", "R2-R5")) pols[[length(pols) + 1]] <- policy_save_top(fs, s1, al)
  for (e1 in c(0.15, 0.3)) pols[[length(pols) + 1]] <- policy_spend_top(fs, e1)
  pols[[length(pols) + 1]] <- policy_barbell(fs, 0.2)
  for (m in c(0.25, 0.5)) pols[[length(pols) + 1]] <- policy_spread(fs, m)
  for (lam in c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)) for (bm in c(1.0, 1.05)) {
    pl <- policy_lagrange(fs, lam, bm); pl$label <- sprintf("adaptive (lambda=%.2f, budget %.0f%%)", lam, 100 * bm); pols[[length(pols) + 1]] <- pl
  }
  pols
}

policy_class7 <- function(label) dplyr::case_when(grepl("^BPA", label) ~ "BPA", grepl("^save-at-top", label) ~ "save-at-top",
  grepl("^spend-at-top", label) ~ "spend-at-top", grepl("^barbell", label) ~ "barbell", grepl("^spread", label) ~ "spread",
  grepl("^adaptive", label) ~ "adaptive", TRUE ~ "other")

# One position: pick values (for compensation/forfeit), all policies, plus the 105% variant of each class's best policy.
run_position7 <- function(p, scen, vb, o25, fm, r11t, beta = 1, use_fail = 1L, i2 = FALSE, extra = integer(), policies = NULL) {
  fs <- focal_set(o25, p, extra = extra)
  pv <- pick_values(scen, vb, fs, fm, r11t)
  cv <- ifelse(fs$comp_eligible, pv$comp, 0); fv <- c(first = pv$first, second = pv$second)
  pols <- if (is.null(policies)) policy_grid7(fs) else policies(fs)
  pols <- lapply(pols, function(x) { x$beta <- beta; x$use_fail <- use_fail; x })
  runs <- lapply(pols, function(pl) eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv, i2 = i2))
  base <- runs[[1]]
  summ <- dplyr::bind_rows(lapply(runs, summarise_eval, base = base)) |> dplyr::mutate(class = policy_class7(policy))
  # overage variants of each class's best (budget 1.0) policy
  best <- summ |> dplyr::filter(!grepl("budget 105", policy), class %in% c("save-at-top", "spend-at-top", "barbell", "spread", "BPA")) |>
    dplyr::group_by(class) |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE) |> dplyr::ungroup()
  ov <- lapply(best$policy, function(lb) {
    pl <- pols[[match(lb, vapply(pols, `[[`, "", "label"))]]
    pl$budget_mult <- 1.05
    if (pl$mode == 0L) {           # put the extra 5% into the rounds 2-5 caps
      ii <- which(fs$round %in% c("R2-CBB", "R3", "R4", "R5")); pl$cap[ii] <- pl$cap[ii] + (0.05 * fs$pool / length(ii)) / fs$slot[ii]
    }
    pl$label <- paste0(lb, " + 5% overage"); pl
  })
  runs_ov <- lapply(ov, function(pl) eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv, i2 = i2))
  summ <- dplyr::bind_rows(summ, dplyr::bind_rows(lapply(runs_ov, summarise_eval, base = base)) |> dplyr::mutate(class = paste0(policy_class7(policy), "+5%")))
  # per-scenario results of BPA and the best overall policy (for depth splits and risk)
  bi <- which.max(vapply(c(runs, runs_ov), function(r) mean(r$surplus), 0))
  allruns <- c(runs, runs_ov)
  list(p = p, pool = fs$pool, slot1 = fs$slot[1], summary = summ |> dplyr::mutate(p = p, pool = fs$pool, slot1 = fs$slot[1], beta = beta, use_fail = use_fail, i2 = i2),
       pick_values = pv$per_pick |> dplyr::mutate(p = p, pick = fs$pick, round = fs$round),
       scen_base = base, scen_best = allruns[[bi]], scen_runs = if (is.null(policies)) NULL else allruns)
}

# Q3 summary per position: best policy overall, best save-at-top, adaptive first-pick ratio, gains with intervals
q3_table <- function(res) {
  dplyr::bind_rows(lapply(res, function(x) {
    s <- x$summary
    best <- s |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE)
    st <- s |> dplyr::filter(class %in% c("save-at-top", "BPA")) |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE)
    ad <- s |> dplyr::filter(class == "adaptive") |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE)
    tibble::tibble(p = x$p, pool = x$pool, slot1 = x$slot1, best_policy = best$policy, best_gain = best$gain, best_gain_lo = best$gain_lo,
      best_gain_hi = best$gain_hi, best_p_pos = best$p_gain_pos, best_gain_se = best$gain_se,
      opt_save_policy = st$policy, opt_s1 = ifelse(st$class == "BPA", 0, as.numeric(sub(".*s1=([0-9.]+).*", "\\1", st$policy))),
      save_gain = st$gain, save_gain_lo = st$gain_lo, save_gain_hi = st$gain_hi, save_p_pos = st$p_gain_pos,
      adaptive_policy = ad$policy, adaptive_first_ratio = ad$first_ratio / x$slot1, adaptive_gain = ad$gain, adaptive_gain_lo = ad$gain_lo,
      adaptive_gain_hi = ad$gain_hi, adaptive_p_pos = ad$p_gain_pos, bpa_surplus = s$e_surplus[s$class == "BPA"][1])
  }))
}

# p*: first position from which the gain of the best non-BPA policy over BPA is reliably positive (P(gain > 0) >= 0.9 across
# posterior blocks) at that and all later positions in the grid
p_star <- function(q3, col = "best_p_pos", thr = 0.9) {
  ok <- q3[[col]] >= thr; ps <- q3$p
  if (!any(ok)) return(NA_integer_)
  for (i in seq_along(ps)) if (all(ok[i:length(ps)])) return(ps[i])
  NA_integer_
}

# Q2: marginal value of $1M moved from the first pick to a later pick, by school of the player bought there
exchange7 <- function(p, scen, vb, o25, fm, r11t, amount = 1e6) {
  fs <- focal_set(o25, p)
  pv <- pick_values(scen, vb, fs, fm, r11t); cv <- ifelse(fs$comp_eligible, pv$comp, 0); fv <- c(first = pv$first, second = pv$second)
  base <- eval_policy(scen, vb, fs, base_policy(fs), fm, r11t, comp_vals = cv, forfeit_vals = fv)
  rows <- list()
  targets <- c(which(fs$round == "R2-CBB")[1], which(fs$round == "R3")[1], which(fs$round == "R4")[1], which(fs$round == "R5")[1], which(fs$round == "R6")[1])
  for (ti in targets) for (sch in c(-1L, 0L, 1L)) {
    pl <- base_policy(fs); pl$cap[1] <- 1 - amount / fs$slot[1]; pl$cap[ti] <- 1 + amount / fs$slot[ti]; pl$school_restrict[ti] <- sch
    pl$label <- sprintf("$1M from pick %d to pick %d (%s)", fs$pick[1], fs$pick[ti], c("any", "HS", "4yr")[sch + 2])
    r <- eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv)
    rows[[length(rows) + 1]] <- summarise_eval(r, base) |> dplyr::mutate(to_pick = fs$pick[ti], to_round = fs$round[ti], school = c("any", "HS", "4yr")[sch + 2])
  }
  # rounds 11+: the $1M funds two over-threshold high-school signings (excess $500k each)
  pl <- base_policy(fs); pl$cap[1] <- 1 - amount / fs$slot[1]; pl$r11 <- list(n_thr = 6L, n_over = 2L, over_ratio = (150000 + amount / 2) / 150000)
  pl$label <- sprintf("$1M from pick %d to rounds 11+ (2 HS over threshold)", fs$pick[1])
  r <- eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv)
  rows[[length(rows) + 1]] <- summarise_eval(r, base) |> dplyr::mutate(to_pick = NA_integer_, to_round = "R11+", school = "HS")
  # staying in round 1: $1M more at the first pick, funded by $1M less spread over rounds 6-10 caps
  pl <- base_policy(fs); pl$cap[1] <- 1 + amount / fs$slot[1]; late <- which(fs$round %in% paste0("R", 6:10))
  pl$cap[late] <- pmax(0.05, 1 - (amount / length(late)) / fs$slot[late]); pl$label <- sprintf("$1M more at pick %d from rounds 6-10", fs$pick[1])
  r <- eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv)
  rows[[length(rows) + 1]] <- summarise_eval(r, base) |> dplyr::mutate(to_pick = fs$pick[1], to_round = "R1 (from R6-10)", school = "any")
  dplyr::bind_rows(rows) |> dplyr::mutate(from_pick = fs$pick[1], p = p)
}

# Depth split: gain of each position's best policy over BPA by tercile of high-school top-100 supply in the scenario
depth_split7 <- function(res) {
  dplyr::bind_rows(lapply(res, function(x) {
    d <- x$scen_best$surplus - x$scen_base$surplus
    q <- stats::quantile(x$scen_base$supply, c(1 / 3, 2 / 3))
    t <- cut(x$scen_base$supply, c(-Inf, q, Inf), labels = c("shallow", "middle", "deep"))
    tibble::tibble(p = x$p, depth = levels(t), gain = as.numeric(tapply(d, t, mean)), se = as.numeric(tapply(d, t, function(v) stats::sd(v) / sqrt(length(v)))),
                   policy = x$scen_best$policy[1])
  }))
}

# Risk: certainty equivalents of BPA and the best policy at each position
risk7 <- function(res, gammas = c(0, 0.01, 0.02, 0.05)) {
  ce <- function(x, g) if (g == 0) mean(x) else -log(mean(exp(-g * (x - mean(x)) / 1e6))) / g * 1e6 + mean(x)
  dplyr::bind_rows(lapply(res, function(x) dplyr::bind_rows(lapply(gammas, function(g)
    tibble::tibble(p = x$p, gamma = g, ce_bpa = ce(x$scen_base$realized, g), ce_best = ce(x$scen_best$realized, g),
                   sd_bpa = stats::sd(x$scen_base$realized), sd_best = stats::sd(x$scen_best$realized), best = x$scen_best$policy[1])))))
}

# Q6 value adjustments on the grid-cell layout
# fg_tv: count-weighted mean 'New Method' expected surplus value of FV 50+ prospects in FanGraphs' Table 2 (Clemens,
#   'Introducing an Updated Method for Prospect Valuation', FanGraphs, 2 Jul 2026): hitters (1x195 + 2x95 + 4x82 + 13x55 +
#   52x45)/72 = $52.3M; pitchers (1x95 + 4x70 + 7x45 + 39x33.5)/51 = $39.1M. FV 50+ is FanGraphs' top-100 tier.
q6_adjustments <- function(dir, fit_arr, basis, md, outcomes, tv, fg_tv = c(hitter = 52.3e6, pitcher = 39.1e6), c40 = 0.5e6, dpw = 8e6, r = 0.08) {
  cells <- arrow::read_parquet(file.path(dir, "grid_cells.parquet"))
  bon <- sort(unique(cells$bonus)); nb <- length(bon)
  idx <- (match(cells$type, type_levels()) - 1L) * 3L * nb + (match(cells$age_c, c(-1, 0, 1)) - 1L) * nb + match(cells$bonus, bon)
  # (b) trade value: P(top-100 by season 2) x max(0, FanGraphs surplus for a top-100 prospect, discounted 2 years,
  #     minus the model's own expected control value for players who were top-100 by season 2 in 2012-16 classes)
  ptop <- top100_cells(fit_arr, cells, basis)
  a <- md[md$year <= 2016 & md$top100_h, ]
  vt <- ifelse(a$event & !is.na(a$v_war), dpw * tv$tier_E1[cbind(v_tier(a$v_war), pmin(pmax(a$t_event, 0), 14) + 1, 1, 2)], 0)
  vt[a$event & is.na(a$v_war)] <- NA
  ev_top <- tapply(vt, ifelse(grepl("-P$", as.character(a$type)), "pitcher", "hitter"), mean, na.rm = TRUE)
  role <- ifelse(grepl("-P$", cells$type), "pitcher", "hitter")
  prem <- pmax(0, fg_tv[role] / (1 + r)^2 - ev_top[role])
  adj_b <- numeric(6 * 3 * nb); adj_b[idx] <- ptop * prem
  # (c) Rule 5: high-school draftees aged 18 or younger get a fifth season before protection; value = c40 x P(arrive and
  #     debut in season 5 or later), applied to high-school cells at or below the mean age (age_c <= 0)
  pr <- dplyr::collect(dplyr::summarise(dplyr::group_by(dplyr::filter(arrow::open_dataset(file.path(dir, "grid_predictive.parquet")), arrive), cell),
                                        n5 = sum(t_debut >= 5), .groups = "drop"))
  nd <- dplyr::collect(dplyr::summarise(dplyr::group_by(arrow::open_dataset(file.path(dir, "grid_predictive.parquet")), cell), n = dplyr::n()))
  pr <- dplyr::left_join(nd, pr, by = "cell"); pr$n5[is.na(pr$n5)] <- 0
  p5 <- pr$n5[match(cells$cell, pr$cell)] / pr$n[match(cells$cell, pr$cell)]
  elig <- grepl("^HS", cells$type) & cells$age_c <= 0
  adj_c <- numeric(6 * 3 * nb); adj_c[idx] <- ifelse(elig, c40 * p5 / (1 + r)^4, 0)
  list(adj_trade = adj_b, adj_rule5 = adj_c, ev_top100 = ev_top, premium = prem[match(c("hitter", "pitcher"), role)],
       cells = dplyr::mutate(cells, p_top100 = ptop, trade_adj = ptop * prem, p_debut_s5 = p5, rule5_adj = ifelse(elig, c40 * p5 / (1 + r)^4, 0)))
}

policy_grid7_small <- function(fs) {
  pols <- list(base_policy(fs))
  for (s1 in c(0.1, 0.2, 0.3)) for (al in c("R2-R5", "R3-R5")) pols[[length(pols) + 1]] <- policy_save_top(fs, s1, al)
  for (lam in c(1.5, 2.0, 2.5, 3.0, 3.5)) for (bm in c(1.0, 1.05)) {
    pl <- policy_lagrange(fs, lam, bm); pl$label <- sprintf("adaptive (lambda=%.2f, budget %.0f%%)", lam, 100 * bm); pols[[length(pols) + 1]] <- pl
  }
  pols
}

positions7_small <- function() c(1L, 5L, 10L, 16L, 24L, 32L, 40L)

# Held-out evaluation of each position's selected policy (chosen on the main bank) on a fresh scenario bank and fresh
# posterior draws: shrinkage between in-sample and held-out gains (optimizer's curse)
holdout7 <- function(res, scen_h, vb_h, o25, fm, r11t) {
  dplyr::bind_rows(lapply(res, function(x) {
    fs <- focal_set(o25, x$p); best <- x$summary |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE)
    pols <- c(policy_grid7(fs))
    labs <- vapply(pols, `[[`, "", "label")
    pl <- if (best$policy %in% labs) pols[[match(best$policy, labs)]] else NULL
    if (is.null(pl)) {            # overage variant
      lb <- sub(" \\+ 5% overage$", "", best$policy); pl <- pols[[match(lb, labs)]]; pl$budget_mult <- 1.05
      if (pl$mode == 0L) { ii <- which(fs$round %in% c("R2-CBB", "R3", "R4", "R5")); pl$cap[ii] <- pl$cap[ii] + (0.05 * fs$pool / length(ii)) / fs$slot[ii] }
      pl$label <- best$policy
    }
    pv <- pick_values(scen_h, vb_h, fs, fm, r11t); cv <- ifelse(fs$comp_eligible, pv$comp, 0); fv <- c(first = pv$first, second = pv$second)
    b <- eval_policy(scen_h, vb_h, fs, base_policy(fs), fm, r11t, comp_vals = cv, forfeit_vals = fv)
    r <- eval_policy(scen_h, vb_h, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv)
    summarise_eval(r, b) |> dplyr::mutate(p = x$p, in_sample_gain = best$gain, in_sample_se = best$gain_se)
  }))
}

# What the adaptive policy buys differently from BPA: by round group, mean ask/slot, share college, mean age_c, value per $
compare_picks7 <- function(p, scen, vb, o25, fm, r11t, lambda = 2, budget_mult = 1.05) {
  fs <- focal_set(o25, p)
  a <- attr(eval_policy(scen, vb, fs, base_policy(fs), fm, r11t, return_picks = TRUE), "picks") |> dplyr::mutate(policy = "BPA")
  pl <- policy_lagrange(fs, lambda, budget_mult)
  b <- attr(eval_policy(scen, vb, fs, pl, fm, r11t, return_picks = TRUE), "picks") |> dplyr::mutate(policy = sprintf("adaptive (lambda=%.2f, budget %.0f%%)", lambda, 100 * budget_mult))
  agec <- function(df) mapply(function(s, i) scen[[s]]$agec[i + 1], df$scen, df$pid)
  dplyr::bind_rows(a, b) |> dplyr::mutate(agec = agec(dplyr::bind_rows(a, b)), rgrp = dplyr::case_when(round == "R1+C" ~ "R1", round == "R2-CBB" ~ "R2",
      round %in% c("R3", "R4", "R5") ~ "R3-5", TRUE ~ "R6-10")) |>
    dplyr::group_by(policy, rgrp) |>
    dplyr::summarise(n = dplyr::n(), ask_over_slot = mean(ask / slot), share_college = mean(grepl("^(4yr|JUCO)", type)), share_hitter = mean(grepl("-H$", type)),
                     mean_age_c = mean(agec), surplus_per_pick = mean(ve - ask), value_per_dollar = sum(ve) / sum(ask), .groups = "drop") |>
    dplyr::mutate(p = p)
}

# I2 benchmark policies (value = bonus): spending levers only
policy_grid7_i2 <- function(fs) {
  pols <- list(base_policy(fs))
  ov <- base_policy(fs, 1.05); ii <- which(fs$round %in% c("R2-CBB", "R3", "R4", "R5"))
  ov$cap[ii] <- ov$cap[ii] + (0.05 * fs$pool / length(ii)) / fs$slot[ii]; ov$label <- "BPA + 5% overage in rounds 2-5"; pols[[2]] <- ov
  r11 <- base_policy(fs); r11$r11 <- list(n_thr = 10L, n_over = 2L, over_ratio = 2.5); r11$label <- "BPA + all rounds 11+ picks signed (2 over threshold)"; pols[[3]] <- r11
  for (lam in c(0.5, 0.9)) for (bm in c(1.0, 1.05)) {
    pl <- policy_lagrange(fs, lam, bm, i2 = TRUE); pl$label <- sprintf("adaptive I2 (lambda=%.2f, budget %.0f%%)", lam, 100 * bm); pols[[length(pols) + 1]] <- pl
  }
  pl <- policy_lagrange(fs, 0.9, 1.05, i2 = TRUE); pl$r11 <- list(n_thr = 10L, n_over = 2L, over_ratio = 2.5); pl$label <- "adaptive I2 (lambda=0.90, 105%) + rounds 11+"; pols[[length(pols) + 1]] <- pl
  pols
}

# Q3 decomposition: first-pick under-slot share s1 with the best adaptive continuation (lambda = the position's best),
# against BPA and against "first pick at slot, then adaptive" (selection and later-round reallocation without top savings)
q3_first7 <- function(p, scen, vb, o25, fm, r11t, q3, s1s = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) {
  fs <- focal_set(o25, p)
  lam <- as.numeric(sub(".*lambda=([0-9.]+).*", "\\1", q3$adaptive_policy[q3$p == p]))
  pv <- pick_values(scen, vb, fs, fm, r11t); cv <- ifelse(fs$comp_eligible, pv$comp, 0); fv <- c(first = pv$first, second = pv$second)
  base <- eval_policy(scen, vb, fs, base_policy(fs), fm, r11t, comp_vals = cv, forfeit_vals = fv)
  runs <- lapply(s1s, function(s1) eval_policy(scen, vb, fs, policy_first_fixed(fs, s1, lam), fm, r11t, comp_vals = cv, forfeit_vals = fv))
  at_slot <- runs[[1]]
  dplyr::bind_rows(lapply(seq_along(s1s), function(i) {
    a <- summarise_eval(runs[[i]], base); b <- summarise_eval(runs[[i]], at_slot)
    a |> dplyr::mutate(p = p, s1 = s1s[i], lambda = lam, gain_vs_first_at_slot = b$gain, gain_vs_first_at_slot_lo = b$gain_lo,
                       gain_vs_first_at_slot_hi = b$gain_hi, p_pos_vs_first_at_slot = b$p_gain_pos, first_ratio_realized = a$first_ratio / fs$slot[1])
  }))
}

# Q4: first overage band. For each position: (i) BPA-style plan vs the same plan with the extra 5% put into rounds 2-5 caps,
# (ii) the best adaptive plan at 100% vs 105% of pool (same lambda). Paired differences across scenarios and posterior blocks;
# value per extra dollar spent = change in E[value] / change in E[spend].
q4_7 <- function(p, scen, vb, o25, fm, r11t, q3) {
  fs <- focal_set(o25, p)
  lam <- as.numeric(sub(".*lambda=([0-9.]+).*", "\\1", q3$adaptive_policy[q3$p == p]))
  pv <- pick_values(scen, vb, fs, fm, r11t); cv <- ifelse(fs$comp_eligible, pv$comp, 0); fv <- c(first = pv$first, second = pv$second)
  ev <- function(pl) eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv)
  ii <- which(fs$round %in% c("R2-CBB", "R3", "R4", "R5"))
  bpa_at <- function(bm) { b <- base_policy(fs, bm); b$cap[ii] <- b$cap[ii] + ((bm - 1) * fs$pool / length(ii)) / fs$slot[ii]; b }
  b0 <- base_policy(fs); b3 <- bpa_at(1.03); b1 <- bpa_at(1.05)
  a0 <- policy_lagrange(fs, lam, 1.0); a3 <- policy_lagrange(fs, lam, 1.03); a1 <- policy_lagrange(fs, lam, 1.05)
  pair <- function(r0, r1, what) {
    d <- r1$surplus - r0$surplus; pb <- tapply(d, r1$block, mean)
    dspend <- mean(r1$spend_r1_10 - r0$spend_r1_10); dval <- mean((r1$value + r1$tax) - (r0$value + r0$tax))
    tibble::tibble(p = p, comparison = what, gain = mean(d), se = stats::sd(d) / sqrt(length(d)), lo = stats::quantile(pb, 0.05), hi = stats::quantile(pb, 0.95),
                   p_gain_pos = mean(pb > 0), extra_spend = dspend, extra_tax = mean(r1$tax - r0$tax), value_per_extra_dollar = dval / dspend,
                   p_over_pool = mean(r1$overage > 1e-9), mean_overage = mean(pmax(r1$overage, 0)), p_band2 = mean(r1$overage > 0.05 + 1e-9))
  }
  rb0 <- ev(b0); ra0 <- ev(a0)
  dplyr::bind_rows(pair(rb0, ev(b3), "BPA: 100% vs 103%"), pair(rb0, ev(b1), "BPA: 100% vs 105%"),
                   pair(ra0, ev(a3), sprintf("adaptive (lambda=%.2f): 100%% vs 103%%", lam)), pair(ra0, ev(a1), sprintf("adaptive (lambda=%.2f): 100%% vs 105%%", lam)))
}

# ---- Q5/robustness: outcome risk with player-level realizations ------------------------------------------------
# The engine's `realized` column uses one predictive draw per value cell per block, shared by every player in that cell,
# and a donor-based dollar value whose mean differs from the expected-value surface. Here each signed rounds 1-10 pick gets
# its own realization: realized = expected surplus under the block's posterior draw + an outcome residual (Vp - Vd) taken
# from an independently chosen block at the player's nearest grid cell, centered so residuals average zero per cell.
# Parameter uncertainty stays shared across players (the block); outcome noise is independent across players.
# The same player in the same scenario gets the same residual under every policy. Rounds 11+ use expected values.
residual_bank7 <- function(vb) {
  R <- mapply(function(p, d) p - d, vb$Vp, vb$Vd, SIMPLIFY = FALSE)
  M <- do.call(cbind, R); M - rowMeans(M)
}
realized7 <- function(r, scen, vb, RB, seed = 5L) {
  pk <- attr(r, "picks"); nb <- length(vb$lbg); nB <- ncol(RB)
  set.seed(seed); npool <- length(scen[[1]]$g)
  idx <- matrix(sample.int(nB, length(scen) * npool, replace = TRUE), length(scen), npool)
  ag <- vapply(seq_len(nrow(pk)), function(i) scen[[pk$scen[i]]]$agec[pk$pid[i] + 1], 0)
  ai <- pmin(3L, pmax(1L, round(ag) + 2L)); bi <- pmin(nb, pmax(1L, round(approx(vb$lbg, seq_len(nb), log(pk$ask), rule = 2)$y)))
  cell <- (match(pk$type, type_levels()) - 1L) * 3L * nb + (ai - 1L) * nb + bi
  res <- RB[cbind(cell, idx[cbind(pk$scen, pk$pid + 1L)])] * pk$signed
  r$realized2 <- r$surplus + as.numeric(tapply(res, factor(pk$scen, seq_along(scen)), sum, default = 0))
  r
}
risk7b <- function(p, scen, vb, o25, fm, r11t, res_p, gammas = c(0, 0.01, 0.02, 0.05)) {
  fs <- focal_set(o25, p); pv <- pick_values(scen, vb, fs, fm, r11t)
  cv <- ifelse(fs$comp_eligible, pv$comp, 0); fv <- c(first = pv$first, second = pv$second)
  pols <- policy_grid7(fs); labs <- vapply(pols, `[[`, "", "label")
  s <- res_p$summary; best_lab <- s$policy[which.max(s$e_surplus)]
  ad100 <- s |> dplyr::filter(policy_class7(policy) == "adaptive", !grepl("budget 105", policy)) |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE)
  use <- c(labs[1], best_lab, ad100$policy)
  RB <- residual_bank7(vb)
  runs <- lapply(use, function(lb) { pl <- pols[[match(lb, labs)]]; pl$beta <- 1; pl$use_fail <- 1L
    realized7(eval_policy(scen, vb, fs, pl, fm, r11t, comp_vals = cv, forfeit_vals = fv, return_picks = TRUE), scen, vb, RB) })
  ce <- function(x, g) if (g == 0) mean(x) else -log(mean(exp(-g * (x - mean(x)) / 1e6))) / g * 1e6 + mean(x)
  dplyr::bind_rows(lapply(seq_along(runs), function(k) dplyr::bind_rows(lapply(gammas, function(g) {
    x <- runs[[k]]$realized2; b <- runs[[1]]$realized2
    tibble::tibble(p = p, policy = use[k], gamma = g, ce = ce(x, g), ce_gain = ce(x, g) - ce(b, g), mean = mean(x), sd = stats::sd(x),
                   q05 = stats::quantile(x, 0.05), q50 = stats::quantile(x, 0.5), q95 = stats::quantile(x, 0.95),
                   p_beats_bpa = mean(x > b), e_surplus = mean(runs[[k]]$surplus), sd_old = stats::sd(runs[[k]]$realized))
  }))))
}
