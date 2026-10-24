# Stage 7: decision model. A focal team's pool plan is evaluated inside simulated draft markets (Stage 5 simulator,
# 2025 draft order and slots, classes resampled from 2014-25), with Stage 4 value surfaces (2012-21 refit) and Stage 6
# signing risk. Core loop in src/engine.cpp.
#
# Accounting
#   M1 (primary, real dollars): surplus = sum over signed players of (V - bonus) - overage tax - forfeited-pick value
#                               + compensation-pick value for unsigned rounds 1-3 picks.
#   M2 (pool dollars sunk): value = sum over signed players of V - tax - forfeits + compensation value. Used for the I2
#                               benchmark (V = bonus), where M1 is identically -tax - forfeits.
# Decisions use the posterior-mean value surface; evaluation uses the scenario's own posterior draw (nested design:
# n_blocks posterior draws x scenarios per draw), so parameter uncertainty enters the outcome distribution.

load_engine <- function() {
  if (!exists("engine_eval", mode = "function")) {
    dir.create("build/rcpp", recursive = TRUE, showWarnings = FALSE)
    Rcpp::sourceCpp("src/engine.cpp", cacheDir = "build/rcpp", env = globalenv())
  }
  invisible(TRUE)
}

# ---- draft order (2025) ----------------------------------------------------------------------------------------
order_2025 <- function(inp, p, si2025) {
  tp <- inp$templates[["2025"]]
  o <- tp$order
  o$slot_usd <- o$slot25 * si2025
  o$rg_sign <- match(sign_rg(o$round_label), sign_round_groups()) - 1L           # 0..3 for rounds 1-10
  o$rbin <- match(offset_bin(o$round_label), offset_bins()) - 1L
  o$round <- round_group(o$round_label)
  o$comp_eligible <- o$round_label %in% c("1", "1C", "C-A", "2", "2C", "C-B", "3") | o$round_label %in% c("SUP", "SUP-2", "SUP-3")
  list(order = o, Bp = tp$Bp, lpc = tp$lpc, late = tp$late)
}

# Focal pick set for a team whose first pick is overall pick p: in each later regular round, the pick at the same
# relative position within the round ("mirrored" position). extra = additional overall picks (e.g. a CB pick).
focal_set <- function(o25, p, extra = integer()) {
  o <- o25$order; rounds <- o$round_label
  rel <- (which(o$pick[rounds == rounds[o$pick == p]] == p)) / sum(rounds == rounds[o$pick == p])
  ks <- which(o$pick == p)
  for (r in as.character(2:10)) {
    idx <- which(rounds == r)
    if (length(idx)) ks <- c(ks, idx[max(1L, ceiling(rel * length(idx)))])
  }
  ks <- sort(unique(c(ks, which(o$pick %in% extra))))
  list(k = ks - 1L, pick = o$pick[ks], slot = o$slot_usd[ks], round = o$round[ks], rg = o$rg_sign[ks],
       comp_eligible = o$comp_eligible[ks], pool = sum(o$slot_usd[ks]))
}

# ---- scenarios -------------------------------------------------------------------------------------------------
make_scenarios <- function(inp, cpar, o25, md, S = 2000L, n_blocks = 100L, seed = 31L) {
  set.seed(seed)
  sch_mean <- tapply(md$age_at_draft, md$school, mean)
  yrs <- inp$years
  out <- vector("list", S)
  blocks <- rep(seq_len(n_blocks), length.out = S)
  for (s in seq_len(S)) {
    ty <- yrs[sample.int(length(yrs), 1)]; j <- sample.int(inp$n_draws, 1)
    tpl <- inp$templates[[as.character(ty)]]
    tpl$order <- o25$order; tpl$Bp <- o25$Bp; tpl$lpc <- o25$lpc; tpl$late <- o25$late
    d <- simulate_draft(inp, j, tpl, cpar)
    pool <- attr(d, "pool"); taken <- attr(d, "taken"); re <- attr(d, "re")
    n <- nrow(pool)
    rest <- setdiff(seq_len(n), taken)
    in_mkt <- rest[is.finite(pool$y[rest]) & !pool$hard[rest]]; in_mkt <- in_mkt[order(pool$y[in_mkt])]
    soft <- rest[!is.finite(pool$y[rest]) & !pool$hard[rest]]; soft <- soft[order(pool$rank[soft])]
    pi <- c(taken, in_mkt, soft)
    school <- match(sub("-.*", "", as.character(pool$type)), school_levels())
    dp <- deal_draw_prep(inp$dea[j, ], o25$Bp, o25$lpc)
    r <- inp$dea[j, ]
    out[[s]] <- list(
      pi = as.integer(pi - 1L), L = nrow(o25$order), slot = o25$order$slot_usd, lpc = o25$lpc, Bw = dp$Bw, Bmu = dp$Bmu,
      rbin = as.integer(o25$order$rbin), g = as.integer(pool$type) - 1L, school = as.integer(school - 1L), ranked = as.integer(pool$ranked),
      rank = as.numeric(pool$rank), agec = as.numeric(pool$age - sch_mean[school_levels()[school]]), sr = as.numeric(pool$senior),
      u1 = stats::runif(n), z2 = stats::rnorm(n), u3 = stats::runif(n),
      wg = dp$wg, wgs = dp$wgs, wx = dp$wx, mg = dp$mg, mx = dp$mx, wsr = as.numeric(dp$wsr), wyre = as.numeric(dp$wysd * re$wy),
      ls0 = as.numeric(dp$ls0), ls1 = as.numeric(dp$ls1), msr = as.numeric(dp$msr), ore = as.numeric(dp$oysd * re$oy),
      off_over = if (is.null(cpar$off_over)) rep(0, 5) else cpar$off_over, off_deep = if (is.null(cpar$off_deep)) rep(0, 5) else cpar$off_deep,
      meta = list(template = ty, deal_draw = j, block = blocks[s],
                  hs_top100_supply = sum(pool$ranked & pool$rank <= 100 & grepl("^HS", pool$type) & !pool$hard),
                  ranked_top100_out = sum(pool$ranked & pool$rank <= 100 & !is.finite(pool$y))))
  }
  out
}

# ---- value surfaces ---------------------------------------------------------------------------------------------
# setting: list(variant = "a", r = 0.08, dpw = 8e6). Returns lbg, Vbar (posterior mean), and per-block Vd and Vp.
value_bank <- function(dir, setting = list(variant = "a", r = 0.08, dpw = 8e6), n_blocks = 100L, seed = 41L, adj = NULL) {
  cells <- arrow::read_parquet(file.path(dir, "grid_cells.parquet"))
  col <- sprintf("e1_%s_r%02d", setting$variant, round(100 * setting$r))
  ex <- arrow::open_dataset(file.path(dir, "grid_expectations.parquet"))
  all <- dplyr::collect(dplyr::select(ex, draw, cell, dplyr::all_of(col)))
  draws <- sort(unique(all$draw)); set.seed(seed); bd <- sample(draws, n_blocks)
  bon <- sort(unique(cells$bonus)); nb <- length(bon)
  idx <- function(cl) { cc <- cells[match(cl, cells$cell), ]
    (match(cc$type, type_levels()) - 1L) * 3L * nb + (match(cc$age_c, c(-1, 0, 1)) - 1L) * nb + match(cc$bonus, bon) }
  flat <- function(df, v) { out <- numeric(6 * 3 * nb); out[idx(df$cell)] <- v; out }
  mb <- all |> dplyr::group_by(cell) |> dplyr::summarise(v = mean(.data[[col]]), .groups = "drop")
  Vbar <- flat(mb, mb$v * setting$dpw)
  pr <- arrow::open_dataset(file.path(dir, "grid_predictive.parquet"))
  pv <- dplyr::collect(dplyr::filter(dplyr::select(pr, draw, cell, v_dollar), draw %in% bd))
  Vd <- lapply(bd, function(d) { x <- all[all$draw == d, ]; flat(x, x[[col]] * setting$dpw) })
  Vp <- lapply(bd, function(d) { x <- pv[pv$draw == d, ]; flat(x, x$v_dollar * setting$dpw / 8e6) })
  vb <- list(lbg = log(bon), bonus = bon, Vbar = Vbar, Vd = Vd, Vp = Vp, draws = bd, setting = setting)
  if (!is.null(adj)) vb <- apply_value_adjustment(vb, adj)
  vb
}

# Additive per-cell adjustments (Q6 asset/time views): adj is a flat vector on the same layout
apply_value_adjustment <- function(vb, adj) {
  vb$Vbar <- vb$Vbar + adj; vb$Vd <- lapply(vb$Vd, `+`, adj); vb$Vp <- lapply(vb$Vp, `+`, adj); vb
}

vlook_r <- function(vb, V, g, agec, bonus) {         # R mirror of the engine's lookup (g 1-based type index)
  nb <- length(vb$lbg); lb <- log(bonus)
  a <- pmin(1, pmax(-1, agec)); a0 <- ifelse(a < 0, 0L, 1L); wa <- ifelse(a < 0, a + 1, a)
  j <- pmin(pmax(findInterval(lb, vb$lbg), 1L), nb - 1L); wb <- pmin(1, pmax(0, (lb - vb$lbg[j]) / (vb$lbg[j + 1] - vb$lbg[j])))
  at <- function(ai, bi) V[(g - 1L) * 3L * nb + ai * nb + bi]
  v0 <- (1 - wb) * at(a0, j) + wb * at(a0, j + 1); v1 <- (1 - wb) * at(a0 + 1, j) + wb * at(a0 + 1, j + 1)
  (1 - wa) * v0 + wa * v1
}

# ---- policies ----------------------------------------------------------------------------------------------------
# rmin: minimum spend (share of slot) reserved for each later pick: 0.75 in rounds 1-5, 0.1 in rounds 6-10 (seniors)
base_policy <- function(fs, budget_mult = 1.0, r11 = list(n_thr = 6L, n_over = 0L, over_ratio = 2.5)) {
  nf <- length(fs$k); late <- !(fs$round %in% c("R1+C", "R2-CBB", "R3", "R4", "R5"))
  list(mode = 0L, cap = rep(1, nf), rmin = ifelse(late, 0.1, 0.75), type_restrict = rep(-1L, nf), school_restrict = rep(-1L, nf),
       lambda = 1, budget_mult = budget_mult, use_fail = 1L, beta = 1, i2_decide = 0L, r11 = r11, label = "BPA (slot caps)")
}

policy_save_top <- function(fs, s1, alloc = c("R2", "R2-R3", "R3-R5", "R2-R5"), budget_mult = 1.0, extra_to = NULL) {
  alloc <- match.arg(alloc); pol <- base_policy(fs, budget_mult)
  S <- s1 * fs$slot[1]; pol$cap[1] <- 1 - s1
  tgt <- switch(alloc, "R2" = "R2-CBB", "R2-R3" = c("R2-CBB", "R3"), "R3-R5" = c("R3", "R4", "R5"), "R2-R5" = c("R2-CBB", "R3", "R4", "R5"))
  ii <- which(fs$round %in% tgt); ii <- ii[ii > 1]
  extra <- S + (budget_mult - 1) * fs$pool
  if (length(ii)) pol$cap[ii] <- 1 + (extra / length(ii)) / fs$slot[ii]
  pol$label <- sprintf("save-at-top s1=%.2f -> %s", s1, alloc); pol
}

policy_spend_top <- function(fs, e1, budget_mult = 1.0) {
  pol <- base_policy(fs, budget_mult); pol$cap[1] <- 1 + e1 + (budget_mult - 1) * fs$pool / fs$slot[1]
  late <- !(fs$round %in% c("R1+C", "R2-CBB", "R3", "R4", "R5")); pol$cap[late] <- 0.2
  pol$label <- sprintf("spend-at-top e1=%.2f", e1); pol
}

policy_barbell <- function(fs, e12, budget_mult = 1.0) {
  pol <- base_policy(fs, budget_mult, r11 = list(n_thr = 10L, n_over = 2L, over_ratio = 2.5))
  pol$cap[1] <- 1 + e12; if (length(fs$k) > 1) pol$cap[2] <- 1 + e12
  mid <- fs$round %in% c("R3", "R4", "R5", paste0("R", 6:10)); pol$cap[mid] <- 0.3; pol$rmin[mid] <- 0.1
  pol$label <- sprintf("barbell e=%.2f", e12); pol
}

policy_spread <- function(fs, m, budget_mult = 1.0) {
  pol <- base_policy(fs, budget_mult)
  ii <- which(fs$round %in% c("R2-CBB", "R3", "R4", "R5")); pol$cap[ii] <- 1 + m
  late <- !(fs$round %in% c("R1+C", "R2-CBB", "R3", "R4", "R5")); pol$cap[late] <- 0.5
  pol$label <- sprintf("spread m=%.2f", m); pol
}

# Adaptive continuation with the first pick fixed by a cap: take the best player whose ask is at most (1 - s1) x slot
policy_first_fixed <- function(fs, s1, lambda, budget_mult = 1.05) {
  pol <- policy_lagrange(fs, lambda, budget_mult); pol$mode <- c(0L, rep(1L, length(fs$k) - 1L)); pol$cap[1] <- 1 - s1
  pol$label <- sprintf("first pick at %.0f%% of slot, then adaptive (lambda=%.2f, budget %.0f%%)", 100 * (1 - s1), lambda, 100 * budget_mult); pol
}

policy_lagrange <- function(fs, lambda, budget_mult = 1.0, i2 = FALSE) {
  pol <- base_policy(fs, budget_mult); pol$mode <- 1L; pol$lambda <- lambda; pol$i2_decide <- as.integer(i2)
  pol$label <- sprintf("adaptive (lambda=%.2f)", lambda); pol
}

# ---- evaluation ----------------------------------------------------------------------------------------------------
r11_module <- function(pol, vb, block, r11_types, fm, beta, threshold = 150000, i2 = FALSE) {
  r <- pol$r11; Vd <- vb$Vd[[block]]
  vthr <- if (i2) threshold else sum(r11_types$share * vlook_r(vb, Vd, match(r11_types$type, type_levels()), 0, threshold))
  pthr <- sum(r11_types$share * p_fail(fm, rep("R11-12", nrow(r11_types)), sub("-.*", "", r11_types$type), 1, beta))
  ask_o <- r$over_ratio * threshold
  vo <- if (i2) ask_o else mean(vlook_r(vb, Vd, match(c("HS-P", "HS-H"), type_levels()), 0, ask_o))
  po <- p_fail(fm, "R11-12", "HS", r$over_ratio, beta)
  list(n_thr = r$n_thr, n_over = r$n_over, value_thr = vthr, p_thr = pthr, ask_over = ask_o, value_over = vo, p_over = po,
       excess_planned = r$n_over * (ask_o - threshold), threshold = threshold)
}

eval_policy <- function(scen, vb, fs, pol, fm, r11_types, comp_vals = NULL, forfeit_vals = c(first = 0, second = 0),
                        i2 = FALSE, gammas = c(0, 0.02, 0.05), seed = 51L, return_picks = FALSE) {
  load_engine(); set.seed(seed)
  nf <- length(fs$k)
  fc <- list(fk = as.integer(fs$k), frg = as.integer(fs$rg), comp = if (is.null(comp_vals)) rep(0, nf) else comp_vals)
  al <- fm[fm$beta == pol$beta & fm$rg %in% sign_round_groups()[1:4], ]
  alpha <- as.numeric(t(sapply(sign_round_groups()[1:4], function(r) sapply(school_levels(), function(s) {
    a <- al$alpha[al$rg == r & al$school_type == s]; if (length(a)) a else al$alpha[al$rg == r & al$school_type == "4yr"] }))))
  res <- vector("list", length(scen)); picks <- if (return_picks) vector("list", length(scen)) else NULL
  blocks <- sort(unique(vapply(scen, function(x) x$meta$block, 1L)))
  r11b <- lapply(seq_along(vb$Vd), function(b) if (b %in% blocks) r11_module(pol, vb, b, r11_types, fm, pol$beta, i2 = i2) else NULL)
  r11_0 <- r11b[[blocks[1]]]
  pl <- list(mode = as.integer(pol$mode), cap = pol$cap, rmin = pol$rmin, type_restrict = as.integer(pol$type_restrict),
             school_restrict = as.integer(pol$school_restrict), lambda = pol$lambda,
             budget = pol$budget_mult * fs$pool - r11_0$excess_planned, use_fail = pol$use_fail, beta = pol$beta, alpha = alpha,
             i2_decide = as.integer(i2 || pol$i2_decide == 1L))
  vls <- lapply(seq_along(vb$Vd), function(b) list(lbg = vb$lbg, Vbar = vb$Vbar, Vd = vb$Vd[[b]], Vp = vb$Vp[[b]], i2_eval = as.integer(i2)))
  ce <- fs$comp_eligible
  for (s in seq_along(scen)) {
    sc <- scen[[s]]; b <- sc$meta$block
    r11 <- r11b[[b]]
    vl <- vls[[b]]
    e <- engine_eval(sc, fc, pl, vl)
    sg <- e$signed
    # rounds 11+: Bernoulli signings
    st <- if (r11$n_thr > 0) stats::runif(r11$n_thr) >= r11$p_thr else logical()
    so <- if (r11$n_over > 0) stats::runif(r11$n_over) >= r11$p_over else logical()
    excess <- sum(so) * (r11$ask_over - r11$threshold)
    pool_s <- fs$pool - sum(fs$slot[!sg])
    spend <- sum(e$ask[sg]) + excess
    over <- spend - pool_s; o <- over / pool_s
    tax <- if (o <= 1e-9) 0 else if (o <= 0.10) 0.75 * over else 1.0 * over
    forfeit <- if (o <= 0.05 + 1e-9) 0 else if (o <= 0.10) forfeit_vals[["first"]] else if (o <= 0.15) forfeit_vals[["first"]] + forfeit_vals[["second"]] else 2 * forfeit_vals[["first"]]
    compv <- sum(fc$comp[!sg & ce])
    v10 <- sum(e$ve[sg]); c10 <- sum(e$ask[sg])
    v11 <- sum(st) * r11$value_thr + sum(so) * r11$value_over; c11 <- sum(st) * r11$threshold + sum(so) * r11$ask_over
    surplus <- v10 - c10 + v11 - c11 - tax - forfeit + compv
    value <- v10 + v11 - tax - forfeit + compv
    real <- sum(e$vp[sg]) - c10 + v11 - c11 - tax - forfeit + compv       # one predictive realization (risk analysis)
    stranded <- sum(pmax(e$ask[!sg] - fs$slot[!sg], 0))
    res[[s]] <- c(s, b, sc$meta$template, sc$meta$hs_top100_supply, surplus, value,
                  real, c10, sum(e$ask), fs$pool, pool_s, o, tax, forfeit, sum(!sg), stranded, e$ask[1], e$ve[1], v11 - c11)
    if (return_picks) picks[[s]] <- data.frame(scen = s, f = seq_len(nf), pick = fs$pick, round = fs$round, pid = e$pid, type = type_levels()[sc$g[e$pid + 1] + 1],
                                               ask = e$ask, slot = fs$slot, pf = e$pf, signed = sg, ve = e$ve, vbar = e$vbar)
  }
  r <- as.data.frame(do.call(rbind, res))
  names(r) <- c("scen", "block", "template", "supply", "surplus", "value", "realized", "spend_r1_10", "spend_planned", "pool", "pool_signed",
                "overage", "tax", "forfeit", "n_failed", "stranded", "first_ask", "first_value", "r11_value")
  r$policy <- pol$label
  if (return_picks) attr(r, "picks") <- dplyr::bind_rows(picks)
  r
}

summarise_eval <- function(r, base = NULL, gammas = c(0.02, 0.05)) {
  ce <- function(x, g) if (g == 0) mean(x) else -log(mean(exp(-g * (x - mean(x)) / 1e6))) / g * 1e6 + mean(x)
  out <- tibble::tibble(policy = r$policy[1], n = nrow(r), e_surplus = mean(r$surplus), se = stats::sd(r$surplus) / sqrt(nrow(r)),
    e_value = mean(r$value), sd_realized = stats::sd(r$realized), ce_g02 = ce(r$realized, 0.02), ce_g05 = ce(r$realized, 0.05),
    spend_share = mean(r$spend_r1_10 / r$pool), p_over_pool = mean(r$overage > 1e-9), e_tax = mean(r$tax), p_forfeit = mean(r$forfeit > 0),
    e_failed = mean(r$n_failed), e_stranded = mean(r$stranded), first_ratio = mean(r$first_ask) / 1, first_value = mean(r$first_value))
  if (!is.null(base)) {
    d <- r$surplus - base$surplus; dv <- r$value - base$value
    pb <- tapply(d, r$block, mean)
    out$gain <- mean(d); out$gain_se <- stats::sd(d) / sqrt(length(d))
    out$gain_lo <- stats::quantile(pb, 0.05); out$gain_hi <- stats::quantile(pb, 0.95); out$p_gain_pos <- mean(pb > 0)
    out$gain_value <- mean(dv); out$gain_value_se <- stats::sd(dv) / sqrt(length(dv))
  }
  out
}

# Value of the focal team's pick at each focal slot under BPA (used for compensation picks and forfeited picks)
pick_values <- function(scen, vb, fs, fm, r11_types, disc = 1.08) {
  pol <- base_policy(fs); pol$use_fail <- 0L
  r <- eval_policy(scen, vb, fs, pol, fm, r11_types, return_picks = TRUE)
  pk <- attr(r, "picks") |> dplyr::group_by(f) |> dplyr::summarise(surplus = mean(ve - ask), value = mean(ve), .groups = "drop")
  list(per_pick = pk, comp = pk$surplus / disc, first = pk$surplus[1] / disc,
       second = pk$surplus[which(fs$round == "R2-CBB")[1]] / disc)
}

r11_type_shares <- function(p) {
  p |> dplyr::filter(year >= 2021, !(pool_round %in% TRUE), signed %in% TRUE, !is.na(bonus), bonus >= 0.9 * r11_threshold,
                     bonus <= 1.02 * r11_threshold, !is.na(school_type), !is.na(role)) |>
    dplyr::mutate(type = paste(school_type, role, sep = "-")) |> dplyr::count(type) |> dplyr::mutate(share = n / sum(n))
}

# P(top-100 prospect by season 2) per grid cell from the joint arrival model (draft-year RE integrated over its prior)
top100_cells <- function(fit_arr, cells, basis, n_draws = 1000L) {
  dr <- fit_arr$draws[round(seq(1, nrow(fit_arr$draws), length.out = n_draws)), ]
  B <- spline_basis(cells$x, knots = basis$knots, boundary = basis$boundary)$B
  g <- match(cells$type, type_levels()); s <- match(sub("-.*", "", cells$type), school_levels())
  D <- nrow(dr); R <- nrow(B); A <- matrix(cells$age_c, D, R, byrow = TRUE); X <- matrix(cells$x, D, R, byrow = TRUE)
  vl <- dmat(dr, "cl") %*% t(B) + dmat(dr, "l_g")[, g, drop = FALSE] + dmat(dr, "l_age")[, s, drop = FALSE] * A
  pl <- ord_probs_L(vl, dmat(dr, "cut")); tl <- dmat(dr, "t_lvl"); L <- dim(pl)[3]
  pt <- 0
  for (l in 1:L) pt <- pt + pl[, , l] * stats::plogis(dr$t0 + dr$t_x * X + dmat(dr, "t_g")[, g, drop = FALSE] + tl[, l])
  colMeans(pt)
}
