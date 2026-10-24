# Stage 5.4: draft-market simulator (rounds 1-10; rounds 11+ added as resampled team-year amounts).
#
# One simulated draft:
#   1. Class template: a historical class (2014-2025 excluding 2020) supplies the ranked board (top 200: rank, type,
#      senior flag, age) and that year's pick order and slots. New draft-year random effects are drawn for every model,
#      so class depth varies both through the template and through the year effects.
#   2. Ranked players get a latent draft position from the faller model: out of the rounds 1-10 market with prob pi,
#      otherwise log position ~ Student-t(mu(rank, type), sigma(rank, school)).
#   3. Unranked players (virtual ranks 201-800; types, senior flags and ages resampled from that template year's
#      unranked rounds 1-10 picks) get log position = log(c * virtual rank) + sigma_200 * N(0,1); c is calibrated so the
#      number of top-200 players taken in rounds 1-10 matches history.
#   4. Other teams take players in order of latent position.
#   5. Bonuses: ranked players from the price model (rank and pick); unranked players from the deal model (pick, type,
#      senior) via component draw and truncated-normal magnitude.
#   6. Budget behaviour (calibrated, optional), relative to a team target (share of its rounds 1-10 pool):
#      - a team projected over target turns rounds 6-10 picks into the best available college seniors, latest picks first;
#      - an over-slot deal that would make the target infeasible is replaced by a non-over deal;
#      - a team projected under target by more than theta (spend_rounds, at most max_spend times) takes the best-ranked in-market high-school
#        player, including signability fallers, whose deal is then drawn with his rank-pick gap (usually over slot).

sim_inputs <- function(ranked, p, fit_fallers, fit_deals, deals_d, n_draws = 1000L, seed = 8L) {
  set.seed(seed)
  yrs <- setdiff(2014:2025, 2020L)
  pbk <- pick_basis(deals_d$lp); lp_mean <- mean(deals_d$lp)
  in_list <- ranked |> dplyr::filter(rank <= 200) |> dplyr::select(year, mlbam_id)
  pk <- p |> dplyr::filter(pool_round %in% TRUE, year %in% yrs) |>
    dplyr::mutate(slot25 = slot / sched_index, type = factor(paste(school_type, role, sep = "-"), levels = type_levels()),
                  senior = senior_nominal %in% TRUE)
  unr <- pk |> dplyr::anti_join(in_list, by = c("year", "mlbam_id")) |> dplyr::filter(!is.na(type)) |>
    dplyr::select(year, pick_overall, type, senior, age_at_draft) |> dplyr::arrange(year, pick_overall)
  templ <- lapply(yrs, function(y) {
    rb <- ranked |> dplyr::filter(year == y, rank <= 200, !is.na(type)) |>
      dplyr::transmute(rank, type, senior = senior_nominal %in% TRUE, age)
    order <- pk |> dplyr::filter(year == y) |> dplyr::arrange(pick_overall) |>
      dplyr::transmute(pick = pick_overall, team_id, round_label, slot25)
    Bp <- unclass(splines::bs(log(order$pick), knots = pbk$knots, Boundary.knots = pbk$boundary, degree = 3, intercept = TRUE))
    rg <- round_group(order$round_label)
    list(year = y, board = rb, order = order, unranked = unr[unr$year == y, ], Bp = Bp, lpc = log(order$pick) - lp_mean,
         late = rg %in% paste0("R", 6:10))
  })
  names(templ) <- yrs
  rb <- pick_basis(log(ranked$rank), df = 6L); pb <- pick_basis(deals_d$lp)
  thin <- function(f) { m <- posterior::as_draws_matrix(f$draws); m <- matrix(as.numeric(m), nrow(m), dimnames = list(NULL, colnames(m))); m[round(seq(1, nrow(m), length.out = n_draws)), ] }
  list(templates = templ, years = yrs, rank_basis = rb, pick_basis = pb, lp_mean = mean(deals_d$lp),
       fal = thin(fit_fallers), dea = thin(fit_deals), n_draws = n_draws)
}

# Deal model parameters for one posterior draw, laid out for fast per-pick evaluation on a template's pick order.
deal_draw_prep <- function(r, Bp, lpc) {
  K <- ncol(Bp); G <- 6L
  gm <- function(nm, rows, cols) matrix(r[sprintf("%s[%d,%d]", nm, rep(rows, times = length(cols)), rep(cols, each = length(rows)))], length(rows))
  list(Bw = Bp %*% t(gm("w", 1:3, 1:K)), Bmu = Bp %*% t(gm("mu_c", 1:3, 1:K)), wg = gm("w_g", 1:3, 1:G), wgs = gm("w_gs", 1:3, 1:G),
       wsr = r[sprintf("w_sr[%d]", 1:3)], wysd = r[sprintf("w_ysd[%d]", 1:3)], wx = gm("w_x", 1:3, 1:3), mg = gm("m_g", 1:3, 1:G),
       msr = r["m_sr"], mx = gm("m_x", 1:3, 1:3), oysd = r["o_ysd"], ls0 = r[sprintf("ls0[%d]", 1:3)], ls1 = r[sprintf("ls1[%d]", 1:3)], lpc = lpc)
}

# Component probabilities and magnitude draws for one pick k and one player (vector inputs of length 1)
deal_one <- function(dp, k, g, sr, x, wy, oy, allow_over = TRUE, over_tilt = 0, deep_tilt = 0) {
  eta <- dp$Bw[k, ] + dp$wg[, g] + dp$wgs[, g] * dp$lpc[k] + dp$wsr * sr + dp$wysd * wy + as.vector(dp$wx %*% x)
  e <- c(eta[1] + deep_tilt, eta[2], 0, eta[3] + over_tilt); if (!allow_over) e[4] <- -Inf
  pr <- exp(e - max(e)); pr <- pr / sum(pr)
  comp <- sample.int(4L, 1L, prob = pr)
  if (comp == 3L) return(c(comp, stats::runif(1, 0.985, 1.015)))
  ci <- c(1L, 2L, NA, 3L)[comp]
  mu <- dp$Bmu[k, ci] + dp$mg[ci, g] + sum(dp$mx[ci, ] * x) + (if (ci == 1L) dp$msr * sr else 0) + (if (ci == 3L) dp$oysd * oy else 0)
  z <- stats::rnorm(1, mu, exp(dp$ls0[ci] + dp$ls1[ci] * dp$lpc[k]))
  c(comp, z_to_ratio(comp, z))
}

default_cpar <- function() list(c = 0.86, budget = TRUE, target_sd = 0.02, target_mean = 1.04, theta = 0.06, late_exp = 0.85, max_spend = 1,
                                spend_rounds = c("R3", "R4", "R5"), senior_save = 0.85, h_hard = 0.7)

# Simulate one draft (other teams only; Stage 7 adds a focal-team policy).
#   cpar$c: unranked position scale; cpar$target_mean / target_sd: team budget targets as a share of the rounds 1-10 pool;
#   cpar$budget: FALSE = no budget behaviour (independent deals).
simulate_draft <- function(inp, j, ty, cpar = default_cpar(), n_virtual = 600L) {
  tp <- if (is.list(ty)) ty else inp$templates[[as.character(ty)]]; fr <- inp$fal[j, ]
  b <- tp$board; nb <- nrow(b)
  lr <- log(b$rank); lrc <- lr - log(50)
  B <- unclass(splines::bs(lr, knots = inp$rank_basis$knots, Boundary.knots = inp$rank_basis$boundary, degree = 3, intercept = TRUE))
  K <- ncol(B); g <- as.integer(b$type); s <- match(sub("-.*", "", as.character(b$type)), school_levels())
  mu <- as.vector(B %*% fr[sprintf("bm[%d]", 1:K)]) + fr[sprintf("m_g[%d]", g)] + fr["m_ysd"] * stats::rnorm(1)
  sg <- exp(fr["s0"] + fr["s1"] * lrc + fr[sprintf("s_s[%d]", s)])
  pio <- stats::plogis(fr["p0"] + fr["p1"] * lrc + fr[sprintf("p_g[%d]", g)] + fr["p_ysd"] * stats::rnorm(1))
  y <- ifelse(stats::runif(nb) < pio, Inf, mu + sg * stats::rnorm(nb))
  # virtual player v takes the attributes of (roughly) the v-th unranked pick of the template year, so seniors and late-round
  # types sit late on the board; beyond the observed unranked picks, attributes are resampled from the last 30% of them
  nu <- nrow(tp$unranked); vv <- seq_len(n_virtual)
  ui <- ifelse(vv <= nu, pmin(nu, pmax(1L, vv + sample(-8:8, n_virtual, replace = TRUE))), sample(ceiling(0.7 * nu):nu, n_virtual, replace = TRUE))
  u <- tp$unranked[ui, ]
  vr <- 200 + seq_len(n_virtual)
  # unranked positions are anchored on where the template year's unranked picks actually went (beyond them: after round 10)
  sg200 <- exp(fr["s0"] + fr["s1"] * (log(200) - log(50)) + mean(fr[sprintf("s_s[%d]", 1:3)]))
  pos <- ifelse(vv <= nu, u$pick_overall, max(tp$order$pick) + 2 * (vv - nu))
  yv <- log(cpar$c * pos) + sg200 * stats::rnorm(n_virtual)
  pool <- tibble::tibble(pid = seq_len(nb + n_virtual), ranked = c(rep(TRUE, nb), rep(FALSE, n_virtual)),
    rank = c(b$rank, vr), type = factor(c(as.character(b$type), as.character(u$type)), levels = type_levels()),
    senior = c(b$senior, u$senior), age = c(b$age, u$age_at_draft), y = c(y, yv))
  ord <- tp$order; L <- nrow(ord)
  dp <- deal_draw_prep(inp$dea[j, ], tp$Bp, tp$lpc)
  wy <- stats::rnorm(3); oy <- stats::rnorm(1)
  gi <- as.integer(pool$type); sri <- as.numeric(pool$senior)
  avail <- rep(TRUE, nrow(pool)); taken <- integer(L); comp <- integer(L); ratio <- numeric(L); smode <- logical(L); spend <- logical(L)
  # ranked players outside the rounds 1-10 market: a share h_hard will not sign at any price; the rest can be bought by a
  # team in spend mode (signability fallers)
  is_hs <- grepl("^HS", as.character(pool$type)); hard <- !is.finite(pool$y) & stats::runif(nrow(pool)) < cpar$h_hard
  sr_rounds <- if (is.null(cpar$spend_rounds)) c("R3", "R4", "R5") else cpar$spend_rounds
  mid <- round_group(ord$round_label) %in% sr_rounds
  team <- as.character(ord$team_id); teams <- unique(team)
  pool_t <- tapply(ord$slot25, team, sum)[teams]
  target <- pool_t * pmin(cpar$target_mean + cpar$target_sd * stats::rnorm(length(teams)), 1.05)
  # calibration offsets on the over-slot and deep-under logits by round group (R1+C, R2+CBB, R3-5, R6-10)
  rbin_idx <- match(offset_bin(ord$round_label), offset_bins())
  off_over <- if (is.null(cpar$off_over)) rep(0, 5) else cpar$off_over
  off_deep <- if (is.null(cpar$off_deep)) rep(0, 5) else cpar$off_deep
  committed <- stats::setNames(rep(0, length(teams)), teams); n_spend <- committed
  # remaining slots after pick k for the picking team, split by rounds 1-5 / 6-10
  late <- tp$late
  o <- order(pool$y); ptr <- 1L
  for (k in seq_len(L)) {
    tm <- team[k]
    after <- which(team == tm & seq_len(L) > k)
    rem_early <- sum(ord$slot25[after[!late[after]]]); rem_late <- sum(ord$slot25[after[late[after]]])
    budget <- isTRUE(cpar$budget)
    # senior mode, scheduled as late as possible: a pick in rounds 6-10 becomes a senior sign only when the projected
    # overshoot (all remaining picks at slot) cannot be covered by senior signs at the team's later rounds 6-10 picks
    overshoot <- committed[tm] + ord$slot25[k] + rem_early + rem_late - target[tm]
    smode[k] <- budget && late[k] && overshoot > cpar$senior_save * rem_late
    i <- NA_integer_
    if (smode[k]) { cand <- which(avail & pool$senior); if (length(cand)) i <- cand[which.min(pool$y[cand])] }
    # spend mode: a team projected to finish under target by more than theta (share of its pool) takes the best-ranked
    # available high-school player who is not a hard no-sign
    slack <- if (budget) (target[tm] - (committed[tm] + ord$slot25[k] + rem_early + cpar$late_exp * rem_late)) / pool_t[tm] else 0
    spend[k] <- budget && !smode[k] && mid[k] && slack > cpar$theta && n_spend[tm] < cpar$max_spend
    if (spend[k]) { cand <- which(avail & pool$ranked & is_hs & !hard); if (length(cand)) { i <- cand[which.min(pool$rank[cand])]; n_spend[tm] <- n_spend[tm] + 1 } }
    if (is.na(i)) { while (!avail[o[ptr]]) ptr <- ptr + 1L; i <- o[ptr] }
    avail[i] <- FALSE; taken[k] <- i
    x <- if (pool$ranked[i]) c(max(0, log(ord$pick[k] / pool$rank[i])), max(0, log(pool$rank[i] / ord$pick[k])), 0) else c(0, 0, 1)
    rb <- rbin_idx[k]
    dd <- deal_one(dp, k, gi[i], sri[i], x, wy, oy, over_tilt = off_over[rb], deep_tilt = off_deep[rb])
    if (budget && dd[1] == 4L && (committed[tm] + dd[2] * ord$slot25[k] + rem_early + 0.1 * rem_late > target[tm] ||
                                  committed[tm] + dd[2] * ord$slot25[k] + 0.9 * rem_early + 0.1 * rem_late > 1.05 * pool_t[tm]))
      dd <- deal_one(dp, k, gi[i], sri[i], x, wy, oy, allow_over = FALSE, deep_tilt = off_deep[rb])
    comp[k] <- dd[1]; ratio[k] <- dd[2]
    committed[tm] <- committed[tm] + ratio[k] * ord$slot25[k]
  }
  d <- dplyr::bind_cols(ord, pool[taken, ])
  d$ratio <- ratio; d$comp <- comp; d$bonus25 <- ratio * d$slot25; d$senior_mode <- smode; d$spend_mode <- spend
  d$template <- tp$year; d$draw <- j
  attr(d, "pool") <- dplyr::mutate(pool, hard = hard); attr(d, "taken") <- taken; attr(d, "re") <- list(wy = wy, oy = oy)
  d
}

# ---- historical targets and validation ----
hist_draft_stats <- function(p, ranked) {
  yrs <- setdiff(2014:2025, 2020L)
  h <- p |> dplyr::filter(pool_round %in% TRUE, year %in% yrs, signed %in% TRUE, !is.na(bonus), !is.na(slot)) |>
    dplyr::mutate(ratio = bonus / slot, comp = as.integer(cut(ratio, ratio_breaks(), right = FALSE)), pick = pick_overall,
                  type = factor(paste(school_type, role, sep = "-"), levels = type_levels()))
  r200 <- ranked |> dplyr::filter(rank <= 200, year %in% yrs)
  list(picks = h, ranked = r200)
}

pick_bin <- function(pick) cut(pick, c(0, 10, 30, 75, 150, 225, 400), labels = c("1-10", "11-30", "31-75", "76-150", "151-225", "226+"))
round_bin <- function(rl) { g <- round_group(rl); dplyr::case_when(g == "R1+C" ~ "R1+C", g == "R2-CBB" ~ "R2+CBB",
                                                                 g %in% c("R3", "R4", "R5") ~ "R3-5", TRUE ~ "R6-10") }

draft_summaries <- function(d, src) {
  d <- d |> dplyr::mutate(pbin = pick_bin(pick), rbin = round_bin(round_label), school = sub("-.*", "", as.character(type)))
  key <- if ("draw" %in% names(d)) c("draw", "template") else "year"
  list(
    ratio_by_pick = d |> dplyr::group_by(pbin) |> dplyr::summarise(q10 = stats::quantile(ratio, 0.1), q50 = stats::median(ratio), q90 = stats::quantile(ratio, 0.9),
      mean = mean(ratio), .groups = "drop") |> dplyr::mutate(source = src),
    by_round = d |> dplyr::group_by(rbin) |> dplyr::summarise(p_over = mean(comp == 4), p_deep = mean(comp == 1), p_at = mean(comp == 3),
      hs_share = mean(school == "HS", na.rm = TRUE), hs_share_of_over = mean(school[comp == 4] == "HS", na.rm = TRUE), .groups = "drop") |> dplyr::mutate(source = src),
    utilization = d |> dplyr::group_by(dplyr::across(dplyr::all_of(c(key, "team_id")))) |>
      dplyr::summarise(u = sum(if ("bonus25" %in% names(d)) bonus25 else bonus) / sum(if ("slot25" %in% names(d)) slot25 else slot), .groups = "drop") |>
      dplyr::summarise(q05 = stats::quantile(u, 0.05), q25 = stats::quantile(u, 0.25), q50 = stats::median(u), q75 = stats::quantile(u, 0.75),
                       q95 = stats::quantile(u, 0.95), share_over_105 = mean(u > 1.05), share_under_100 = mean(u < 1.0)) |> dplyr::mutate(source = src))
}

run_validation_sims <- function(inp, n_per_template = 60L, cpar = list(c = 0.6, budget = TRUE, target_sd = 0.03, target_mean = 1.03), seed = 9L) {
  set.seed(seed)
  jobs <- expand.grid(rep = seq_len(n_per_template), ty = inp$years)
  jobs$j <- sample.int(inp$n_draws, nrow(jobs), replace = TRUE)
  dplyr::bind_rows(lapply(seq_len(nrow(jobs)), function(k) simulate_draft(inp, jobs$j[k], jobs$ty[k], cpar) |> dplyr::mutate(draw = k)))
}

ranked_taken_stats <- function(d, hist_ranked) {
  sim <- d |> dplyr::filter(ranked) |> dplyr::group_by(draw) |> dplyr::summarise(n_taken = dplyr::n(), n_top50 = sum(rank <= 50),
    med_pick_top50 = stats::median(pick[rank <= 50]), .groups = "drop") |>
    dplyr::summarise(dplyr::across(-draw, list(mean = mean, lo = ~stats::quantile(.x, 0.05), hi = ~stats::quantile(.x, 0.95))))
  hist <- hist_ranked |> dplyr::group_by(year) |> dplyr::summarise(n_taken = sum(taken_r10), n_top50 = sum(taken_r10 & rank <= 50),
    med_pick_top50 = stats::median(pick_overall[taken_r10 & rank <= 50]), .groups = "drop") |>
    dplyr::summarise(dplyr::across(-year, list(mean = mean, lo = ~stats::quantile(.x, 0.05), hi = ~stats::quantile(.x, 0.95))))
  dplyr::bind_rows(sim |> dplyr::mutate(source = "simulated"), hist |> dplyr::mutate(source = "historical"))
}

# Calibrate the unranked-position scale c so that simulated top-200 players taken in rounds 1-10 match history.
calibrate_c <- function(inp, hist_ranked, grid = c(0.35, 0.45, 0.55, 0.65, 0.8, 1.0), n_per = 8L, seed = 10L) {
  target <- mean(tapply(hist_ranked$taken_r10, hist_ranked$year, sum))
  res <- sapply(grid, function(cc) {
    set.seed(seed)
    sims <- lapply(seq_len(n_per * length(inp$years)), function(k) {
      ty <- inp$years[(k - 1) %% length(inp$years) + 1]
      d <- simulate_draft(inp, sample.int(inp$n_draws, 1), ty, utils::modifyList(default_cpar(), list(c = cc, budget = FALSE))); sum(d$ranked) })
    mean(unlist(sims))
  })
  c_hat <- stats::approx(res, grid, xout = target, rule = 2)$y
  list(grid = tibble::tibble(c = grid, mean_ranked_taken = res), target = target, c = c_hat)
}

# Calibrate the budget behaviour (target mean, spend threshold theta, spend cap) by grid search on a combined loss over
# the historical team utilization quantiles and, by round group, P(over-slot), P(deep under) and HS share.
calibrate_budget <- function(inp, c_hat, hist_stats, means = c(1.03, 1.05), thetas = c(0.03, 0.08), max_spend = c(1L, 2L), h_hard = c(0.5, 0.8),
                             spend_rounds = list(`R3-5` = c("R3", "R4", "R5"), `R2-5` = c("R2-CBB", "R3", "R4", "R5"), `R3-10` = paste0("R", 3:10)),
                             sd = 0.02, n_per = 4L, seed = 12L) {
  hs <- draft_summaries(hist_stats$picks, "historical")
  hr <- hist_stats$ranked |> dplyr::group_by(year) |> dplyr::summarise(t50 = sum(taken_r10 & rank <= 50), .groups = "drop")
  combos <- expand.grid(m = means, theta = thetas, ms = max_spend, sr = names(spend_rounds), hh = h_hard, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(combos)), function(i) {
    cp <- utils::modifyList(default_cpar(), list(c = c_hat, target_mean = combos$m[i], target_sd = sd, theta = combos$theta[i],
                                                 max_spend = combos$ms[i], spend_rounds = spend_rounds[[combos$sr[i]]], h_hard = combos$hh[i]))
    sim <- run_validation_sims(inp, n_per_template = n_per, cpar = cp, seed = seed)
    ss <- draft_summaries(sim, "sim")
    uq <- c("q05", "q25", "q50", "q75", "q95")
    lu <- sum(((unlist(ss$utilization[uq]) - unlist(hs$utilization[uq])) / 0.02)^2)
    br <- dplyr::inner_join(hs$by_round, ss$by_round, by = "rbin", suffix = c("_h", "_s"))
    lr <- sum(((br$p_over_s - br$p_over_h) / 0.03)^2 + ((br$hs_share_s - br$hs_share_h) / 0.03)^2 + ((br$p_deep_s - br$p_deep_h) / 0.03)^2)
    pb <- dplyr::inner_join(hs$ratio_by_pick, ss$ratio_by_pick, by = "pbin", suffix = c("_h", "_s"))
    lp <- sum(((log(pb$q10_s) - log(pb$q10_h)) / 0.3)^2 + ((log(pb$q50_s) - log(pb$q50_h)) / 0.05)^2)
    t50 <- mean(tapply(sim$ranked & sim$rank <= 50, sim$draw, sum))
    lt <- ((t50 - mean(hr$t50)) / 0.5)^2
    tibble::tibble(m = combos$m[i], theta = combos$theta[i], max_spend = combos$ms[i], spend_rounds = combos$sr[i], h_hard = combos$hh[i],
                   loss_util = lu, loss_round = lr, loss_pick = lp, loss_top50 = lt, loss = lu + lr + lp + lt,
                   u_q05 = ss$utilization$q05, u_q50 = ss$utilization$q50, u_q95 = ss$utilization$q95, top50_taken = t50)
  })
  g <- dplyr::bind_rows(rows)
  best <- g[which.min(g$loss), ]
  list(grid = g, best = best, sd = sd, best_spend_rounds = spend_rounds[[best$spend_rounds]])
}

offset_bins <- function() c("R1+C", "R2+CBB", "R3-5", "R6-7", "R8-10")
offset_bin <- function(rl) { g <- round_group(rl); dplyr::case_when(g == "R1+C" ~ "R1+C", g == "R2-CBB" ~ "R2+CBB", g %in% c("R3", "R4", "R5") ~ "R3-5",
                                                                  g %in% c("R6", "R7") ~ "R6-7", TRUE ~ "R8-10") }
offset_shares <- function(d) d |> dplyr::mutate(ob = offset_bin(round_label)) |> dplyr::group_by(ob) |>
  dplyr::summarise(p_over = mean(comp == 4), p_deep = mean(comp == 1), .groups = "drop")

# Calibrate over-slot / deep-under logit offsets by round group so that simulated component shares match history
# (with the budget rules on). Fixed-point iteration on the logit scale.
calibrate_deal_offsets <- function(inp, cpar, hist_stats, iters = 5L, n_per = 8L, seed = 21L) {
  rb <- offset_bins()
  hs <- offset_shares(hist_stats$picks); hs <- hs[match(rb, hs$ob), ]
  cpar$off_over <- rep(0, 5); cpar$off_deep <- rep(0, 5); trace <- list()
  lg <- function(p) stats::qlogis(pmin(pmax(p, 1e-3), 1 - 1e-3))
  for (it in seq_len(iters)) {
    sim <- run_validation_sims(inp, n_per_template = n_per, cpar = cpar, seed = seed + it)
    ss <- offset_shares(sim); ss <- ss[match(rb, ss$ob), ]
    trace[[it]] <- tibble::tibble(iter = it, rbin = rb, p_over_sim = ss$p_over, p_over_hist = hs$p_over, p_deep_sim = ss$p_deep,
                                  p_deep_hist = hs$p_deep, off_over = cpar$off_over, off_deep = cpar$off_deep)
    cpar$off_over <- cpar$off_over + (lg(hs$p_over) - lg(ss$p_over))
    cpar$off_deep <- cpar$off_deep + ifelse(hs$p_deep > 0.005, lg(hs$p_deep) - lg(ss$p_deep), 0)
  }
  list(cpar = cpar, trace = dplyr::bind_rows(trace))
}

hist_utilization_r10 <- function(p) {
  yrs <- setdiff(2014:2025, 2020L)
  p |> dplyr::filter(pool_round %in% TRUE, year %in% yrs, signed %in% TRUE) |> dplyr::group_by(year, team_id) |>
    dplyr::summarise(miss = sum(is.na(bonus)), u = sum(bonus, na.rm = TRUE) / sum(slot, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(miss == 0) |> dplyr::pull(u)
}
