# Stage 5: supply and availability.
# 5.1 deal distribution by pick; 5.2 faller supply (pick taken vs pre-draft rank) and price given rank and pick;
# 5.3 class depth. Money is in 2025 draft-market dollars (schedule index), as in Stage 3.

base_draws <- function(fit, n) { m <- posterior::as_draws_matrix(fit$draws); m <- matrix(as.numeric(m), nrow(m), dimnames = list(NULL, colnames(m)))
  m[round(seq(1, nrow(m), length.out = n)), , drop = FALSE] }

ratio_breaks <- function() c(0, 0.25, 0.985, 1.015, Inf)
comp_labels <- function() c("deep under (<0.25)", "under [0.25,0.985)", "at slot", "over (>1.015)")

pick_basis <- function(lp, df = 7L, knots = NULL, boundary = NULL) {
  if (is.null(knots)) { knots <- stats::quantile(lp, seq(0, 1, length.out = df - 2)[-c(1, df - 2)]); boundary <- range(lp) + c(-0.05, 0.05) }
  list(B = unclass(splines::bs(lp, knots = knots, Boundary.knots = boundary, degree = 3, intercept = TRUE)), knots = knots, boundary = boundary)
}

deal_data <- function(p, ranked, min_year = 2014L) {
  rk <- ranked |> dplyr::filter(rank <= 200) |> dplyr::select(year, mlbam_id, rank)
  p |> dplyr::filter(year >= min_year, pool_round %in% TRUE, signed %in% TRUE, !is.na(bonus), !is.na(slot), slot > 0, !is.na(school_type), !is.na(role)) |>
    dplyr::left_join(rk, by = c("year", "mlbam_id")) |>
    dplyr::mutate(ratio = bonus / slot, comp = as.integer(cut(ratio, ratio_breaks(), right = FALSE)),
                  comp = ifelse(ratio == 1.015, 3L, comp),
                  type = factor(paste(school_type, role, sep = "-"), levels = type_levels()),
                  senior = senior_nominal %in% TRUE, lp = log(pick_overall),
                  unr = as.numeric(is.na(rank)), fall = ifelse(is.na(rank), 0, pmax(0, log(pick_overall / rank))),
                  rise = ifelse(is.na(rank), 0, pmax(0, log(rank / pick_overall))),
                  round_grp = round_group(round_label)) |>
    dplyr::filter(!is.na(type))
}

# round groups: first round incl. supplemental/compensation (C-A, SUP, PPI); second round incl. competitive balance
# rounds and later compensation rounds (CB-A, CB-B, C-B, SUP-2, SUP-3)
round_group <- function(rl) dplyr::case_when(rl %in% c("1", "1C", "C-A", "SUP", "PPI") ~ "R1+C",
                                             rl %in% c("2", "2C", "C-B", "SUP-2", "SUP-3") | grepl("^CB", rl) ~ "R2-CBB",
                                             TRUE ~ paste0("R", suppressWarnings(as.integer(sub("C$", "", rl)))))

lu_lo <- function() log(0.25)
lu_hi <- function() log(0.985)
ratio_to_z <- function(comp, ratio) {
  z <- rep(0, length(ratio))
  k <- comp == 1; z[k] <- log(log(0.25 / ratio[k]))
  k <- comp == 2; q <- (log(ratio[k]) - lu_lo()) / (lu_hi() - lu_lo()); z[k] <- stats::qlogis(pmin(pmax(q, 1e-4), 1 - 1e-4))
  k <- comp == 4; z[k] <- log(ratio[k] - 1.015)
  z
}
z_to_ratio <- function(comp, z) {
  r <- rep(1, length(z))
  k <- comp == 1; r[k] <- 0.25 * exp(-exp(z[k]))
  k <- comp == 2; r[k] <- exp(lu_lo() + (lu_hi() - lu_lo()) * stats::plogis(z[k]))
  k <- comp == 3; r[k] <- stats::runif(sum(k), 0.985, 1.015)
  k <- comp == 4; r[k] <- 1.015 + exp(z[k])
  r
}

deals_stan_data <- function(d, pb = NULL) {
  if (is.null(pb)) pb <- pick_basis(d$lp)
  B <- unclass(splines::bs(d$lp, knots = pb$knots, Boundary.knots = pb$boundary, degree = 3, intercept = TRUE))
  list(N = nrow(d), K = ncol(B), B = B, lp = d$lp - mean(d$lp), G = 6L, g = as.integer(d$type), sr = as.numeric(d$senior),
       fall = d$fall, rise = d$rise, unr = d$unr,
       Y = 14L, yr = d$year - 2011L, comp = d$comp, z = ratio_to_z(d$comp, d$ratio), prior_only = 0L)
}

# Posterior summaries of component probabilities and mean ratio on a pick x type grid (new draft year: RE integrated).
deal_grid <- function(fit, d, picks = c(1:40, seq(45, 315, 5)), n_draws = 800L, seed = 3L, fall = 0, unr = 0) {
  set.seed(seed)
  pb <- pick_basis(d$lp); lpm <- mean(d$lp)
  B <- unclass(splines::bs(log(picks), knots = pb$knots, Boundary.knots = pb$boundary, degree = 3, intercept = TRUE))
  dr <- base_draws(fit, n_draws)
  K <- ncol(B); G <- 6L
  cells <- expand.grid(pick = picks, type = type_levels(), senior = c(FALSE, TRUE), stringsAsFactors = FALSE) |>
    dplyr::filter(!(senior & !grepl("^4yr", type)))
  out <- vector("list", nrow(cells))
  re_w <- matrix(stats::rnorm(3 * nrow(dr)), ncol = 3); re_o <- stats::rnorm(nrow(dr))     # common new-year draws for every cell
  getm <- function(nm, r, cdim) matrix(dr[, sprintf("%s[%d,%d]", nm, r, seq_len(cdim))], ncol = cdim)
  for (ci in seq_len(nrow(cells))) {
    pi <- match(cells$pick[ci], picks); g <- match(cells$type[ci], type_levels()); sr <- as.numeric(cells$senior[ci])
    eta <- sapply(1:3, function(c) getm("w", c, K) %*% B[pi, ] + dr[, sprintf("w_g[%d,%d]", c, g)] + dr[, sprintf("w_gs[%d,%d]", c, g)] * (log(cells$pick[ci]) - lpm) + dr[, sprintf("w_sr[%d]", c)] * sr +
                    dr[, sprintf("w_x[%d,1]", c)] * fall + dr[, sprintf("w_x[%d,3]", c)] * unr +
                    dr[, sprintf("w_ysd[%d]", c)] * re_w[, c])
    e <- cbind(eta[, 1], eta[, 2], 0, eta[, 3]); pr <- exp(e - apply(e, 1, max)); pr <- pr / rowSums(pr)
    lpc <- log(cells$pick[ci]) - lpm
    xm <- function(c) dr[, sprintf("m_x[%d,1]", c)] * fall + dr[, sprintf("m_x[%d,3]", c)] * unr
    mu_d <- getm("mu_c", 1, K) %*% B[pi, ] + dr[, sprintf("m_g[1,%d]", g)] + dr[, "m_sr"] * sr + xm(1)
    mu_u <- getm("mu_c", 2, K) %*% B[pi, ] + dr[, sprintf("m_g[2,%d]", g)] + xm(2)
    mu_o <- getm("mu_c", 3, K) %*% B[pi, ] + dr[, sprintf("m_g[3,%d]", g)] + dr[, "o_ysd"] * re_o + xm(3)
    s <- sapply(1:3, function(c) exp(dr[, sprintf("ls0[%d]", c)] + dr[, sprintf("ls1[%d]", c)] * lpc))
    mc <- function(comp, mu, sd) rowMeans(matrix(z_to_ratio(rep(comp, length(mu) * 200), stats::rnorm(length(mu) * 200, rep(mu, 200), rep(sd, 200))), ncol = 200))
    em_d <- mc(1, mu_d, s[, 1]); em_u <- mc(2, mu_u, s[, 2]); em_o <- mc(4, mu_o, s[, 3])
    mr <- pr[, 1] * em_d + pr[, 2] * em_u + pr[, 3] * 1 + pr[, 4] * em_o
    q <- function(x) c(mean(x), stats::quantile(x, c(0.05, 0.95)))
    out[[ci]] <- tibble::tibble(pick = cells$pick[ci], type = cells$type[ci], senior = cells$senior[ci],
      p_deep = mean(pr[, 1]), p_under = mean(pr[, 2]), p_at = mean(pr[, 3]), p_over = q(pr[, 4])[1], p_over_lo = q(pr[, 4])[2], p_over_hi = q(pr[, 4])[3],
      mean_ratio = q(mr)[1], mean_ratio_lo = q(mr)[2], mean_ratio_hi = q(mr)[3], mean_over_ratio = mean(em_o))
  }
  dplyr::bind_rows(out) |> dplyr::mutate(fall = fall, unr = unr)
}

# Posterior predictive check: simulated vs observed component shares by round group and school type
deal_ppc <- function(fit, d, n_draws = 200L, seed = 4L) {
  set.seed(seed)
  sdat <- deals_stan_data(d); dr <- unclass(posterior::as_draws_matrix(fit$draws)); attr(dr, "nchains") <- NULL; idx <- round(seq(1, nrow(dr), length.out = n_draws))
  K <- sdat$K; sims <- matrix(0L, n_draws, sdat$N)
  for (j in seq_along(idx)) {
    r <- stats::setNames(as.numeric(dr[idx[j], ]), colnames(dr))
    eta <- sapply(1:3, function(c) sdat$B %*% r[sprintf("w[%d,%d]", c, 1:K)] + r[sprintf("w_g[%d,%d]", c, sdat$g)] + r[sprintf("w_gs[%d,%d]", c, sdat$g)] * sdat$lp + r[sprintf("w_sr[%d]", c)] * sdat$sr +
                    r[sprintf("w_x[%d,1]", c)] * sdat$fall + r[sprintf("w_x[%d,2]", c)] * sdat$rise + r[sprintf("w_x[%d,3]", c)] * sdat$unr +
                    r[sprintf("w_ysd[%d]", c)] * r[sprintf("w_yz[%d,%d]", c, sdat$yr)])
    e <- cbind(eta[, 1], eta[, 2], 0, eta[, 3]); pr <- exp(e - apply(e, 1, max)); pr <- pr / rowSums(pr)
    u <- stats::runif(sdat$N); sims[j, ] <- 1L + (u > pr[, 1]) + (u > pr[, 1] + pr[, 2]) + (u > pr[, 1] + pr[, 2] + pr[, 3])
  }
  grp <- interaction(d$round_grp, d$school_type, drop = TRUE)
  dplyr::bind_rows(lapply(levels(grp), function(gl) {
    k <- grp == gl
    obs <- tabulate(d$comp[k], 4) / sum(k)
    sm <- t(apply(sims[, k, drop = FALSE], 1, function(x) tabulate(x, 4) / length(x)))
    tibble::tibble(group = gl, n = sum(k), component = comp_labels(), observed = obs, predicted = colMeans(sm),
                   lo = apply(sm, 2, stats::quantile, 0.05), hi = apply(sm, 2, stats::quantile, 0.95))
  })) |> dplyr::mutate(inside_90 = observed >= lo & observed <= hi)
}

over_slot_tables <- function(d) {
  list(
    by_round_school = d |> dplyr::group_by(round_grp, school_type) |> dplyr::summarise(n = dplyr::n(), p_over = mean(comp == 4), p_deep = mean(comp == 1),
      median_ratio = stats::median(ratio), .groups = "drop"),
    hs_share_of_over = d |> dplyr::filter(comp == 4) |> dplyr::mutate(after_r1 = !(round_grp %in% "R1+C")) |>
      dplyr::group_by(after_r1) |> dplyr::summarise(n_over = dplyr::n(), hs_share = mean(school_type == "HS"), .groups = "drop"),
    by_year = d |> dplyr::group_by(year) |> dplyr::summarise(n = dplyr::n(), p_over = mean(comp == 4), p_at = mean(comp == 3), p_deep = mean(comp == 1),
      over_dollars_rel = sum(pmax(bonus - slot, 0)) / sum(slot), .groups = "drop"))
}

# ---- 5.2 ranked players ---------------------------------------------------------------------------------------
ranked_players <- function(rk, bio, p, dd, min_year = 2014L) {
  last_r10 <- p |> dplyr::filter(pool_round %in% TRUE) |> dplyr::group_by(year) |> dplyr::summarise(L = max(pick_overall), .groups = "drop")
  pk <- p |> dplyr::select(year, mlbam_id, pick_overall, round_label, pool_round, signed, bonus, slot, school_type, role, senior_nominal, age_at_draft, team_id)
  r <- rk |> dplyr::filter(list == "draft", list_year >= min_year) |> dplyr::rename(year = list_year) |>
    dplyr::left_join(bio |> dplyr::select(year = list_year, mlbam_id, committed, birth_date), by = c("year", "mlbam_id")) |>
    dplyr::left_join(pk, by = c("year", "mlbam_id")) |>
    dplyr::left_join(last_r10, by = "year") |>
    dplyr::left_join(dd |> dplyr::select(year, draft_start), by = "year")
  r |> dplyr::mutate(
    age_bio = as.numeric(as.Date(draft_start) - birth_date) / 365.25,
    role_inf = ifelse(position %in% c("RHP", "LHP", "P"), "P", "H"),
    school_inf = ifelse((!is.na(committed) & committed != "") | age_bio < 19.6, "HS", "4yr"),
    school = dplyr::coalesce(school_type, school_inf), role = dplyr::coalesce(role, role_inf),
    type = factor(paste(school, role, sep = "-"), levels = type_levels()),
    age = dplyr::coalesce(age_at_draft, age_bio),
    taken_r10 = !is.na(pick_overall) & pick_overall <= L,
    drafted_any = !is.na(pick_overall))
}

faller_stan_data <- function(r, rb = NULL) {
  lr <- log(r$rank)
  if (is.null(rb)) rb <- pick_basis(lr, df = 6L)
  B <- unclass(splines::bs(lr, knots = rb$knots, Boundary.knots = rb$boundary, degree = 3, intercept = TRUE))
  list(N = nrow(r), K = ncol(B), B = B, lr = lr - log(50), G = 6L, g = as.integer(r$type), S = 3L,
       s = match(sub("-.*", "", as.character(r$type)), school_levels()), Y = 14L, yr = r$year - 2011L,
       obs = as.integer(r$taken_r10), y = ifelse(r$taken_r10, log(r$pick_overall), 0), logL = log(r$L + 0.5))
}

# Availability: P(still on the board at pick p | rank, type) = P(not taken before p), new draft year
availability_grid <- function(fit, r, ranks = c(1, 3, 5, 10, 15, 20, 30, 40, 50, 75, 100, 150, 200), picks = c(1:40, seq(45, 315, 5)),
                              L = 315, n_draws = 600L, seed = 5L) {
  set.seed(seed)
  rb <- pick_basis(log(r$rank), df = 6L)
  B <- unclass(splines::bs(log(ranks), knots = rb$knots, Boundary.knots = rb$boundary, degree = 3, intercept = TRUE))
  dr <- base_draws(fit, n_draws)
  K <- ncol(B)
  out <- list()
  for (ti in seq_along(type_levels())) for (ri in seq_along(ranks)) {
    s_idx <- match(sub("-.*", "", type_levels()[ti]), school_levels()); lrc <- log(ranks[ri]) - log(50)
    ye <- stats::rnorm(nrow(dr)); ye2 <- stats::rnorm(nrow(dr))
    mu <- dr[, sprintf("bm[%d]", 1:K)] %*% B[ri, ] + dr[, sprintf("m_g[%d]", ti)] + dr[, "m_ysd"] * ye
    sg <- exp(dr[, "s0"] + dr[, "s1"] * lrc + dr[, sprintf("s_s[%d]", s_idx)])
    pio <- stats::plogis(dr[, "p0"] + dr[, "p1"] * lrc + dr[, sprintf("p_g[%d]", ti)] + dr[, "p_ysd"] * ye2)
    for (pk in picks) {
      # available at pick pk = not taken at picks 1..pk-1: y >= log(pk - 0.5)
      surv <- pio + (1 - pio) * (1 - stats::pnorm((log(pk - 0.5) - mu) / sg))
      out[[length(out) + 1]] <- tibble::tibble(type = type_levels()[ti], rank = ranks[ri], pick = pk,
        p_avail = mean(surv), lo = stats::quantile(surv, 0.05), hi = stats::quantile(surv, 0.95))
    }
  }
  dplyr::bind_rows(out)
}

# Empirical check of availability: observed share of rank-tier players still unpicked at pick p vs model (in-sample years)
availability_check <- function(fit, r, tiers = c(0, 10, 25, 50, 100, 150, 250), picks = c(10, 20, 30, 40, 60, 80, 100, 150, 200, 300),
                               n_draws = 300L, seed = 6L) {
  set.seed(seed)
  sd <- faller_stan_data(r); dr <- base_draws(fit, n_draws)
  K <- sd$K
  mu <- dr[, sprintf("bm[%d]", 1:K)] %*% t(sd$B) + dr[, sprintf("m_g[%d]", sd$g)] + dr[, "m_ysd"] * dr[, sprintf("m_yz[%d]", sd$yr)]
  sg <- exp(dr[, "s0"] + dr[, "s1"] %o% sd$lr + dr[, sprintf("s_s[%d]", sd$s)])
  pio <- stats::plogis(dr[, "p0"] + dr[, "p1"] %o% sd$lr + dr[, sprintf("p_g[%d]", sd$g)] + dr[, "p_ysd"] * dr[, sprintf("p_yz[%d]", sd$yr)])
  tier <- cut(r$rank, tiers)
  sch <- sub("-.*", "", as.character(r$type))
  # posterior predictive: simulate each player's outcome (out of market, or log pick) per draw
  N <- ncol(mu); out <- matrix(stats::runif(n_draws * N), n_draws) < pio
  ysim <- mu + sg * matrix(stats::rnorm(n_draws * N), n_draws)
  Lm <- matrix(sd$logL, n_draws, N, byrow = TRUE)
  gone <- !out & ysim < Lm                                   # taken in rounds 1-10 at pick exp(ysim)
  res <- list()
  for (tl in levels(tier)) for (sc in c("HS", "4yr")) {
    k <- which(tier == tl & sch == sc & r$year != 2020)
    if (length(k) < 20) next
    for (pk in picks) {
      obs <- mean(!r$taken_r10[k] | r$pick_overall[k] >= pk)
      m <- rowMeans(!gone[, k, drop = FALSE] | ysim[, k, drop = FALSE] >= log(pk - 0.5))
      res[[length(res) + 1]] <- tibble::tibble(rank_tier = tl, school = sc, n = length(k), pick = pk, observed = obs, predicted = mean(m),
                                               lo = stats::quantile(m, 0.05), hi = stats::quantile(m, 0.95))
    }
  }
  dplyr::bind_rows(res) |> dplyr::mutate(inside_90 = observed >= lo & observed <= hi)
}

# Smooth league-wide slot curve in 2025 schedule dollars (for slot "at a rank" and for extrapolation)
slot_curve <- function(p) {
  s <- p |> dplyr::filter(pool_round %in% TRUE, !is.na(slot), slot > 0, year != 2020) |>
    dplyr::mutate(ls = log(slot / sched_index), lp = log(pick_overall))
  fit <- stats::lm(ls ~ splines::ns(lp, df = 8), data = s)
  list(fit = fit, f = function(pick) exp(stats::predict(fit, newdata = data.frame(lp = log(pick)))),
       r2 = summary(fit)$r.squared, sd = summary(fit)$sigma)
}

price_data <- function(r, p, sc) {
  idx <- p |> dplyr::distinct(year, sched_index)
  r |> dplyr::filter(taken_r10, signed %in% TRUE, !is.na(bonus), !is.na(slot)) |>
    dplyr::left_join(idx, by = "year") |>
    dplyr::mutate(lb = log(bonus / sched_index), ls_pick = log(sc$f(pick_overall)), ls_rank = log(sc$f(rank)),
                  gap = log(pick_overall) - log(rank))
}

price_stan_data <- function(pd) {
  list(N = nrow(pd), lb = pd$lb, ls_pick = pd$ls_pick - mean(pd$ls_pick), ls_rank = pd$ls_rank - mean(pd$ls_pick), G = 6L, g = as.integer(pd$type),
       Y = 14L, yr = pd$year - 2011L, sr = as.numeric(pd$senior_nominal %in% TRUE), ls_mean = mean(pd$ls_pick))
}

# ---- 5.3 class depth -------------------------------------------------------------------------------------------
class_depth <- function(p, r, sc) {
  idx <- p |> dplyr::distinct(year, sched_index)
  b <- p |> dplyr::filter(signed %in% TRUE, !is.na(bonus)) |> dplyr::left_join(idx |> dplyr::rename(si = sched_index), by = "year") |>
    dplyr::mutate(b25 = bonus / si) |>
    dplyr::group_by(year) |> dplyr::summarise(
      n_signed_1m = sum(b25 >= 1e6), n_signed_2m = sum(b25 >= 2e6), dollars_over_1m = sum(pmax(b25 - 1e6, 0)) / 1e6,
      total_r1_10 = sum(b25[pool_round %in% TRUE]) / 1e6,
      over_slot_dollars = sum(pmax(bonus - slot, 0)[pool_round %in% TRUE], na.rm = TRUE) / sum(slot[pool_round %in% TRUE & !is.na(bonus)], na.rm = TRUE),
      hs_over_slot_r3_10 = sum(pool_round %in% TRUE & school_type == "HS" & bonus > 1.015 * slot & !(round_group(round_label) %in% c("R1+C", "R2-CBB")), na.rm = TRUE),
      .groups = "drop")
  rr <- r |> dplyr::group_by(year) |> dplyr::summarise(hs_top50 = sum(rank <= 50 & grepl("^HS", type)), hs_top100 = sum(rank <= 100 & grepl("^HS", type)),
    pitchers_top50 = sum(rank <= 50 & grepl("-P$", type)), top100_not_in_r10 = sum(rank <= 100 & !taken_r10), .groups = "drop")
  dplyr::left_join(b, rr, by = "year")
}
