# Stage 4 primitives on a grid of bonus x type x age, for every posterior draw.

grid_cells <- function(basis, sched_2025, bonus_grid = exp(seq(log(1e4), log(1.1e7), length.out = 48)), age_c = c(-1, 0, 1)) {
  cells <- expand.grid(bonus = bonus_grid, type = type_levels(), age_c = age_c, stringsAsFactors = FALSE)
  cells$g <- match(cells$type, type_levels())
  cells$s <- match(sub("-.*", "", cells$type), school_levels())
  cells$x <- log((cells$bonus + 1000) / (sched_2025 * 7.2e6))
  cells$cell <- seq_len(nrow(cells))
  list(cells = cells, B = spline_basis(cells$x, knots = basis$knots, boundary = basis$boundary)$B)
}

tail_probs <- function(hl, ks = c(5, 10), n_gh = 8) {
  gh <- gh_nodes(n_gh)
  pp <- 0; for (a in seq_along(gh$z)) pp <- pp + gh$w[a] * plogis(hl$eta_p + hl$sd_p * gh$z[a])
  lapply(stats::setNames(ks, paste0("p_gt", ks)), function(k) {
    tt <- 0; for (a in seq_along(gh$z)) tt <- tt + gh$w[a] * plnorm(k, hl$mu + hl$sd_m * gh$z[a], hl$sigma, lower.tail = FALSE)
    pp * tt
  })
}

compute_grid <- function(fit_arr, fit_val, gc, D1, vgrid, chunk = 100L, seed = 4L, org_fixed_arr = NULL, org_fixed_val = NULL,
                         vp = valuation_params()) {
  set.seed(seed)
  cl <- gc$cells; R <- nrow(cl); nd <- min(nrow(fit_arr$draws), nrow(fit_val$draws))
  combos <- expand.grid(variant = vp$variants, r = vp$r, stringsAsFactors = FALSE)
  res <- list(); pred <- list()
  for (st in seq(1, nd, by = chunk)) {
    ii <- st:min(nd, st + chunk - 1); D <- length(ii)
    da <- fit_arr$draws[ii, ]; dv <- fit_val$draws[ii, ]
    lp <- arrival_lp(da, gc$B, cl$g, cl$s, cl$age_c, cl$x)
    ap <- arrival_predict(da, lp, org_fixed = if (is.null(org_fixed_arr)) NULL else org_fixed_arr[ii])
    hl <- hurdle_lp(dv, gc$B, cl$g, cl$s, cl$age_c)
    if (!is.null(org_fixed_val)) { hl$eta_p <- hl$eta_p + org_fixed_val$p[ii]; hl$mu <- hl$mu + org_fixed_val$m[ii]
                                   hl$sd_p <- dv$sy_p; hl$sd_m <- dv$sy_m }
    hm <- hurdle_mass(hl, vgrid)                                    # D x R x V
    tp <- tail_probs(hl)
    Tn <- dim(ap$p_t)[3]
    M <- matrix(hm, D * R, length(vgrid))                           # rows: draw-major within cell? (D varies fastest)
    PT <- matrix(ap$p_t, D * R, Tn)
    ev_arr <- as.vector(M %*% vgrid)
    out <- data.frame(draw = rep(ii, times = R), cell = rep(cl$cell, each = D),
                      p_arr = as.vector(ap$p_arr), ev_war = as.vector(ap$p_arr) * ev_arr,
                      ev_war_given_arr = ev_arr,
                      p_gt5 = as.vector(ap$p_arr * tp$p_gt5), p_gt10 = as.vector(ap$p_arr * tp$p_gt10))
    for (ci in seq_len(nrow(combos))) {
      vt <- M %*% D1[, , combos$variant[ci], as.character(combos$r[ci])]   # (D*R) x Tn : E[surplus per $1 of $/WAR | arrive, t]
      out[[sprintf("e1_%s_r%02d", combos$variant[ci], round(100 * combos$r[ci]))]] <- rowSums(vt * PT)
    }
    res[[length(res) + 1]] <- out
    # one predictive realization per draw x cell
    arrive <- stats::runif(D * R) < out$p_arr
    draw_cat <- function(P) { cs <- P %*% upper.tri(diag(ncol(P)), diag = TRUE); 1L + rowSums(cs < stats::runif(nrow(P)) * cs[, ncol(P)]) }
    td <- pmin(draw_cat(PT), Tn) - 1L
    vi <- pmin(draw_cat(M), length(vgrid))
    vwar <- ifelse(arrive, vgrid[vi], 0)
    vdol <- ifelse(arrive, D1[cbind(vi, td + 1L, 1L, which(vp$r == vp$r_default))] * vp$dpw_default, 0)
    pred[[length(pred) + 1]] <- data.frame(draw = out$draw, cell = out$cell, arrive = arrive, t_debut = ifelse(arrive, td, NA_integer_),
                                           v_war = vwar, v_dollar = vdol)
  }
  list(expect = dplyr::bind_rows(res), predictive = dplyr::bind_rows(pred), cells = cl, combos = combos)
}

# Summaries: expected surplus, marginal surplus per dollar, return on bonus, curvature, tails (default valuation)
summarise_grid <- function(gr, vp = valuation_params(), variant = "a", r = vp$r_default, dpw = vp$dpw_default) {
  col <- sprintf("e1_%s_r%02d", variant, round(100 * r))
  e <- dplyr::left_join(gr$expect, gr$cells[, c("cell", "bonus", "type", "age_c")], by = "cell")
  e$ev_dollar <- e[[col]] * dpw; e$e_surplus <- e$ev_dollar - e$bonus; e$return <- e$ev_dollar / e$bonus
  e <- e |> dplyr::arrange(draw, type, age_c, bonus) |> dplyr::group_by(draw, type, age_c) |>
    dplyr::mutate(dS_dB = (dplyr::lead(e_surplus) - dplyr::lag(e_surplus)) / (dplyr::lead(bonus) - dplyr::lag(bonus)),
                  dV_dB = (dplyr::lead(ev_dollar) - dplyr::lag(ev_dollar)) / (dplyr::lead(bonus) - dplyr::lag(bonus)),
                  d2V = (dplyr::lead(ev_dollar) - ev_dollar) / (dplyr::lead(bonus) - bonus) - (ev_dollar - dplyr::lag(ev_dollar)) / (bonus - dplyr::lag(bonus))) |>
    dplyr::ungroup()
  q <- function(x, p) stats::quantile(x, p, na.rm = TRUE)
  e |> dplyr::group_by(type, age_c, bonus) |> dplyr::summarise(
    p_arr_lo = q(p_arr, .05), p_arr_hi = q(p_arr, .95), p_arr = mean(p_arr),
    ev_war_lo = q(ev_war, .05), ev_war_hi = q(ev_war, .95), ev_war = mean(ev_war),
    ev_war_given_arr = mean(ev_war_given_arr), p_gt5 = mean(p_gt5), p_gt10 = mean(p_gt10),
    ev_dollar_lo = q(ev_dollar, .05), ev_dollar_hi = q(ev_dollar, .95), ev_dollar = mean(ev_dollar),
    e_surplus_lo = q(e_surplus, .05), e_surplus_hi = q(e_surplus, .95), e_surplus = mean(e_surplus),
    return_lo = q(return, .05), return_hi = q(return, .95), return = mean(return),
    dS_dB_lo = q(dS_dB, .05), dS_dB_hi = q(dS_dB, .95), dS_dB = mean(dS_dB, na.rm = TRUE),
    dV_dB = mean(dV_dB, na.rm = TRUE), p_convex = mean(d2V > 0, na.rm = TRUE), .groups = "drop")
}

# ---- ordinal value model (LOO-preferred) ----------------------------------------------------
ordinal_lp <- function(dr, B, g, s, age_c, G = 6L, td = NULL) {
  K <- ncol(B); e <- dmat(dr, "c") %*% t(B); dd <- darr(dr, "d", G, K)
  for (gg in unique(g)) { j <- which(g == gg); e[, j] <- e[, j] + dd[, gg, ] %*% t(B[j, , drop = FALSE]) }
  e <- e + dmat(dr, "ba")[, s, drop = FALSE] * matrix(age_c, nrow(dr), length(age_c), byrow = TRUE) + td_term(dr, "b_td", "b_td2", s, td)
  if ("a_g[1]" %in% names(dr)) e <- e + dmat(dr, "a_g")[, g, drop = FALSE]
  list(eta = e, cut = dmat(dr, "cut"), sd = sqrt(dr$sy^2 + dr$so^2), is_p = g %% 2 == 1, dtop_p = dsc(dr, "dtop_p"))
}

# role-specific cutpoints: pitchers' top cutpoint shifted by dtop_p
ord_probs_role <- function(eta, cut, is_p, dtop_p) {
  pr <- array(0, c(nrow(eta), ncol(eta), 5))
  if (any(!is_p)) pr[, !is_p, ] <- ord_probs(eta[, !is_p, drop = FALSE], cut)
  if (any(is_p)) { cp <- cut; cp[, 4] <- cp[, 4] + dtop_p; pr[, is_p, ] <- ord_probs(eta[, is_p, drop = FALSE], cp) }
  pr
}

ordinal_tier_probs <- function(ol, n_gh = 10) {
  gh <- gh_nodes(n_gh); pr <- 0
  for (a in seq_along(gh$z)) pr <- pr + gh$w[a] * ord_probs_role(ol$eta + ol$sd * gh$z[a], ol$cut, ol$is_p, ol$dtop_p)
  pr
}

compute_grid_ord <- function(fit_arr, fit_ord, gc, tv, chunk = 100L, seed = 4L, org_fixed_arr = NULL, org_fixed_val = NULL,
                             vp = valuation_params()) {
  set.seed(seed)
  cl <- gc$cells; R <- nrow(cl); nd <- min(nrow(fit_arr$draws), nrow(fit_ord$draws))
  combos <- expand.grid(variant = vp$variants, r = vp$r, stringsAsFactors = FALSE)
  donors_by_tier <- split(seq_along(tv$tier), factor(tv$tier, 1:5))
  ri0 <- which(vp$r == vp$r_default)
  res <- list(); pred <- list()
  for (st in seq(1, nd, by = chunk)) {
    ii <- st:min(nd, st + chunk - 1); D <- length(ii)
    da <- fit_arr$draws[ii, ]; dv <- fit_ord$draws[ii, ]
    of <- if (is.null(org_fixed_arr)) NULL else org_fixed_arr[ii]
    ap <- predict_arrival(da, gc$B, cl$g, cl$s, cl$age_c, cl$x, org_fixed = of)
    Tn <- dim(ap$p_t)[3]
    PT <- matrix(ap$p_t, D * R, Tn); pa <- as.vector(ap$p_arr)
    w_t <- PT / pmax(rowSums(PT), 1e-12)                            # debut-timing distribution given arrival
    TPt <- array(0, c(D * R, 5, Tn))                                 # tier probs given arrival at t
    for (t in seq_len(Tn)) {
      ol <- ordinal_lp(dv, gc$B, cl$g, cl$s, cl$age_c, td = rep(t - 1, R))
      if (!is.null(org_fixed_val)) { ol$eta <- ol$eta + org_fixed_val[ii]; ol$sd <- dv$sy }
      TPt[, , t] <- matrix(ordinal_tier_probs(ol), D * R, 5)
    }
    TP <- matrix(0, D * R, 5); for (t in seq_len(Tn)) TP <- TP + w_t[, t] * TPt[, , t]   # tier probs given arrival
    ev_arr <- as.vector(TP %*% tv$tier_mean_v)
    out <- data.frame(draw = rep(ii, times = R), cell = rep(cl$cell, each = D), p_arr = pa, ev_war = pa * ev_arr,
                      ev_war_given_arr = ev_arr, p_gt5 = pa * (TP[, 4] + TP[, 5]), p_gt10 = pa * TP[, 5],
                      p_le0_given_arr = TP[, 1])
    for (ci in seq_len(nrow(combos))) {
      E1 <- tv$tier_E1[, , combos$variant[ci], as.character(combos$r[ci])]          # 5 x Tn
      acc <- 0; for (t in seq_len(Tn)) acc <- acc + PT[, t] * as.vector(TPt[, , t] %*% E1[, t])
      out[[sprintf("e1_%s_r%02d", combos$variant[ci], round(100 * combos$r[ci]))]] <- acc
    }
    res[[length(res) + 1]] <- out
    draw_cat <- function(P) { cs <- P %*% upper.tri(diag(ncol(P)), diag = TRUE); 1L + rowSums(cs < stats::runif(nrow(P)) * cs[, ncol(P)]) }
    arrive <- stats::runif(D * R) < pa
    td <- pmin(draw_cat(PT), Tn) - 1L
    tier <- pmin(draw_cat(t(vapply(seq_len(D * R), function(k) TPt[k, , td[k] + 1L], numeric(5)))), 5L)
    donor <- vapply(tier, function(k) { d <- donors_by_tier[[k]]; d[sample.int(length(d), 1)] }, integer(1))
    pred[[length(pred) + 1]] <- data.frame(draw = out$draw, cell = out$cell, arrive = arrive, t_debut = ifelse(arrive, td, NA_integer_),
      v_war = ifelse(arrive, tv$v[donor], 0),
      v_dollar = ifelse(arrive, tv$donor_pv[cbind(donor, td + 1L, 1L, ri0)] * vp$dpw_default, 0))
  }
  list(expect = dplyr::bind_rows(res), predictive = dplyr::bind_rows(pred), cells = cl, combos = combos)
}
