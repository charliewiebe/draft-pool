# Stage 4 post-processing: parameter extraction, predictions, prior/posterior predictive checks,
# out-of-time validation, organization effects.

# ---- parameter extraction ------------------------------------------------------------------
dmat <- function(dr, name) {                    # draws x index matrix for vector parameter `name`
  cols <- grep(paste0("^", name, "\\[\\d+\\]$"), names(dr), value = TRUE)
  idx <- as.integer(sub(".*\\[(\\d+)\\]$", "\\1", cols))
  as.matrix(dr[, cols[order(idx)], drop = FALSE])
}
darr <- function(dr, name, G, K) {              # draws x G x K array for matrix parameter `name[g,k]`
  a <- array(NA_real_, c(nrow(dr), G, K))
  for (gg in 1:G) for (k in 1:K) a[, gg, k] <- dr[[sprintf("%s[%d,%d]", name, gg, k)]]
  a
}
dsc <- function(dr, name) if (name %in% names(dr)) dr[[name]] else rep(0, nrow(dr))

gh_nodes <- function(n = 10) {                  # Gauss-Hermite for E[f(Z)], Z ~ N(0,1)
  i <- seq_len(n - 1); J <- matrix(0, n, n); J[cbind(i, i + 1)] <- sqrt(i / 2); J[cbind(i + 1, i)] <- sqrt(i / 2)
  e <- eigen(J, symmetric = TRUE)
  list(z = sqrt(2) * e$values, w = e$vectors[1, ]^2)
}

# Arrival linear predictors for rows of (B, g, s, age_c). Returns draws x rows matrices (no RE / u).
arrival_lp <- function(dr, B, g, s, age_c, x, G = 6L) {
  K <- ncol(B); c <- dmat(dr, "c"); dd <- darr(dr, "d", G, K)
  ba <- dmat(dr, "beta_age"); blg <- dmat(dr, "b_lat_g"); bla <- dmat(dr, "b_lat_age")
  eta <- c %*% t(B)
  for (gg in unique(g)) { j <- which(g == gg); eta[, j] <- eta[, j] + dd[, gg, ] %*% t(B[j, , drop = FALSE]) }
  eta <- eta + ba[, s, drop = FALSE] * matrix(age_c, nrow(dr), length(age_c), byrow = TRUE)
  lat <- outer(dr$b_lat_x, x) + blg[, g, drop = FALSE] + bla[, s, drop = FALSE] * matrix(age_c, nrow(dr), length(age_c), byrow = TRUE)
  list(eta = eta, lat = lat, alpha = dmat(dr, "alpha"))
}

# Per-draw, per-row: P(ever arrive) and P(debut at t), t = 0..Tmax, integrating draft-year/org REs and u
# (re_fixed: optional draws x rows matrix of fixed RE added to eta instead of integrating the org RE).
arrival_predict <- function(dr, lp, n_gh = 8, org_fixed = NULL) {
  gh <- gh_nodes(n_gh); D <- nrow(lp$eta); R <- ncol(lp$eta); Tn <- ncol(lp$alpha)
  sd_org <- if (is.null(org_fixed)) dr$sigma_org else 0
  sd_c <- sqrt(dr$sigma_yr^2 + sd_org^2); lam_i <- dsc(dr, "lambda_inc"); lam_l <- dsc(dr, "lambda_lat")
  base <- lp$eta + if (is.null(org_fixed)) 0 else org_fixed
  p_arr <- matrix(0, D, R); pt <- array(0, c(D, R, Tn))
  for (a in seq_along(gh$z)) for (b in seq_along(gh$z)) {
    w <- gh$w[a] * gh$w[b]
    p <- plogis(base + sd_c * gh$z[a] + lam_i * gh$z[b])
    surv <- matrix(1, D, R)
    for (t in seq_len(Tn)) {
      h <- plogis(lp$alpha[, t] + lp$lat + lam_l * gh$z[b])
      pt[, , t] <- pt[, , t] + w * p * surv * h
      surv <- surv * (1 - h)
    }
    p_arr <- p_arr + w * p
  }
  list(p_arr = p_arr, p_t = pt)
}

# Hurdle value predictors (no RE): draws x rows
# td: seasons from draft to debut for each row (NULL = term omitted, e.g. prior draws)
td_term <- function(dr, lin, quad, s, td) {
  if (is.null(td) || !(paste0(lin, "[1]") %in% names(dr))) return(0)
  tc <- matrix(td - td_center(), nrow(dr), length(td), byrow = TRUE)
  dmat(dr, lin)[, s, drop = FALSE] * tc + dr[[quad]] * tc^2
}

hurdle_lp <- function(dr, B, g, s, age_c, G = 6L, td = NULL) {
  K <- ncol(B); f <- function(cn, dn, an) {
    e <- dmat(dr, cn) %*% t(B); dd <- darr(dr, dn, G, K)
    for (gg in unique(g)) { j <- which(g == gg); e[, j] <- e[, j] + dd[, gg, ] %*% t(B[j, , drop = FALSE]) }
    e + dmat(dr, an)[, s, drop = FALSE] * matrix(age_c, nrow(dr), length(age_c), byrow = TRUE)
  }
  ag <- function(nm) if (paste0(nm, "[1]") %in% names(dr)) dmat(dr, nm)[, g, drop = FALSE] else 0
  list(eta_p = f("cp", "dp", "ba_p") + td_term(dr, "bt_p", "bt2_p", s, td) + ag("a_p"),
       mu = f("cm", "dm", "ba_m") + td_term(dr, "bt_m", "bt2_m", s, td) + ag("a_m"),
       sigma = dmat(dr, "sigma")[, g, drop = FALSE], lambda_neg = dr$lambda_neg,
       sd_p = sqrt(dr$sy_p^2 + dr$so_p^2), sd_m = sqrt(dr$sy_m^2 + dr$so_m^2))
}

# V_WAR | arrival: probability mass on a v grid (bin centers), integrating REs. Returns draws x rows x bins.
hurdle_mass <- function(hl, vgrid, n_gh = 8) {
  gh <- gh_nodes(n_gh); D <- nrow(hl$eta_p); R <- ncol(hl$eta_p); step <- vgrid[2] - vgrid[1]
  edges <- c(vgrid - step / 2, max(vgrid) + step / 2)
  pp <- matrix(0, D, R); for (a in seq_along(gh$z)) pp <- pp + gh$w[a] * plogis(hl$eta_p + hl$sd_p * gh$z[a])
  pos <- which(vgrid > 0); neg <- which(vgrid <= 0)
  out <- array(0, c(D, R, length(vgrid)))
  # negative part: V = -E, E ~ exponential(lambda_neg); bin [lo, hi] gets P(E in [max(-hi, 0), -lo]); lowest bin takes the tail
  lam <- hl$lambda_neg
  for (k in neg) {
    lo <- edges[k]; hi <- edges[k + 1]
    upper <- if (k == min(neg)) 1 else 1 - exp(-lam * (-lo))
    m <- upper - (1 - exp(-lam * max(-hi, 0)))
    out[, , k] <- (1 - pp) * m
  }
  # positive part: lognormal, integrate mu RE by GH; first positive bin takes (0, its upper edge]; top bin takes the tail
  for (a in seq_along(gh$z)) {
    mu <- hl$mu + hl$sd_m * gh$z[a]
    prev <- 0
    for (k in pos) {
      cur <- if (k == max(pos)) 1 else plnorm(edges[k + 1], mu, hl$sigma)
      out[, , k] <- out[, , k] + gh$w[a] * pp * (cur - prev)
      prev <- cur
    }
  }
  out
}

# Marginal arrival prediction from arrival_cond.stan:
#   P(debut at t | x) = sum_{l, top} P(l | x) P(top | l, x) P(debut at t | l, top, x), with debut by h for l = MLB.
# Draft-year and organization REs on incidence are integrated by GH (or org fixed via org_fixed, draws vector).
arrival_cond_predict <- function(dr, B, g, s, age_c, x, h = 2L, L = 9L, Tn = 15L, G = 6L, n_gh = 8, org_fixed = NULL) {
  D <- nrow(dr); R <- nrow(B); K <- ncol(B); A <- matrix(age_c, D, R, byrow = TRUE); X <- matrix(x, D, R, byrow = TRUE)
  # I model
  vl <- dmat(dr, "cl") %*% t(B) + dmat(dr, "l_g")[, g, drop = FALSE] + dmat(dr, "l_age")[, s, drop = FALSE] * A
  pl <- ord_probs_L(vl, dmat(dr, "cut"))                         # D x R x L
  tl <- dmat(dr, "t_lvl")
  # incidence / latency linear predictors without I terms
  c <- dmat(dr, "c"); dd <- darr(dr, "d", G, K)
  eta <- c %*% t(B)
  for (gg in unique(g)) { j <- which(g == gg); eta[, j] <- eta[, j] + dd[, gg, ] %*% t(B[j, , drop = FALSE]) }
  eta <- eta + dmat(dr, "beta_age")[, s, drop = FALSE] * A + (if (is.null(org_fixed)) 0 else org_fixed)
  lat <- dr$b_lat_x * X + dmat(dr, "b_lat_g")[, g, drop = FALSE] + dmat(dr, "b_lat_age")[, s, drop = FALSE] * A
  alpha <- dmat(dr, "alpha"); gl <- dmat(dr, "gam_lvl"); dl <- dmat(dr, "del_lvl")
  sd_c <- sqrt(dr$sigma_yr^2 + (if (is.null(org_fixed)) dr$sigma_org^2 else 0)); gh <- gh_nodes(n_gh)
  p_arr <- matrix(0, D, R); p_t <- array(0, c(D, R, Tn))
  # MLB by h: timing ~ softmax(pi0 + pi_s[s])
  pi0 <- dmat(dr, "pi0"); ps <- lapply(1:3, function(k) sapply(1:(h + 1), function(t) dr[[sprintf("pi_s[%d,%d]", k, t)]]))
  for (j in seq_len(R)) {
    lg <- pi0 + ps[[s[j]]]; w <- exp(lg - apply(lg, 1, max)); w <- w / rowSums(w)
    p_t[, j, 1:(h + 1)] <- pl[, j, L] * w
  }
  p_arr <- p_arr + pl[, , L]
  for (l in 1:(L - 1)) for (top in 0:1) {
    ptop <- plogis(dr$t0 + dr$t_x * X + dmat(dr, "t_g")[, g, drop = FALSE] + tl[, l])
    wI <- pl[, , l] * (if (top == 1) ptop else 1 - ptop)
    inc <- 0; for (a in seq_along(gh$z)) inc <- inc + gh$w[a] * plogis(eta + gl[, l] + dr$gam_top * top + sd_c * gh$z[a])
    la <- lat + dl[, l] + dr$del_top * top
    surv <- 1
    for (t in (h + 2):Tn) { hz <- plogis(alpha[, t] + la); p_t[, , t] <- p_t[, , t] + wI * inc * surv * hz; surv <- surv * (1 - hz) }
    p_arr <- p_arr + wI * inc
  }
  list(p_arr = p_arr, p_t = p_t)
}

ord_probs_L <- function(eta, cut) {           # eta D x R, cut D x (L-1) -> D x R x L
  Lm <- ncol(cut); D <- nrow(eta); R <- ncol(eta); pr <- array(0, c(D, R, Lm + 1)); prev <- 0
  for (k in 1:Lm) { cur <- plogis(cut[, k] - eta); pr[, , k] <- cur - prev; prev <- cur }
  pr[, , Lm + 1] <- 1 - prev; pr
}

# Dispatch on the fitted arrival model: conditional joint (arrival_cond.stan) vs cure model (arrival.stan, use_u = 0)
predict_arrival <- function(dr, B, g, s, age_c, x, org_fixed = NULL) {
  if ("gam_lvl[1]" %in% names(dr)) arrival_cond_predict(dr, B, g, s, age_c, x, org_fixed = org_fixed)
  else arrival_predict(dr, arrival_lp(dr, B, g, s, age_c, x), org_fixed = org_fixed)
}
