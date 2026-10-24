# Valuation layer (assumptions, not facts). Everything in 2025 dollars for a 2025-draft decision.
#   $/WAR (market, above the league minimum): grid 6.5, 8 (default), 9.5, 11 $M
#     (FanGraphs 2022 put free agency at roughly $6.5M per WAR above minimum; 2024-25 commentary cites $8.9M-$11M)
#   salaries: pre-arbitration seasons 1-3 at the league minimum (no surplus cost beyond replacement);
#     arbitration seasons 4-6 cost phi_k x WAR_k x $/WAR above minimum, phi = (0.20, 0.35, 0.55)
#     (approximate FanGraphs 2022 arbitration-to-free-agency ratios for batters/starters); Super Two ignored
#   discount: real annual rate r grid 0.06, 0.08 (default), 0.10, from the draft date; control season k at t_debut + k - 1
#   variants: a = linear in WAR; b0/b1 = only WAR above 0 / 1 per season counts (roster-slot); d = star premium,
#     +50% per WAR above 4 in a season. (c: concave utility of class totals is applied in the decision model.)
# Per-season paths come from resolved MLB arrivals ("donors"): for a target V_WAR v, the 25 donors with the nearest V
# are used, each donor's played control seasons shifted by (v - V_donor) / n_played.

control_paths <- function(o, seasons) {
  don <- o |> dplyr::filter(debuted, v_war_resolved, year <= value_max_year()) |> dplyr::select(mlbam_id, v_war)
  s <- seasons |> dplyr::filter(mlbam_id %in% don$mlbam_id) |>
    dplyr::mutate(mlb = dplyr::coalesce(mlb_games, 0L) > 0 | dplyr::coalesce(pa, 0) > 0 | dplyr::coalesce(ipouts, 0) > 0) |>
    dplyr::filter(mlb) |> dplyr::arrange(mlbam_id, season) |> dplyr::group_by(mlbam_id) |>
    dplyr::mutate(k = dplyr::row_number()) |> dplyr::filter(k <= 6) |> dplyr::ungroup()
  W <- matrix(0, nrow(don), 6); idx <- match(s$mlbam_id, don$mlbam_id)
  W[cbind(idx, s$k)] <- dplyr::coalesce(s$bwar, 0)
  played <- tabulate(idx, nrow(don))
  list(mlbam_id = don$mlbam_id, v = rowSums(W), W = W, played = pmax(played, 1L))
}

valuation_params <- function() list(dpw = c(6.5e6, 8e6, 9.5e6, 11e6), r = c(0.06, 0.08, 0.10), phi = c(0.20, 0.35, 0.55),
                                    variants = c("a", "b0", "b1", "d"), dpw_default = 8e6, r_default = 0.08)

season_value <- function(w, variant, phi) {     # surplus in WAR-dollar units ($/WAR = 1) for a 6-column season matrix
  w2 <- switch(variant, a = w, b0 = pmax(w, 0), b1 = pmax(w - 1, 0), d = w + 0.5 * pmax(w - 4, 0))
  arb <- matrix(c(0, 0, 0, phi), nrow(w), 6, byrow = TRUE)
  w2 * ifelse(w > 0, 1 - arb, 1)
}

# D1[v, t, variant, r]: expected surplus per $1 of $/WAR given V_WAR = v and debut t seasons after the draft season
value_function <- function(paths, vgrid, Tn = 15L, vp = valuation_params(), knn = 25L) {
  D1 <- array(0, c(length(vgrid), Tn, length(vp$variants), length(vp$r)),
              dimnames = list(NULL, NULL, vp$variants, as.character(vp$r)))
  for (iv in seq_along(vgrid)) {
    nn <- order(abs(paths$v - vgrid[iv]))[seq_len(knn)]
    W <- paths$W[nn, , drop = FALSE]; pl <- paths$played[nn]
    play <- W != 0 | col(W) <= pl
    W <- W + play * ((vgrid[iv] - paths$v[nn]) / pl)
    for (vi in seq_along(vp$variants)) {
      sv <- season_value(W, vp$variants[vi], vp$phi)
      for (ri in seq_along(vp$r)) for (t in 0:(Tn - 1)) {
        disc <- (1 + vp$r[ri])^-(t + 0:5)
        D1[iv, t + 1, vi, ri] <- mean(sv %*% disc)
      }
    }
  }
  D1
}

# Ordinal-model valuation: each resolved MLB arrival's own control-season path, valued for every debut timing t.
# donor_pv[i, t, variant, r]: surplus per $1 of $/WAR; tier_E1[k, t, variant, r]: mean over donors in WAR tier k.
tier_valuation <- function(paths, Tn = 15L, vp = valuation_params()) {
  n <- length(paths$v)
  pv <- array(0, c(n, Tn, length(vp$variants), length(vp$r)), dimnames = list(NULL, NULL, vp$variants, as.character(vp$r)))
  for (vi in seq_along(vp$variants)) {
    sv <- season_value(paths$W, vp$variants[vi], vp$phi)
    for (ri in seq_along(vp$r)) for (t in 0:(Tn - 1)) pv[, t + 1, vi, ri] <- sv %*% ((1 + vp$r[ri])^-(t + 0:5))
  }
  tier <- v_tier(paths$v)
  E1 <- array(0, c(5, Tn, length(vp$variants), length(vp$r)), dimnames = dimnames(pv)[-1] |> (\(d) c(list(NULL), d))())
  for (k in 1:5) E1[k, , , ] <- apply(pv[tier == k, , , , drop = FALSE], 2:4, mean)
  list(donor_pv = pv, tier = tier, v = paths$v, tier_E1 = E1, tier_mean_v = tapply(paths$v, factor(tier, 1:5), mean),
       tier_n = tabulate(tier, 5))
}
