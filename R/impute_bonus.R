# Selection-adjusted multiple imputation of missing bonuses for signed picks.
#
# Rounds 11+, 2012-2016 (B-Ref is the only source and reports selectively by bonus size):
#   true-bonus distribution f(rel | round band, school type): 2017-19 Stats API bonuses (near-complete), in threshold
#     units rel = bonus / r11_threshold so it transfers across the $100k -> $125k change.
#   year-specific reporting probability by bonus band b:  pi_y(b) = reported_y(b) / (N_y * fbar_y(b)), capped at 1,
#     where N_y = signed R11+ picks, fbar_y(b) = average over those picks of their cell's donor share in band b.
#     This matches the reported distribution exactly in every band where the cap does not bind.
#   missing-bonus draws: weighted hot deck from the cell's donors with weight 1 - pi_y(band(rel)).
#   Validation: the same estimator applied to 2017 B-Ref reports (donors 2018-19 only), compared with the Stats API truth.
# Rounds 1-10 and 2017+ R11+ missing (few): MAR hot deck on ratio_to_slot (R1-10) or rel (R11+), same band x type.

rel_bands <- function(rel) cut(rel, c(-Inf, 0.05, 0.25, 0.5, 0.99, 1.01, 2, Inf),
                               labels = c("<5%", "5-25%", "25-50%", "50-99%", "at thr", "1-2x", ">2x"))
round_band <- function(lbl) {
  r <- suppressWarnings(as.integer(lbl))
  cut(r, c(10, 15, 20, 30, 40), labels = c("11-15", "16-20", "21-30", "31-40"))
}

make_donors <- function(p, years, bonus_col = "sa_bonus_pos") {
  p |> dplyr::filter(year %in% years, !pool_round, signed %in% TRUE, !is.na(.data[[bonus_col]])) |>
    dplyr::transmute(rb = round_band(round_label), school_type, rel = .data[[bonus_col]] / r11_threshold) |>
    dplyr::filter(!is.na(rb), !is.na(school_type)) |>
    dplyr::mutate(band = rel_bands(rel))
}

cell_donors <- function(donors, rb, st) {
  d <- donors[donors$rb %in% rb & donors$school_type %in% st, ]
  if (nrow(d) < 20) d <- donors[donors$rb %in% rb, ]
  d
}

# targets: signed R11+ picks for one year with columns rb, school_type, rel_reported (NA if not reported)
estimate_reporting <- function(targets, donors) {
  lv <- levels(donors$band)
  fbar <- Reduce(`+`, lapply(seq_len(nrow(targets)), function(i) {
    d <- cell_donors(donors, targets$rb[i], targets$school_type[i])
    as.numeric(table(factor(d$band, levels = lv))) / nrow(d)
  })) / nrow(targets)
  rep_n <- as.numeric(table(factor(rel_bands(targets$rel_reported[!is.na(targets$rel_reported)]), levels = lv)))
  expected <- nrow(targets) * fbar
  pi_raw <- ifelse(expected > 0, rep_n / expected, 0)
  tibble::tibble(band = factor(lv, levels = lv), fbar = fbar, expected_n = expected, reported_n = rep_n,
                 pi_raw = pi_raw, pi = pmin(pi_raw, 1), capped = pi_raw > 1)
}

draw_missing <- function(targets, donors, rep_tbl, M) {
  miss <- which(is.na(targets$rel_reported))
  dplyr::bind_rows(lapply(miss, function(i) {
    d <- cell_donors(donors, targets$rb[i], targets$school_type[i])
    w <- 1 - rep_tbl$pi[match(as.character(d$band), as.character(rep_tbl$band))]
    if (sum(w) <= 0) w <- rep(1, nrow(d))
    tibble::tibble(idx = i, m = seq_len(M), rel_imp = d$rel[sample.int(nrow(d), M, replace = TRUE, prob = w)])
  }))
}

impute_bonuses <- function(p, M = 20L, seed = 20261020L) {
  set.seed(seed)
  donors <- make_donors(p, 2017:2019)
  base <- p |> dplyr::filter(signed %in% TRUE, !pool_round) |> dplyr::mutate(rb = round_band(round_label)) |>
    dplyr::filter(!is.na(rb))

  # ---- validation on 2017: B-Ref reporting was selective; Stats API gives the truth ----
  t17 <- base |> dplyr::filter(year == 2017, !is.na(sa_bonus_pos), !is.na(school_type)) |>
    dplyr::mutate(rel_reported = br_bonus / r11_threshold, rel_true = sa_bonus_pos / r11_threshold)
  d1819 <- make_donors(p, 2018:2019)
  rep17 <- estimate_reporting(t17, d1819)
  dr17 <- draw_missing(t17, d1819, rep17, M)
  mar17 <- dplyr::bind_rows(lapply(which(is.na(t17$rel_reported)), function(i) {
    d <- cell_donors(d1819, t17$rb[i], t17$school_type[i]); tibble::tibble(idx = i, rel_imp = sample(d$rel, M, replace = TRUE)) }))
  summ <- function(x, label) tibble::tibble(set = label, n = length(x), median_rel = stats::median(x), mean_rel = mean(x),
    share_lt_0.05 = mean(x < 0.05), share_below_thr = mean(x < 0.99), share_at_thr = mean(abs(x - 1) <= 0.01), share_above_thr = mean(x > 1.01))
  validation_2017 <- dplyr::bind_rows(
    summ(t17$rel_true[is.na(t17$rel_reported)], "truth: Stats API bonus of picks B-Ref did not report"),
    summ(dr17$rel_imp, "selection-adjusted imputation"),
    summ(mar17$rel_imp, "MAR imputation (no selection adjustment)"))

  # ---- 2012-2016 ----
  rep_tables <- list(); out <- list()
  for (y in 2012:2016) {
    t <- base |> dplyr::filter(year == y, !is.na(school_type)) |> dplyr::mutate(rel_reported = bonus / r11_threshold)
    rt <- estimate_reporting(t, donors); rt$year <- y; rep_tables[[length(rep_tables) + 1]] <- rt
    dm <- draw_missing(t, donors, rt, M)
    out[[length(out) + 1]] <- tibble::tibble(year = y, pick_overall = t$pick_overall[dm$idx], m = dm$m,
      bonus_imp = dm$rel_imp * t$r11_threshold[dm$idx], method = "R11+ 2012-16 selection-adjusted hot deck")
  }
  # signed R11+ 2012-16 with unknown school type: cell = round band only
  nst <- base |> dplyr::filter(year <= 2016, is.na(school_type), is.na(bonus))
  for (i in seq_len(nrow(nst))) {
    d <- donors[donors$rb %in% nst$rb[i], ]
    rt <- rep_tables[[which(2012:2016 == nst$year[i])]]
    w <- 1 - rt$pi[match(as.character(d$band), as.character(rt$band))]
    out[[length(out) + 1]] <- tibble::tibble(year = nst$year[i], pick_overall = nst$pick_overall[i], m = seq_len(M),
      bonus_imp = d$rel[sample.int(nrow(d), M, TRUE, prob = w)] * nst$r11_threshold[i], method = "R11+ 2012-16 selection-adjusted hot deck")
  }

  # ---- other missing signed bonuses: MAR hot deck ----
  oth <- p |> dplyr::filter(signed %in% TRUE, is.na(bonus), !(year <= 2016 & !pool_round))
  obs_r10 <- p |> dplyr::filter(pool_round, !is.na(ratio_to_slot), signed %in% TRUE, year != 2020) |>
    dplyr::transmute(rnd = suppressWarnings(as.integer(round_label)), school_type, ratio_to_slot)
  for (i in seq_len(nrow(oth))) {
    if (oth$pool_round[i]) {
      rn <- suppressWarnings(as.integer(oth$round_label[i])); if (is.na(rn)) rn <- 1L
      d <- obs_r10$ratio_to_slot[obs_r10$rnd %in% (rn + -1:1) & obs_r10$school_type %in% oth$school_type[i]]
      v <- sample(d, M, replace = TRUE) * oth$slot[i]; meth <- "R1-10 MAR hot deck on ratio_to_slot"
    } else {
      d <- cell_donors(donors, round_band(oth$round_label[i]), oth$school_type[i])$rel
      v <- sample(d, M, replace = TRUE) * oth$r11_threshold[i]; meth <- "R11+ 2017+ MAR hot deck"
    }
    out[[length(out) + 1]] <- tibble::tibble(year = oth$year[i], pick_overall = oth$pick_overall[i], m = seq_len(M), bonus_imp = v, method = meth)
  }

  list(imputations = dplyr::bind_rows(out), reporting = dplyr::bind_rows(rep_tables),
       reporting_2017_check = rep17, validation_2017 = validation_2017)
}
