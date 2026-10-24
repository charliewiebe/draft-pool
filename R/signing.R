# Stage 6: signing risk.
# Descriptive model: P(unsigned) for drafted players by round group, school, pre-draft standing, eligibility, region, age.
# Decision-point model: P(fail | team targets ratio x at a pick in round group r, school s) = logit^-1(alpha_rs + beta * log(max(x, 1)))
#   Offers to unsigned players are not observed, so beta is NOT identified; it is an assumption (grid 0, 1, 2; default 1).
#   alpha_rs is calibrated so that, over the historical distribution of signed bonus ratios in the cell, the implied failure
#   rate equals the posterior unsigned rate for the cell (new draft year).

sign_round_groups <- function() c("R1+C", "R2-CBB", "R3-5", "R6-10", "R11-12", "R13-20")
sign_rg <- function(rl) {
  g <- round_group(rl); n <- suppressWarnings(as.integer(sub("^R", "", g)))
  dplyr::case_when(g %in% c("R1+C", "R2-CBB") ~ g, n %in% 3:5 ~ "R3-5", n %in% 6:10 ~ "R6-10", n %in% 11:12 ~ "R11-12",
                   n %in% 13:20 ~ "R13-20", TRUE ~ NA_character_)
}

signing_data <- function(p, ranked, min_year = 2014L) {
  rk <- ranked |> dplyr::filter(rank <= 200) |> dplyr::select(year, mlbam_id, rank)
  d <- p |> dplyr::filter(year >= min_year, year != 2020L, !is.na(school_type)) |>
    dplyr::left_join(rk, by = c("year", "mlbam_id")) |>
    dplyr::mutate(rg = sign_rg(round_label), unsigned = !(signed %in% TRUE),
                  fall = ifelse(is.na(rank), 0, pmax(0, log(pick_overall / rank))), unr = as.numeric(is.na(rank)),
                  so = as.numeric(school_type == "4yr" & college_class %in% "SO"), cold = as.numeric(cold_weather %in% TRUE)) |>
    dplyr::filter(!is.na(rg))
  d |> dplyr::group_by(school_type) |> dplyr::mutate(age_c = dplyr::coalesce(age_at_draft - mean(age_at_draft, na.rm = TRUE), 0)) |> dplyr::ungroup()
}

signing_stan_data <- function(d) {
  list(N = nrow(d), y = as.integer(d$unsigned), R = length(sign_round_groups()), rg = match(d$rg, sign_round_groups()),
       S = 3L, sc = match(d$school_type, school_levels()), fall = d$fall, unr = d$unr, so = d$so, cold = d$cold, age_c = d$age_c,
       Y = 14L, yr = d$year - 2011L)
}

# Posterior unsigned rate per (round group, school) for a new draft year, average covariates of the cell
signing_cell_rates <- function(fit, d, n_draws = 1000L) {
  dr <- posterior::as_draws_df(fit$draws); dr <- dr[round(seq(1, nrow(dr), length.out = n_draws)), ]
  g <- function(nm) as.numeric(dr[[nm]])
  cells <- d |> dplyr::group_by(rg, school_type) |>
    dplyr::summarise(n = dplyr::n(), unsigned_obs = mean(unsigned), fall = mean(fall), unr = mean(unr), so = mean(so), cold = mean(cold),
                     age_c = mean(age_c), .groups = "drop")
  out <- lapply(seq_len(nrow(cells)), function(i) {
    r <- match(cells$rg[i], sign_round_groups()); s <- match(cells$school_type[i], school_levels())
    eta <- g(sprintf("a_r[%d]", r)) + g(sprintf("a_s[%d]", s)) + g("sd_rs") * g(sprintf("z_rs[%d,%d]", r, s)) + g("b_fall") * cells$fall[i] +
      g("b_unr") * cells$unr[i] + g("b_so") * cells$so[i] + g("b_cold") * cells$cold[i] + g(sprintf("b_age[%d]", s)) * cells$age_c[i] +
      g("sd_y") * stats::rnorm(nrow(dr))
    pr <- stats::plogis(eta)
    tibble::tibble(rate = mean(pr), lo = stats::quantile(pr, 0.05), hi = stats::quantile(pr, 0.95))
  })
  dplyr::bind_cols(cells, dplyr::bind_rows(out))
}

signing_effects <- function(fit) {
  fit$summary |> dplyr::filter(variable %in% c("b_fall", "b_unr", "b_so", "b_cold", "sd_y", "sd_rs") | grepl("^b_age|^a_s\\[", variable)) |>
    dplyr::mutate(odds_ratio = exp(mean))
}

# Calibrate alpha per (round group, school) for each beta. Ratio samples: signed rounds 1-10 deals in the cell (deal data);
# rounds 11+: ratio to the rounds 11+ threshold.
calibrate_fail_model <- function(rates, p, deals_d, betas = c(0, 1, 2, 3)) {
  r10 <- deals_d |> dplyr::mutate(rg = sign_rg(round_label)) |> dplyr::select(rg, school_type, ratio)
  r11 <- p |> dplyr::filter(year >= 2014, year != 2020, !(pool_round %in% TRUE), signed %in% TRUE, !is.na(bonus), !is.na(r11_threshold)) |>
    dplyr::mutate(rg = sign_rg(round_label), ratio = bonus / r11_threshold) |> dplyr::filter(!is.na(rg)) |> dplyr::select(rg, school_type, ratio)
  samp <- dplyr::bind_rows(r10, r11) |> dplyr::filter(!is.na(rg), !is.na(ratio), !is.na(school_type))
  out <- list()
  for (b in betas) for (i in seq_len(nrow(rates))) {
    x <- samp$ratio[samp$rg == rates$rg[i] & samp$school_type == rates$school_type[i]]
    if (length(x) < 5) x <- samp$ratio[samp$rg == rates$rg[i]]
    lx <- log(pmax(x, 1))
    f <- function(a) mean(stats::plogis(a + b * lx)) - rates$rate[i]
    a <- stats::uniroot(f, c(-15, 5))$root
    out[[length(out) + 1]] <- tibble::tibble(rg = rates$rg[i], school_type = rates$school_type[i], beta = b, alpha = a,
      target_rate = rates$rate[i], p_fail_at_slot = stats::plogis(a), p_fail_1_5x = stats::plogis(a + b * log(1.5)), p_fail_2x = stats::plogis(a + b * log(2)),
      p_fail_3x = stats::plogis(a + b * log(3)))
  }
  dplyr::bind_rows(out)
}

p_fail <- function(fm, rg, school, ratio, beta = 1) {
  k <- fm[fm$beta == beta, ]
  a <- k$alpha[match(paste(rg, school), paste(k$rg, k$school_type))]
  a[is.na(a)] <- k$alpha[match(paste(rg[is.na(a)], "4yr"), paste(k$rg, k$school_type))]
  stats::plogis(a + beta * log(pmax(ratio, 1)))
}
