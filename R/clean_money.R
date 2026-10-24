# Stage 3: money normalization, rounds 11+ censoring, 2020 deferral.

# CPI-U (BLS series CUUR0000SA0, all items, NSA) via the datasets/cpi-us mirror (BLS hosts not reachable here).
fetch_cpi <- function(dest_dir = file.path("data", "raw", "cpi")) {
  if (!dir.exists(file.path(dest_dir, ".git"))) {
    system2("git", c("clone", "--depth", "1", "https://github.com/datasets/cpi-us", dest_dir))
    sha <- system2("git", c("-C", dest_dir, "rev-parse", "HEAD"), stdout = TRUE)
    log_request("https://github.com/datasets/cpi-us", dest_dir, 200, NA, sha, "git clone --depth 1; mirror of BLS CPI-U CUUR0000SA0")
  }
  file.path(dest_dir, "data", "cpiai.csv")
}

cpi_annual <- function(path) {
  d <- utils::read.csv(path)
  d$year <- as.integer(substr(d$Date, 1, 4))
  a <- stats::aggregate(Index ~ year, d, function(x) c(mean = mean(x), n = length(x)))
  data.frame(year = a$year, cpi = a$Index[, "mean"], months = a$Index[, "n"])
}

# Per-year money indices.
#   slot1:        slot of pick 1 (breaks in 2017: CBA cut picks 1-2)
#   league_pool:  sum of published team pools (breaks in 2020: five rounds)
#   sched_index:  fixed-basket schedule index = sum of slots at picks 1..300 / same sum in 2012
#                 (2020 has 160 pool picks; its slots were frozen at 2019 values, so 2020 = 2019 by rule)
year_index <- function(slots, pools, cpi, basket = 300L, base_cpi_year = 2025L) {
  base <- sum(slots$slot[slots$year == 2012 & slots$pick_overall <= basket])
  yi <- slots |> dplyr::group_by(year) |> dplyr::summarise(
    slot1 = slot[pick_overall == 1], n_pool_picks = dplyr::n(),
    basket_sum = if (max(pick_overall) >= basket) sum(slot[pick_overall <= basket]) else NA_real_, .groups = "drop")
  yi$basket_sum[yi$year == 2020] <- yi$basket_sum[yi$year == 2019]
  yi$sched_index <- yi$basket_sum / base
  yi <- dplyr::left_join(yi, pools |> dplyr::group_by(year) |> dplyr::summarise(league_pool = sum(pool_published), .groups = "drop"), by = "year")
  yi <- dplyr::left_join(yi, cpi[, c("year", "cpi")], by = "year")
  yi$cpi_to_base <- cpi$cpi[cpi$year == base_cpi_year] / yi$cpi
  yi$sched_to_2025 <- yi$sched_index[yi$year == 2025] / yi$sched_index
  yi
}

# PV of a 2020 bonus under the mandated deferral: <= $100k up front, remainder 50% on 2021-07-01 and 50% on 2022-07-01.
# Discounted to the 2020 signing date at annual rate r (signing assumed ~2020-07-01).
pv_2020_bonus <- function(b, r) {
  up <- pmin(b, 1e5); rest <- pmax(b - 1e5, 0)
  up + rest / 2 / (1 + r) + rest / 2 / (1 + r)^2
}

normalize_money <- function(p, yi, offsets_usd = c(500, 1000, 5000, 10000), r_default = 0.08) {
  p <- dplyr::left_join(p, yi |> dplyr::select(year, slot1, league_pool, sched_index, cpi_to_base, sched_to_2025), by = "year")
  ref <- 7200000  # 2012 pick-1 slot: bonus_rel units are "2012 No. 1 slot equivalents"
  p <- p |> dplyr::mutate(
    bonus_rel_p1 = bonus / slot1, bonus_rel_pool = bonus / league_pool,
    bonus_rel = bonus / (sched_index * ref),                     # primary
    slot_rel_p1 = slot / slot1, slot_rel_pool = slot / league_pool, slot_rel = slot / (sched_index * ref),
    bonus_2025cpi = bonus * cpi_to_base, bonus_2025sched = bonus * sched_to_2025,
    bonus_pv = ifelse(year == 2020, pv_2020_bonus(bonus, r_default), bonus),
    bonus_rel_pv = bonus_pv / (sched_index * ref))
  for (o in offsets_usd) p[[paste0("log_bonus_rel_o", o)]] <- log((p$bonus + o) / (p$sched_index * ref))
  p
}

# Rounds 11+ censoring representation of the bonus as a value signal.
#   bonus at exactly the threshold -> right-censored at the threshold (value >= threshold)
#   bonus below/above threshold     -> exact
#   signed, bonus missing           -> interval [0, Inf) here; multiply imputed separately
r11_censoring <- function(p) {
  r11 <- !p$pool_round & p$signed %in% TRUE
  p$bonus_cens <- dplyr::case_when(
    !(p$signed %in% TRUE) ~ "not_signed",
    p$pool_round & !is.na(p$bonus) ~ "exact",
    p$pool_round ~ "missing",
    r11 & is.na(p$bonus) ~ "missing",
    r11 & !is.na(p$r11_threshold) & p$bonus == p$r11_threshold ~ "right_at_threshold",
    TRUE ~ "exact")
  p$bonus_lo <- dplyr::case_when(p$bonus_cens %in% c("exact", "right_at_threshold") ~ p$bonus, p$bonus_cens == "missing" ~ 0, TRUE ~ NA_real_)
  p$bonus_hi <- dplyr::case_when(p$bonus_cens == "exact" ~ p$bonus, p$bonus_cens %in% c("right_at_threshold", "missing") ~ Inf, TRUE ~ NA_real_)
  p$senior_nominal <- p$signed %in% TRUE & !is.na(p$bonus) & p$bonus <= 10000
  p
}
