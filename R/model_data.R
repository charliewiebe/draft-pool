# Stage 4 model data. One row per signed player (signing pick with an MLBAM id and known type).
# x = log((bonus_pv + $1,000) / (sched_index * $7.2M)); missing bonuses use imputation m.

type_levels <- function() c("HS-P", "HS-H", "4yr-P", "4yr-H", "JUCO-P", "JUCO-H")
school_levels <- function() c("HS", "4yr", "JUCO")
milb_levels <- function() c("none", "ROK", "ROA", "A-", "A", "A+", "AA", "AAA", "MLB")

build_model_data <- function(p, o, prog, imp, m = 1L, offset = 1000, last_season = 2026L, int_horizon = 2L) {
  im <- imp$imputations |> dplyr::filter(m == !!m) |> dplyr::select(year, pick_overall, bonus_imp)
  d <- p |> dplyr::filter(signing_pick, !is.na(mlbam_id)) |>
    dplyr::left_join(im, by = c("year", "pick_overall")) |>
    dplyr::mutate(bonus_use = dplyr::coalesce(bonus_pv, ifelse(year == 2020, pv_2020_bonus(bonus_imp, 0.08), bonus_imp)),
                  bonus_imputed = is.na(bonus_pv) & !is.na(bonus_imp),
                  x = log((bonus_use + offset) / (sched_index * 7.2e6)),
                  type = factor(paste(school_type, role, sep = "-"), levels = type_levels()),
                  school = factor(school_type, levels = school_levels()))
  n0 <- nrow(d)
  d <- dplyr::filter(d, !is.na(x), !is.na(type), !is.na(age_at_draft))
  # age centered within school type
  d <- d |> dplyr::group_by(school) |> dplyr::mutate(age_c = age_at_draft - mean(age_at_draft)) |> dplyr::ungroup()
  d <- dplyr::left_join(d, o |> dplyr::select(mlbam_id, debut_season, debuted, out_of_pro, v_war, v_war_resolved,
                                              v_war_control_obs, control_status, mlb_seasons), by = "mlbam_id")
  # intermediate outcomes at horizon h seasons after the draft season (observable for year + h <= last_season)
  ip <- prog |> dplyr::filter(years_since_draft >= 0, years_since_draft <= int_horizon) |>
    dplyr::mutate(lvl = match(milb_top_level, milb_levels()) , lvl = ifelse(mlb_games > 0, length(milb_levels()), lvl)) |>
    dplyr::group_by(mlbam_id) |>
    dplyr::summarise(top100_h = any(!is.na(top100_rank)), lvl_h = suppressWarnings(max(lvl, na.rm = TRUE)), .groups = "drop") |>
    dplyr::mutate(lvl_h = ifelse(is.finite(lvl_h), lvl_h, 1L))
  d <- dplyr::left_join(d, ip, by = "mlbam_id") |>
    dplyr::mutate(has_int = year + int_horizon <= last_season, top100_h = dplyr::coalesce(top100_h, FALSE),
                  lvl_h = dplyr::coalesce(lvl_h, 1L),
                  cov2020 = year + 1 <= 2020 & year + int_horizon >= 2020)
  d <- d |> dplyr::mutate(
    t_obs = last_season - year,                                  # last observed season index (0 = draft season)
    event = debuted %in% TRUE, t_event = ifelse(event, debut_season - year, NA_integer_),
    resolved_non = !event & out_of_pro %in% TRUE,
    year_idx = year - 2011L, org_idx = as.integer(factor(team_id)),
    group = as.integer(type), school_idx = as.integer(school))
  attr(d, "dropped") <- n0 - nrow(d)
  d
}

spline_basis <- function(x, df = 8L, knots = NULL, boundary = NULL) {
  if (is.null(knots)) {
    knots <- stats::quantile(x, probs = seq(0, 1, length.out = df - 2)[-c(1, df - 2)])
    boundary <- range(x) + c(-0.25, 0.25)
  }
  B <- splines::bs(x, knots = knots, Boundary.knots = boundary, degree = 3, intercept = TRUE)
  list(B = unclass(B)[, , drop = FALSE], knots = knots, boundary = boundary)
}

arrival_stan_data <- function(d, basis, use_u = 1L, prior_only = 0L, save_ll = 0L, Tmax = 14L, Q = 7L) {
  gh <- gh_nodes(Q)
  B <- spline_basis(d$x, knots = basis$knots, boundary = basis$boundary)$B
  list(N = nrow(d), K = ncol(B), G = length(type_levels()), S = 3L, Y = 14L, O = 30L, Tmax = Tmax,
       B = B, x = d$x, g = d$group, s = d$school_idx, age_c = d$age_c, yr = d$year_idx, org = d$org_idx,
       t_obs = pmin(d$t_obs, Tmax), event = as.integer(d$event), t_event = ifelse(d$event, pmin(d$t_event, Tmax), 0L),
       resolved_non = as.integer(d$resolved_non),
       has_int = as.integer(d$has_int), top100 = as.integer(d$top100_h), L = length(milb_levels()), lvl = as.integer(d$lvl_h),
       cov2020 = as.integer(d$cov2020), use_u = use_u, prior_only = prior_only, save_ll = save_ll,
       Q = Q, zq = gh$z, wq = gh$w)
}

v_tier <- function(v) as.integer(cut(v, c(-Inf, 0, 2, 5, 10, Inf)))

value_stan_data <- function(d, basis, prior_only = 0L) {
  a <- d[d$event, ]
  cens <- !(a$v_war_resolved %in% TRUE)
  v <- ifelse(cens, a$v_war_control_obs, a$v_war)
  B <- spline_basis(a$x, knots = basis$knots, boundary = basis$boundary)$B
  list(N = nrow(a), K = ncol(B), G = length(type_levels()), S = 3L, Y = 14L, O = 30L, B = B, g = a$group, s = a$school_idx,
       age_c = a$age_c, yr = a$year_idx, org = a$org_idx, cens = as.integer(cens), v = v, tier = v_tier(v),
       td = a$t_event - td_center(), prior_only = prior_only)
}

# Data for arrival_cond.stan: classes with intermediate outcomes observed at horizon h (draft year + h <= last season).
arrival_cond_stan_data <- function(d, basis, h = 2L, Tmax = 14L, last_season = 2026L) {
  d <- d[d$year + h <= last_season, ]
  B <- spline_basis(d$x, knots = basis$knots, boundary = basis$boundary)$B
  list(N = nrow(d), K = ncol(B), G = length(type_levels()), S = 3L, Y = 14L, O = 30L, Tmax = Tmax, h = h,
       B = B, x = d$x, g = d$group, s = d$school_idx, age_c = d$age_c, yr = d$year_idx, org = d$org_idx,
       t_obs = pmin(d$t_obs, Tmax), event = as.integer(d$event), t_event = ifelse(d$event, pmin(d$t_event, Tmax), 0L),
       resolved_non = as.integer(d$resolved_non), top100 = as.integer(d$top100_h), L = length(milb_levels()),
       lvl = ifelse(d$event & d$t_event <= h, length(milb_levels()), as.integer(d$lvl_h)),   # debut date governs MLB-by-h
       cov2020 = as.integer(d$cov2020), save_ll = 0L)
}

td_center <- function() 4

# Value-given-arrival models use classes whose control periods are (nearly) resolved; a partial control-period WAR is
# not a valid bound for early careers (WAR can decrease, and V <= 0 partials carry no information under a bound).
value_max_year <- function() 2016L
value_oot_train_max <- function() 2014L
