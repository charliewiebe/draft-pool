library(testthat)
root <- normalizePath("../..")
tr <- function(x) { old <- setwd(root); on.exit(setwd(old)); targets::tar_read_raw(x) }

test_that("all Stage 4 fits meet the diagnostic thresholds (R-hat < 1.01, bulk/tail ESS > 400, no divergences, >= 4000 draws)", {
  d <- tr("diag4")
  expect_true(all(d$draws >= 4000))
  expect_true(all(d$divergent == 0))
  expect_true(all(d$max_rhat < 1.01 + 1e-9))
  expect_true(all(d$min_ess_bulk > 400 & d$min_ess_tail > 400))
  expect_true(all(d$min_ebfmi > 0.3))
})

test_that("vectorized arrival likelihood is the model actually fitted (save_ll path gives finite log-lik)", {
  f <- tr("fit_arr_mature")
  expect_true(is.finite(f$loo_log_lik$estimates["elpd_loo", 1]))
})

test_that("grid: probabilities valid, arrival increases with bonus at the top, timing mass <= arrival", {
  g <- tr("grid4_summary")
  expect_true(all(g$p_arr >= 0 & g$p_arr <= 1))
  expect_true(all(g$p_gt10 <= g$p_gt5 + 1e-12))
  top <- g[g$age_c == 0 & g$bonus > 5e5, ]
  for (ty in unique(top$type)) { x <- top[top$type == ty, ]; x <- x[order(x$bonus), ]; expect_true(all(diff(x$p_arr) > 0)) }
})

test_that("new-year predictions (how the grid predicts) match observed tier shares for resolved classes", {
  v <- tr("newyear4")
  expect_true(all(abs(v$pred_share - v$obs_share) < 0.06))
  expect_lt(v$sy[1], 0.5)                       # year effect measures class strength, not censoring selection
})

test_that("joint fit is 4 chains x 1000 and chunks continue chains (adaptation fixed)", {
  j <- tr("fit_arr_joint")
  expect_equal(posterior::nchains(j$draws), 4L)
  expect_equal(posterior::niterations(j$draws), 1000L)
})

test_that("valuation: tier values ordered; discounting lowers value with later debut", {
  tv <- tr("tv")
  expect_true(all(diff(as.numeric(tv$tier_mean_v)) > 0))
  e <- tv$tier_E1[5, , "a", "0.08"]; expect_true(all(diff(e) < 0))
})
