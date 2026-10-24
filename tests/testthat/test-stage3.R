library(testthat)
root <- normalizePath("../..")
tr <- function(x) { old <- setwd(root); on.exit(setwd(old)); targets::tar_read_raw(x) }

test_that("year index: 2020 equals 2019 (slot freeze), 2012 = 1, CPI factor 2025 = 1", {
  yi <- tr("yidx")
  expect_equal(yi$sched_index[yi$year == 2012], 1)
  expect_equal(yi$sched_index[yi$year == 2020], yi$sched_index[yi$year == 2019])
  expect_equal(yi$cpi_to_base[yi$year == 2025], 1)
  expect_false(anyNA(yi$cpi_to_base))
})

test_that("normalized money is consistent", {
  p <- tr("picks3")
  expect_equal(nrow(p), 12978L)
  s <- p[!is.na(p$bonus), ]
  expect_true(all(s$bonus_rel > 0))
  expect_equal(s$bonus_rel, s$bonus / (s$sched_index * 7200000))
  expect_true(all(p$bonus_pv[p$year != 2020] == p$bonus[p$year != 2020], na.rm = TRUE))
  expect_true(all(p$bonus_pv[p$year == 2020] <= p$bonus[p$year == 2020], na.rm = TRUE))
})

test_that("censoring flags", {
  p <- tr("picks3")
  r <- p[p$bonus_cens == "right_at_threshold", ]
  expect_true(all(!r$pool_round & r$bonus == r$r11_threshold & is.infinite(r$bonus_hi)))
  expect_true(all(p$bonus_cens[!(p$signed %in% TRUE)] == "not_signed"))
  expect_true(all(is.na(p$bonus[p$bonus_cens == "missing"])))
})

test_that("imputations cover every signed pick with a missing bonus, M draws each, nothing else", {
  p <- tr("picks3"); im <- tr("imp")$imputations
  k <- unique(im[, c("year", "pick_overall")])
  expect_equal(nrow(dplyr::anti_join(k, p[p$signed %in% TRUE & is.na(p$bonus), ], by = c("year", "pick_overall"))), 0L)
  expect_true(all(table(paste(im$year, im$pick_overall)) == 20))
  expect_true(all(im$bonus_imp > 0))
})

test_that("2017 out-of-sample check: selection-adjusted imputation tracks truth better than MAR", {
  v <- tr("imp")$validation_2017
  truth <- v[1, ]; sel <- v[2, ]; mar <- v[3, ]
  expect_lt(abs(sel$share_below_thr - truth$share_below_thr), abs(mar$share_below_thr - truth$share_below_thr))
  expect_lt(abs(sel$mean_rel - truth$mean_rel), abs(mar$mean_rel - truth$mean_rel))
})

test_that("outcomes: one row per signing pick; v_war defined only when resolved", {
  o <- tr("outcomes"); p <- tr("picks3")
  expect_equal(nrow(o), sum(p$signing_pick & !is.na(p$mlbam_id)))
  expect_equal(anyDuplicated(o$mlbam_id), 0L)
  expect_true(all(is.na(o$v_war) == !o$v_war_resolved))
  expect_true(all(o$control_seasons_obs <= 6, na.rm = TRUE))
})
