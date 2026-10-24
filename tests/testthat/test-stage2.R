library(testthat)
root <- normalizePath("../..")   # testthat::test_dir() runs from tests/testthat
withr_dir <- function(code) { old <- setwd(root); on.exit(setwd(old)); force(code) }

test_that("rules table: every cell has a source URL and a known status; years complete", {
  r <- withr_dir(targets::tar_read(rules))
  expect_true(all(grepl("^https://", r$source_url)))
  expect_true(all(r$status %in% c("verified_source", "observed_in_data", "inferred_same_cba", "current_rule_page")))
  core <- c("n_rounds", "r11_threshold_usd", "band1_0_5pct", "band2_5_10pct", "band3_10_15pct", "band4_gt15pct",
            "unsigned_slot_removed_from_pool", "draft_lottery_top6")
  for (k in core) expect_setequal(r$year[r$rule == k], 2012:2025)
  thr <- setNames(r$value[r$rule == "r11_threshold_usd"], r$year[r$rule == "r11_threshold_usd"])
  expect_equal(unname(thr[c("2016", "2017", "2022", "2023")]), c("100000", "125000", "125000", "150000"))
})

test_that("rounds observed in Stats API match regime breaks", {
  r <- withr_dir(targets::tar_read(rules))
  nr <- setNames(as.integer(r$value[r$rule == "n_rounds"]), r$year[r$rule == "n_rounds"])
  expect_true(all(nr[as.character(2012:2019)] == 40)); expect_equal(nr[["2020"]], 5L)
  expect_true(all(nr[as.character(2021:2025)] == 20))
})

test_that("slot schedule is position-based: 2016 published = 2012 x scale within $200", {
  art <- withr_dir(targets::tar_read(art))
  a12 <- art[["2012"]]$picks; a16 <- art[["2016"]]$picks
  j <- merge(a16, a12, by = "pick_overall")
  expect_lte(max(abs(j$value.x - round(j$value.y * 9015000 / 7200000 / 100) * 100)), 200)
})

test_that("pool reconstruction matches published pools (except documented post-publication trades)", {
  pr <- withr_dir(targets::tar_read(pools))
  bad <- pr[abs(pr$diff) > 500, ]
  expect_true(all(bad$year == 2014 & bad$team_id %in% c(134L, 146L)))  # Marlins->Pirates pick 39 trade
  expect_equal(sum(pr$pool_reconstructed[pr$year == 2014]), sum(pr$pool_published[pr$year == 2014]), tolerance = 1e-4)
})

test_that("Stats API slot values agree with MLB.com per-pick lists 2018-2025", {
  sc <- withr_dir(targets::tar_read(slot_check))
  s <- sc[sc$year >= 2018, ]
  expect_true(all(s$n_value_equal == s$n_both))
})

test_that("bonus reconciliation: zero Stats API bonus never becomes a bonus; categories exhaustive", {
  p <- withr_dir(targets::tar_read(picks))
  expect_false(any(p$bonus %in% 0 & p$bonus_status == "sa_only"))
  expect_true(all(p$bonus_status %in% c("agree", "disagree", "sa_only", "br_only", "neither")))
  expect_true(all(is.na(p$bonus[p$bonus_status == "neither"])))
})

test_that("one row per pick; no player signs twice", {
  p <- withr_dir(targets::tar_read(picks))
  expect_equal(anyDuplicated(p[, c("year", "pick_overall")]), 0L)
  expect_true(all(p$n_signed_picks <= 1))
})
