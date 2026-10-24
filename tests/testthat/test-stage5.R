root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("model_data.R", "accrual.R", "supply.R", "simulator.R")) source(file.path(root, "R", f))

test_that("accrual panel rows respect the control window and exit rule", {
  md <- tibble::tibble(mlbam_id = 1:2, year = c(2015L, 2015L), debut_season = c(2017L, 2018L), age_at_draft = c(21, 18),
                       role = c("H", "P"), school = c("4yr", "HS"), group = c(4L, 1L), out_of_pro = c(FALSE, TRUE),
                       control_status = c("complete", "exited_before_6"), debuted = TRUE)
  se <- tibble::tibble(mlbam_id = c(rep(1L, 8), 2L, 2L), season = c(2017:2024, 2018L, 2019L),
                       mlb_games = c(rep(100L, 8), 5L, NA), milb_top_level = c(rep(NA, 8), NA, "AAA"),
                       bwar = c(1, 2, 3, 1, 1, 1, 9, 9, 0.2, NA), pa = NA_real_, ipouts = NA_real_)
  pn <- accrual_panel(md, se, last_season = 2026L)
  r1 <- pn$rows[pn$rows$mlbam_id == 1, ]
  expect_equal(range(r1$k_prev), c(1L, 5L))                 # rows stop once six MLB seasons are banked
  expect_equal(pn$end$cumw_end[pn$end$mlbam_id == 1], 9)     # control WAR = first six seasons only
  r2 <- pn$rows[pn$rows$mlbam_id == 2, ]
  expect_true(all(r2$none_run < 2))                          # no rows after two straight seasons out of pro ball
  expect_equal(r2$state[r2$season == 2019], 2L)
})

test_that("ratio components partition bonus / slot", {
  r <- c(0.05, 0.249, 0.25, 0.984, 0.985, 1, 1.015, 1.016, 3)
  comp <- as.integer(cut(r, ratio_breaks(), right = FALSE)); comp <- ifelse(r == 1.015, 3L, comp)
  expect_equal(comp, c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L))
})

test_that("component magnitude transforms round-trip and stay in range", {
  r <- c(0.01, 0.2, 0.3, 0.9, 1.02, 2.5)
  comp <- as.integer(cut(r, ratio_breaks(), right = FALSE))
  expect_equal(z_to_ratio(comp, ratio_to_z(comp, r)), r, tolerance = 1e-6)
  set.seed(2); z <- rnorm(1000, 0, 3)
  expect_true(all(z_to_ratio(rep(1L, 1000), z) < 0.25)); expect_true(all(z_to_ratio(rep(4L, 1000), z) > 1.015))
  u <- z_to_ratio(rep(2L, 1000), z); expect_true(all(u >= 0.25 & u <= 0.985))
})

test_that("round groups map supplemental and competitive-balance labels", {
  expect_equal(round_group(c("1", "1C", "C-A", "SUP", "PPI", "2", "CB-A", "CB-B", "C-B", "SUP-2", "3", "10")),
               c("R1+C", "R1+C", "R1+C", "R1+C", "R1+C", "R2-CBB", "R2-CBB", "R2-CBB", "R2-CBB", "R2-CBB", "R3", "R10"))
})

test_that("a simulated draft fills every pick with a distinct player and respects deal components", {
  inp_path <- file.path(root, "_targets")
  skip_if_not(dir.exists(inp_path))
  old <- setwd(root); on.exit(setwd(old))
  inp <- tryCatch(targets::tar_read(sim_inp), error = function(e) NULL)
  skip_if(is.null(inp))
  set.seed(3)
  d <- simulate_draft(inp, 1L, inp$years[1])
  expect_equal(nrow(d), nrow(inp$templates[[1]]$order))
  expect_false(any(duplicated(d$pid)))
  expect_true(all(d$ratio > 0))
  expect_equal(d$comp, as.integer(cut(d$ratio, ratio_breaks(), right = FALSE)) |> (\(x) ifelse(d$ratio == 1.015, 3L, x))())
})
