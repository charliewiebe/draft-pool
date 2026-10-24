root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("model_data.R", "supply.R", "simulator.R", "signing.R", "decision.R")) source(file.path(root, "R", f))

toy_scenario <- function(n = 40L, L = 6L) {
  set.seed(1)
  list(pi = as.integer(0:(n - 1)), L = L, slot = c(3e6, 2e6, 1e6, 8e5, 5e5, 3e5), lpc = rep(0, L),
       Bw = matrix(c(-9, 0, -9), L, 3, byrow = TRUE), Bmu = matrix(0, L, 3), rbin = rep(0L, L),
       g = rep(0:5, length.out = n), school = rep(c(0L, 0L, 1L, 1L, 2L, 2L), length.out = n), ranked = rep(0L, n), rank = rep(300, n),
       agec = rep(0, n), sr = rep(0, n), u1 = seq(0.01, 0.99, length.out = n), z2 = stats::rnorm(n), u3 = rep(0.99, n),
       wg = matrix(0, 3, 6), wgs = matrix(0, 3, 6), wx = matrix(0, 3, 3), mg = matrix(0, 3, 6), mx = matrix(0, 3, 3),
       wsr = rep(0, 3), wyre = rep(0, 3), ls0 = rep(-1, 3), ls1 = rep(0, 3), msr = 0, ore = 0, off_over = rep(0, 5), off_deep = rep(0, 5))
}

test_that("engine respects the budget, the caps and the other teams' priority order", {
  old <- setwd(root); on.exit(setwd(old))
  load_engine()
  sc <- toy_scenario()
  lbg <- log(exp(seq(log(1e4), log(1.1e7), length.out = 48)))
  V <- rep(2 * exp(lbg), 18)                          # value = 2 x bonus everywhere
  vl <- list(lbg = lbg, Vbar = V, Vd = V, Vp = V, i2_eval = 0L)
  fc <- list(fk = c(1L, 4L), frg = c(0L, 2L), comp = c(0, 0))
  pl <- list(mode = c(0L, 0L), cap = c(1, 1), rmin = c(0.75, 0.1), type_restrict = c(-1L, -1L), school_restrict = c(-1L, -1L), lambda = 1,
             budget = 2.5e6, use_fail = 0L, beta = 1, alpha = rep(-5, 12), i2_decide = 0L)
  e <- engine_eval(sc, fc, pl, vl)
  expect_true(all(e$ask <= c(2e6, 5e5) * 1 + 1e-6))     # caps at slot
  expect_lte(sum(e$ask), 2.5e6 + 1e-6)                  # budget
  expect_false(0L %in% e$pid)                           # player 0 went to the other team at pick 1
  expect_equal(length(unique(e$pid)), 2L)
  expect_equal(e$ve, 2 * e$ask, tolerance = 0.01)        # value lookup interpolates the surface (linear in log bonus)
})

test_that("focal pick sets mirror the round-1 position", {
  o <- data.frame(pick = 1:60, round_label = rep(as.character(1:2), each = 30), slot_usd = 60:1 * 1e5, rg_sign = 0L, rbin = 0L)
  o$round <- round_group(o$round_label); o$comp_eligible <- TRUE
  o25 <- list(order = o)
  fs <- focal_set(o25, 10)
  expect_equal(fs$pick, c(10, 40))
  expect_equal(fs$pool, sum(o$slot_usd[c(10, 40)]))
})

test_that("signing round groups cover rounds 1-20", {
  expect_equal(sign_rg(c("1", "CB-A", "4", "8", "11", "15", "21")), c("R1+C", "R2-CBB", "R3-5", "R6-10", "R11-12", "R13-20", NA))
})
