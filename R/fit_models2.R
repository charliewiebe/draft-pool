# run_fit with user-supplied initial values (kept separate so existing fit targets are not invalidated).
run_fit_init <- function(model_file, data, init, chains = 4L, warmup = 750L, sampling = 1000L, seed = 1L, adapt_delta = 0.9,
                         drop_vars = character(), label = basename(model_file)) {
  m <- cmdstanr::cmdstan_model(model_file); t0 <- Sys.time()
  f <- m$sample(data = data, chains = chains, parallel_chains = min(chains, 2L), iter_warmup = warmup, iter_sampling = sampling,
                seed = seed, adapt_delta = adapt_delta, init = init, refresh = 0, show_messages = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  keep <- setdiff(f$metadata()$stan_variables, c(drop_vars, "lp__"))
  summ <- posterior::summarise_draws(f$draws(variables = keep), "mean", "sd", ~quantile(.x, c(0.05, 0.95)), "rhat", "ess_bulk", "ess_tail")
  diag <- f$diagnostic_summary(quiet = TRUE)
  list(label = label, draws = posterior::as_draws_df(f$draws(variables = keep)), summary = summ, u_summary = NULL,
       diagnostics = list(divergent = sum(diag$num_divergent), max_treedepth = sum(diag$num_max_treedepth), ebfmi = diag$ebfmi,
                          max_rhat = max(summ$rhat, na.rm = TRUE), min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
                          min_ess_tail = min(summ$ess_tail, na.rm = TRUE), minutes = elapsed, chains = chains, draws = chains * sampling))
}

# Jittered starting values for the accrual model in the region the well-behaved pilot chain occupied
# (one pilot chain from CmdStan's default uniform(-2, 2) inits stuck at lp ~ -35,500 with nu near its bound).
accrual_inits <- function(chains, Y = 14L, seed = 1L) {
  set.seed(seed)
  lapply(seq_len(chains), function(k) {
    j <- function(n, s = 0.1) stats::rnorm(n, 0, s)
    list(a0 = j(2), a_milb = j(2), a_k = matrix(j(8), 2, 4), a_gap = matrix(j(6), 2, 3), b_l = j(2), b_m = j(2), b_age = j(2, 0.02),
         b_age2 = j(2, 0.005), b_hit = j(2), sig_s = c(0.3, 0.5) + abs(j(2, 0.05)), z_s = matrix(j(2 * Y, 0.3), 2, Y),
         c0 = 0.1 + j(1, 0.02), c_k = j(4, 0.05), c_l = 0.2 + j(1, 0.02), c_m = 0.25 + j(1, 0.02), c_age = j(1, 0.01), c_age2 = j(1, 0.002),
         c_hit = j(1, 0.05), c_gap = j(1, 0.05), s0 = c(-0.55, -0.42) + j(2, 0.02), s_m = j(1, 0.02), s_l = j(1, 0.02),
         e0 = c(-0.5, -0.4) + j(2, 0.05), e_m = j(1, 0.02), e_l = j(1, 0.02), r20 = 0.45, rs20 = 0.6)
  })
}

# Thin a fit's draws by k within each chain (keeps the run_fit structure; diagnostics are left as computed on all draws)
thin_fit <- function(f, k = 4L) { f$draws <- posterior::thin_draws(f$draws, thin = k); f }
