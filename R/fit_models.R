# Stan fitting wrappers. Returns compact objects (draws of model-level parameters, diagnostics, LOO, timings).

run_fit <- function(model_file, data, chains = 4L, warmup = 750L, sampling = 1000L, seed = 20261020L,
                    adapt_delta = 0.9, drop_vars = c("u", "log_lik", "log_lik_tier", "eta_p", "mu", "eta"),
                    loo_var = NULL, label = basename(model_file)) {
  m <- cmdstanr::cmdstan_model(model_file)
  t0 <- Sys.time()
  f <- m$sample(data = data, chains = chains, parallel_chains = min(chains, 2L), iter_warmup = warmup, iter_sampling = sampling,
                seed = seed, adapt_delta = adapt_delta, refresh = 0, show_messages = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  vars <- f$metadata()$stan_variables
  keep <- setdiff(vars, c(drop_vars, "lp__"))
  dr <- posterior::as_draws_df(f$draws(variables = keep))
  summ <- posterior::summarise_draws(f$draws(variables = keep), "mean", "sd", ~quantile(.x, c(0.05, 0.95)), "rhat", "ess_bulk", "ess_tail")
  diag <- f$diagnostic_summary(quiet = TRUE)
  out <- list(label = label, draws = dr, summary = summ,
              diagnostics = list(divergent = sum(diag$num_divergent), max_treedepth = sum(diag$num_max_treedepth), ebfmi = diag$ebfmi,
                                 max_rhat = max(summ$rhat, na.rm = TRUE), min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
                                 min_ess_tail = min(summ$ess_tail, na.rm = TRUE), minutes = elapsed, chains = chains, draws = chains * sampling),
              u_summary = if ("u" %in% vars) posterior::summarise_draws(f$draws("u"), "mean", "sd") else NULL)
  for (lv in loo_var) {
    ll <- f$draws(lv, format = "matrix")
    keepcols <- colSums(abs(ll)) > 0                      # drop all-zero contributions (no information)
    r_eff <- loo::relative_eff(exp(ll[, keepcols]), chain_id = rep(seq_len(chains), each = sampling))
    out[[paste0("loo_", lv)]] <- loo::loo(ll[, keepcols], r_eff = r_eff)
    out[[paste0("ll_cols_", lv)]] <- which(keepcols)
  }
  if ("eta_p" %in% vars) out$fitted_means <- list(eta_p = colMeans(f$draws("eta_p", format = "matrix")), mu = colMeans(f$draws("mu", format = "matrix")))
  out
}


# Combine independently run chain groups (same model/data, different seeds) into one fit object;
# R-hat / ESS are recomputed across all chains.
combine_fits <- function(...) {
  fs <- list(...)
  dr <- fs[[1]]$draws
  for (k in seq_along(fs)[-1]) {
    d <- fs[[k]]$draws; d$.chain <- d$.chain + max(dr$.chain)
    dr <- posterior::bind_draws(dr, d, along = "chain")
  }
  summ <- posterior::summarise_draws(dr, "mean", "sd", ~quantile(.x, c(0.05, 0.95)), "rhat", "ess_bulk", "ess_tail")
  dg <- lapply(fs, `[[`, "diagnostics")
  list(label = fs[[1]]$label, draws = dr, summary = summ,
       diagnostics = list(divergent = sum(sapply(dg, `[[`, "divergent")), max_treedepth = sum(sapply(dg, `[[`, "max_treedepth")),
                          ebfmi = unlist(lapply(dg, `[[`, "ebfmi")), max_rhat = max(summ$rhat, na.rm = TRUE),
                          min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE), min_ess_tail = min(summ$ess_tail, na.rm = TRUE),
                          minutes = sum(sapply(dg, `[[`, "minutes")), chains = posterior::nchains(dr), draws = posterior::ndraws(dr)),
       u_summary = fs[[1]]$u_summary)
}

# ---- chunked sampling: warmup once, then continue chains in fixed-adaptation chunks ----------
last_draw_inits <- function(fit, params) {
  dr <- posterior::subset_draws(fit$draws(variables = params), iteration = posterior::niterations(fit$draws()))
  lapply(seq_len(posterior::nchains(dr)), function(k) {
    rv <- posterior::as_draws_rvars(posterior::subset_draws(dr, chain = k))
    lapply(rv, function(r) { a <- posterior::draws_of(r); d <- dim(a)[-1]; if (length(d) <= 1) as.vector(a) else array(a, d) })
  })
}

chunk_state <- function(f, params, m) {
  list(inits = last_draw_inits(f, params), inv_metric = f$inv_metric(matrix = FALSE),
       step_size = f$metadata()$step_size_adaptation, model_file = m$stan_file())
}

run_warmup <- function(model_file, data, chains = 2L, warmup = 600L, seed = 1L, adapt_delta = 0.9) {
  m <- cmdstanr::cmdstan_model(model_file); t0 <- Sys.time()
  f <- m$sample(data = data, chains = chains, parallel_chains = chains, iter_warmup = warmup, iter_sampling = 1L,
                seed = seed, adapt_delta = adapt_delta, refresh = 100, show_messages = TRUE)
  params <- names(m$variables()$parameters)
  list(state = chunk_state(f, params, m), params = params, minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")),
       data = data, seed = seed)
}

run_chunk <- function(prev, n_iter = 250L, k = 1L, drop_vars = c("log_lik", "eta")) {
  st <- prev$state; m <- cmdstanr::cmdstan_model(st$model_file); nch <- length(st$inits); t0 <- Sys.time()
  mf <- vapply(seq_len(nch), function(i) { p <- tempfile(fileext = ".json"); cmdstanr::write_stan_json(list(inv_metric = st$inv_metric[[i]]), p); p }, "")
  f <- m$sample(data = prev$data, chains = nch, parallel_chains = nch, iter_warmup = 0L, iter_sampling = n_iter, adapt_engaged = FALSE,
                metric_file = mf, step_size = unlist(st$step_size), init = st$inits, seed = prev$seed + 1000L * k,
                refresh = 50, show_messages = TRUE)
  vars <- setdiff(f$metadata()$stan_variables, c(drop_vars, "lp__"))
  diag <- f$diagnostic_summary(quiet = TRUE)
  list(state = chunk_state(f, prev$params, m), params = prev$params, data = prev$data, seed = prev$seed,
       draws = posterior::as_draws_df(f$draws(variables = vars)),
       diag = list(divergent = sum(diag$num_divergent), max_treedepth = sum(diag$num_max_treedepth), ebfmi = diag$ebfmi,
                   minutes = as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

# Assemble chunks of one chain group (along iterations) into a run_fit-like object
assemble_chunks <- function(warm, ...) {
  ch <- list(...)
  dr <- Reduce(function(a, b) posterior::bind_draws(a, b, along = "iteration"), lapply(ch, `[[`, "draws"))
  summ <- posterior::summarise_draws(dr, "mean", "sd", ~quantile(.x, c(0.05, 0.95)), "rhat", "ess_bulk", "ess_tail")
  list(label = "chunked", draws = dr, summary = summ, u_summary = NULL,
       diagnostics = list(divergent = sum(sapply(ch, function(x) x$diag$divergent)), max_treedepth = sum(sapply(ch, function(x) x$diag$max_treedepth)),
                          ebfmi = unlist(lapply(ch, function(x) x$diag$ebfmi)), max_rhat = max(summ$rhat, na.rm = TRUE),
                          min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE), min_ess_tail = min(summ$ess_tail, na.rm = TRUE),
                          minutes = warm$minutes + sum(sapply(ch, function(x) x$diag$minutes)),
                          chains = posterior::nchains(dr), draws = posterior::ndraws(dr)))
}
