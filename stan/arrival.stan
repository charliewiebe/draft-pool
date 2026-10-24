// MLB arrival: mixture cure model.
//   incidence  P(ever debut) = inv_logit(eta_i)
//     eta_i = sum_k B_ik (c_k + d_{g,k}) + beta_age[s] * age_c + sigma_yr * z_yr + sigma_org * z_org + lambda_inc * u_i
//     c: first-order random walk (P-spline); d_g: group (school x role) deviations, random walk, shrunk by tau_d
//   latency    discrete-time hazard of debut in season t (t = 0 is the draft season), given eventual debut
//     logit h_it = alpha_t + b_lat_x * x_i + b_lat_g[g] + b_lat_age[s] * age_c + lambda_lat * u_i
//   likelihood debuted at t:        log p + log h_t + sum_{j<t} log(1 - h_j)
//              out of pro, no debut: log(1 - p)            (comeback-to-debut rate ~0.4%, ignored)
//              active, no debut:     log((1 - p) + p * prod_{j<=T} (1 - h_j))   (right-censored)
//   joint (use_u = 1): shared latent development u_i ~ N(0,1) also drives
//     top-100 appearance within h seasons (logit) and highest level reached within h seasons (ordinal)
functions {
  real rw_lpdf(vector v, real scale) {
    return normal_lpdf(v[2:rows(v)] - v[1:(rows(v) - 1)] | 0, scale);
  }
}
data {
  int<lower=1> N; int<lower=1> K; int<lower=1> G; int<lower=1> S; int<lower=1> Y; int<lower=1> O; int<lower=1> Tmax;
  matrix[N, K] B; vector[N] x;
  array[N] int<lower=1, upper=G> g; array[N] int<lower=1, upper=S> s; vector[N] age_c;
  array[N] int<lower=1, upper=Y> yr; array[N] int<lower=1, upper=O> org;
  array[N] int<lower=0, upper=Tmax> t_obs; array[N] int<lower=0, upper=1> event; array[N] int<lower=0, upper=Tmax> t_event;
  array[N] int<lower=0, upper=1> resolved_non;
  array[N] int<lower=0, upper=1> has_int; array[N] int<lower=0, upper=1> top100;
  int<lower=2> L; array[N] int<lower=1, upper=L> lvl; array[N] int<lower=0, upper=1> cov2020;
  int<lower=0, upper=1> use_u; int<lower=0, upper=1> prior_only; int<lower=0, upper=1> save_ll;
}
transformed data {
  // index sets and survival masks for a vectorized likelihood
  int T1 = Tmax + 1;
  int NE = sum(event); int NR = sum(resolved_non); int NC = N - NE - NR;
  array[NE] int ev; array[NR] int rn; array[NC] int cz; array[NE] int te;
  matrix[NE, T1] Mev = rep_matrix(0, NE, T1);     // 1 for seasons survived before debut
  matrix[NC, T1] Mcz = rep_matrix(0, NC, T1);     // 1 for seasons observed without debut
  int NI = sum(has_int); array[NI] int ii;
  {
    int a = 1; int b = 1; int c = 1; int k = 1;
    for (i in 1:N) {
      if (event[i]) { ev[a] = i; te[a] = t_event[i] + 1; for (t in 1:t_event[i]) Mev[a, t] = 1; a += 1; }
      else if (resolved_non[i]) { rn[b] = i; b += 1; }
      else { cz[c] = i; for (t in 1:(t_obs[i] + 1)) Mcz[c, t] = 1; c += 1; }
      if (has_int[i]) { ii[k] = i; k += 1; }
    }
  }
  array[NI] int top_i = top100[ii]; array[NI] int lvl_i = lvl[ii];
  vector[NI] x_i = x[ii]; vector[NI] c20_i = to_vector(cov2020[ii]); array[NI] int g_i = g[ii];
}
parameters {
  vector[K] c_z; real<lower=0> sigma_rw;
  matrix[G, K] d_raw; real<lower=0> tau_d;
  vector[S] beta_age;
  real<lower=0> sigma_yr; vector[Y] z_yr; real<lower=0> sigma_org; vector[O] z_org;
  vector[Tmax + 1] alpha_z; real<lower=0> sigma_alpha;
  real b_lat_x; vector[G] b_lat_g; vector[S] b_lat_age;
  vector[use_u ? N : 0] u;
  real<lower=0> lambda_lvl; real lambda_inc; real lambda_lat; real lambda_top;
  real t0; real t_x; vector[G] t_g;
  ordered[L - 1] cut; real l_x; vector[G] l_g; real l_2020;
}
transformed parameters {
  // non-centered random walks: c[1] ~ N(-2, 2), c[k] - c[k-1] ~ N(0, sigma_rw); d_g[k] - d_g[k-1] ~ N(0, tau_d)
  vector[K] c; vector[Tmax + 1] alpha; matrix[G, K] d;
  c[1] = -2 + 2 * c_z[1];
  for (k in 2:K) c[k] = c[k - 1] + sigma_rw * c_z[k];
  alpha[1] = -3 + 2 * alpha_z[1];
  for (t in 2:(Tmax + 1)) alpha[t] = alpha[t - 1] + sigma_alpha * alpha_z[t];
  for (gg in 1:G) d[gg] = tau_d * cumulative_sum(d_raw[gg]);
}
model {
  vector[N] eta; vector[N] lat;
  c_z ~ std_normal(); sigma_rw ~ normal(0, 0.5);
  to_vector(d_raw) ~ std_normal();
  tau_d ~ normal(0, 0.5);
  beta_age ~ normal(0, 0.5);
  sigma_yr ~ normal(0, 0.5); z_yr ~ std_normal(); sigma_org ~ normal(0, 0.5); z_org ~ std_normal();
  alpha_z ~ std_normal(); sigma_alpha ~ normal(0, 1);
  b_lat_x ~ normal(0, 1); b_lat_g ~ normal(0, 1); b_lat_age ~ normal(0, 0.5);
  lambda_inc ~ normal(0, 1); lambda_lat ~ normal(0, 1); lambda_top ~ normal(0, 1); lambda_lvl ~ normal(0, 1);
  t0 ~ normal(-3, 2); t_x ~ normal(0, 1); t_g ~ normal(0, 1);
  cut ~ normal(0, 3); l_x ~ normal(0, 1); l_g ~ normal(0, 1); l_2020 ~ normal(0, 1);
  if (use_u) u ~ std_normal();

  eta = B * c + rows_dot_product(B, d[g]) + beta_age[s] .* age_c + sigma_yr * z_yr[yr] + sigma_org * z_org[org];
  lat = b_lat_x * x + b_lat_g[g] + b_lat_age[s] .* age_c;
  if (use_u) { eta += lambda_inc * u; lat += lambda_lat * u; }

  if (!prior_only) {
    // debuted at te: log p + sum_{t < te} log(1 - h_t) + log h_te
    {
      vector[NE] le = lat[ev];
      matrix[NE, T1] H = log1m_inv_logit(rep_matrix(le, T1) + rep_matrix(alpha', NE));
      target += sum(log_inv_logit(eta[ev])) + sum(rows_dot_product(Mev, H)) + sum(log_inv_logit(alpha[te] + le));
    }
    // out of affiliated ball without debut: log(1 - p)
    target += sum(log1m_inv_logit(eta[rn]));
    // still active, no debut: log(1 - p (1 - S)), S = prod_{t <= t_obs} (1 - h_t)
    {
      matrix[NC, T1] H = log1m_inv_logit(rep_matrix(lat[cz], T1) + rep_matrix(alpha', NC));
      target += sum(log1m_exp(log_inv_logit(eta[cz]) + log1m_exp(rows_dot_product(Mcz, H))));
    }
    if (use_u) {
      vector[NI] ui = u[ii];
      top_i ~ bernoulli_logit(t0 + t_x * x_i + t_g[g_i] + lambda_top * ui);
      lvl_i ~ ordered_logistic(l_x * x_i + l_g[g_i] + l_2020 * c20_i + lambda_lvl * ui, cut);
    }
  }
}
generated quantities {
  vector[save_ll ? N : 0] log_lik;     // arrival contribution only (conditional on u when use_u = 1)
  if (save_ll) {
    matrix[N, G] BD = B * d';
    for (i in 1:N) {
      real e = B[i] * c + BD[i, g[i]] + beta_age[s[i]] * age_c[i] + sigma_yr * z_yr[yr[i]] + sigma_org * z_org[org[i]];
      real la = b_lat_x * x[i] + b_lat_g[g[i]] + b_lat_age[s[i]] * age_c[i];
      if (use_u) { e += lambda_inc * u[i]; la += lambda_lat * u[i]; }
      if (event[i]) {
        real lp = log_inv_logit(e);
        for (t in 0:(t_event[i] - 1)) lp += log1m_inv_logit(alpha[t + 1] + la);
        log_lik[i] = lp + log_inv_logit(alpha[t_event[i] + 1] + la);
      } else if (resolved_non[i]) {
        log_lik[i] = log1m_inv_logit(e);
      } else {
        real ls = 0;
        for (t in 0:t_obs[i]) ls += log1m_inv_logit(alpha[t + 1] + la);
        log_lik[i] = log_sum_exp(log1m_inv_logit(e), log_inv_logit(e) + ls);
      }
    }
  }
}
