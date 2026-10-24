// Joint model of intermediate outcomes I and MLB arrival A via the factorization p(I | x) p(A | I, x).
// I is measured at horizon h seasons after the draft season (t = 0..h):
//   lvl: highest level reached by h (1 = none ... 8 = AAA, 9 = MLB debut by h); top: on an MLB Pipeline Top 100 by h.
// Model for I:
//   lvl ~ ordered_logistic(spline_l(x) + type effect + age + 2020 shift, cut)
//   top | lvl ~ bernoulli_logit(t0 + t_x x + t_g[g] + t_lvl[lvl])
//   debut timing among lvl = 9: t in 0..h ~ categorical(softmax(pi0 + pi_school)), t = 0 as reference
// Model for A given I (players not in the majors by h, i.e. lvl < 9):
//   incidence  P(ever debut | I) = inv_logit(eta), eta = spline(x) + group deviation + age + year RE + org RE
//                                                  + gam_lvl[lvl] + gam_top * top
//   latency    discrete hazard for seasons t > h:  logit h_t = alpha_t + b_lat_x x + b_lat_g[g] + b_lat_age age
//                                                  + del_lvl[lvl] + del_top * top
//   likelihood: debuted at t > h; out of affiliated ball without debut; active and censored (as in arrival.stan)
// Classes without I observed (draft year + h > last season) are excluded from this joint fit.
// Marginal arrival curve: P(A | x) = P(lvl = 9 | x) + sum_{l < 9, top} P(l, top | x) P(A | l, top, x).
data {
  int<lower=1> N; int<lower=1> K; int<lower=1> G; int<lower=1> S; int<lower=1> Y; int<lower=1> O; int<lower=1> Tmax;
  int<lower=0> h;
  matrix[N, K] B; vector[N] x;
  array[N] int<lower=1, upper=G> g; array[N] int<lower=1, upper=S> s; vector[N] age_c;
  array[N] int<lower=1, upper=Y> yr; array[N] int<lower=1, upper=O> org;
  array[N] int<lower=0, upper=Tmax> t_obs; array[N] int<lower=0, upper=1> event; array[N] int<lower=0, upper=Tmax> t_event;
  array[N] int<lower=0, upper=1> resolved_non;
  array[N] int<lower=0, upper=1> top100; int<lower=2> L; array[N] int<lower=1, upper=L> lvl; array[N] int<lower=0, upper=1> cov2020;
  int<lower=0, upper=1> save_ll;
}
transformed data {
  int T1 = Tmax + 1; int LM = L - 1;                // LM = number of non-MLB levels
  int NM = 0; for (i in 1:N) NM += (lvl[i] == L);
  int NA = N - NM;
  array[NM] int im; array[NA] int ia;
  { int a = 1; int b = 1; for (i in 1:N) { if (lvl[i] == L) { im[a] = i; a += 1; } else { ia[b] = i; b += 1; } } }
  // among lvl < L: debut after h, resolved non-arrival, censored
  // censored players with no exposure after h (t_obs <= h) contribute nothing to A | I and are skipped
  int NE = 0; int NR = 0; int NC = 0;
  for (n in 1:NA) { int i = ia[n]; NE += event[i]; NR += resolved_non[i]; NC += (!event[i] && !resolved_non[i] && t_obs[i] > h); }
  array[NE] int ev; array[NR] int rn; array[NC] int cz; array[NE] int te;
  matrix[NE, T1] Mev = rep_matrix(0, NE, T1); matrix[NC, T1] Mcz = rep_matrix(0, NC, T1);
  {
    int a = 1; int b = 1; int c = 1;
    for (n in 1:NA) {
      int i = ia[n];
      if (event[i]) { ev[a] = i; te[a] = t_event[i] + 1; for (t in (h + 2):t_event[i]) Mev[a, t] = 1; a += 1; }
      else if (resolved_non[i]) { rn[b] = i; b += 1; }
      else if (t_obs[i] > h) { cz[c] = i; for (t in (h + 2):(t_obs[i] + 1)) Mcz[c, t] = 1; c += 1; }
    }
  }
  array[NM] int tm; array[NM] int sm; for (a in 1:NM) { tm[a] = min(t_event[im[a]], h) + 1; sm[a] = s[im[a]]; }
  vector[N] top_v = to_vector(top100); vector[N] c20 = to_vector(cov2020);
}
parameters {
  vector[K] c_z; real<lower=0> sigma_rw; matrix[G, K] d_raw; real<lower=0> tau_d; vector[S] beta_age;
  real<lower=0> sigma_yr; vector[Y] z_yr; real<lower=0> sigma_org; vector[O] z_org;
  vector[T1] alpha_z; real<lower=0> sigma_alpha; real b_lat_x; vector[G] b_lat_g; vector[S] b_lat_age;
  vector[LM] gam_lvl_raw; real gam_top; vector[LM] del_lvl_raw; real del_top;
  // I model
  vector[K] cl_z; real<lower=0> sig_rw_l; vector[G] l_g; vector[S] l_age; real l_2020; ordered[L - 1] cut;
  real t0; real t_x; vector[G] t_g; vector[L] t_lvl_raw;
  vector[h] pi0_free; matrix[S, h] pi_s_free;      // softmax reference category: t = 0
}
transformed parameters {
  vector[K] c; vector[T1] alpha; matrix[G, K] d; vector[K] cl;
  vector[LM] gam_lvl = gam_lvl_raw - mean(gam_lvl_raw); vector[LM] del_lvl = del_lvl_raw - mean(del_lvl_raw);
  vector[L] t_lvl = t_lvl_raw - mean(t_lvl_raw);
  vector[h + 1] pi0 = append_row(0, pi0_free); matrix[S, h + 1] pi_s = append_col(rep_vector(0, S), pi_s_free);
  c[1] = -2 + 2 * c_z[1]; for (k in 2:K) c[k] = c[k - 1] + sigma_rw * c_z[k];
  alpha[1] = -3 + 2 * alpha_z[1]; for (t in 2:T1) alpha[t] = alpha[t - 1] + sigma_alpha * alpha_z[t];
  for (gg in 1:G) d[gg] = tau_d * cumulative_sum(d_raw[gg]);
  cl[1] = 1.5 * cl_z[1]; for (k in 2:K) cl[k] = cl[k - 1] + sig_rw_l * cl_z[k];
}
model {
  c_z ~ std_normal(); sigma_rw ~ normal(0, 0.5); to_vector(d_raw) ~ std_normal(); tau_d ~ normal(0, 0.5);
  beta_age ~ normal(0, 0.5); sigma_yr ~ normal(0, 0.5); z_yr ~ std_normal(); sigma_org ~ normal(0, 0.5); z_org ~ std_normal();
  alpha_z ~ std_normal(); sigma_alpha ~ normal(0, 1); b_lat_x ~ normal(0, 1); b_lat_g ~ normal(0, 1); b_lat_age ~ normal(0, 0.5);
  gam_lvl_raw ~ normal(0, 1.5); gam_top ~ normal(0, 1.5); del_lvl_raw ~ normal(0, 1); del_top ~ normal(0, 1);
  cl_z ~ std_normal(); sig_rw_l ~ normal(0, 0.5); l_g ~ normal(0, 1); l_age ~ normal(0, 0.5); l_2020 ~ normal(0, 1);
  cut ~ normal(0, 3); t0 ~ normal(-3, 2); t_x ~ normal(0, 1); t_g ~ normal(0, 1); t_lvl_raw ~ normal(0, 2);
  pi0_free ~ normal(0, 2); to_vector(pi_s_free) ~ normal(0, 1);

  // I | x
  {
    vector[N] vl = B * cl + l_g[g] + l_age[s] .* age_c + l_2020 * c20;
    lvl ~ ordered_logistic(vl, cut);
    top100 ~ bernoulli_logit(t0 + t_x * x + t_g[g] + t_lvl[lvl]);
  }
  // debut timing among MLB-by-h
  for (a in 1:NM) tm[a] ~ categorical_logit(pi0 + pi_s[sm[a]]');

  // A | I for lvl < L
  {
    vector[N] eta = B * c + rows_dot_product(B, d[g]) + beta_age[s] .* age_c + sigma_yr * z_yr[yr] + sigma_org * z_org[org];
    vector[N] lat = b_lat_x * x + b_lat_g[g] + b_lat_age[s] .* age_c;
    for (n in 1:NA) { int i = ia[n]; eta[i] += gam_lvl[lvl[i]] + gam_top * top_v[i]; lat[i] += del_lvl[lvl[i]] + del_top * top_v[i]; }
    {
      vector[NE] le = lat[ev];
      target += sum(log_inv_logit(eta[ev])) + sum(rows_dot_product(Mev, log1m_inv_logit(rep_matrix(le, T1) + rep_matrix(alpha', NE))))
                + sum(log_inv_logit(alpha[te] + le));
    }
    target += sum(log1m_inv_logit(eta[rn]));
    target += sum(log1m_exp(log_inv_logit(eta[cz]) + log1m_exp(rows_dot_product(Mcz, log1m_inv_logit(rep_matrix(lat[cz], T1) + rep_matrix(alpha', NC))))));
  }
}
