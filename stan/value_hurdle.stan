// Control-period WAR given MLB arrival: hurdle model.
//   P(V > 0) = inv_logit(eta_p);  V | V > 0 ~ lognormal(mu, sigma_g);  -V | V <= 0 ~ exponential(lambda_neg)
//   eta_p, mu: P-spline in x (common + group deviations) + age by school + debut timing (linear by school + quadratic)
//             + draft-year RE + org RE
//   censored (control period incomplete): partial sum nu treated as a lower bound, contribution P(V > nu)
functions {
  real rw_lpdf(vector v, real scale) { return normal_lpdf(v[2:rows(v)] - v[1:(rows(v) - 1)] | 0, scale); }
  real tier_lpmf(int k, real p, real mu, real sg, real lam) {
    // tiers: 1: V<=0, 2: (0,2], 3: (2,5], 4: (5,10], 5: >10
    if (k == 1) return log1m(p);
    if (k == 2) return log(p) + lognormal_lcdf(2 | mu, sg);
    if (k == 3) return log(p) + log_diff_exp(lognormal_lcdf(5 | mu, sg), lognormal_lcdf(2 | mu, sg));
    if (k == 4) return log(p) + log_diff_exp(lognormal_lcdf(10 | mu, sg), lognormal_lcdf(5 | mu, sg));
    return log(p) + lognormal_lccdf(10 | mu, sg);
  }
  real tier_ge_log(int k, real p, real mu, real sg) {   // log P(tier >= k), k >= 2
    real cutv = k == 2 ? 0 : (k == 3 ? 2 : (k == 4 ? 5 : 10));
    if (k == 2) return log(p);
    return log(p) + lognormal_lccdf(cutv | mu, sg);
  }
}
data {
  int<lower=1> N; int<lower=1> K; int<lower=1> G; int<lower=1> S; int<lower=1> Y; int<lower=1> O;
  matrix[N, K] B; array[N] int<lower=1, upper=G> g; array[N] int<lower=1, upper=S> s; vector[N] age_c;
  array[N] int<lower=1, upper=Y> yr; array[N] int<lower=1, upper=O> org;
  array[N] int<lower=0, upper=1> cens; vector[N] v; array[N] int<lower=1, upper=5> tier;
  vector[N] td;      // seasons from draft to debut, centered (t_debut - 4)
  int<lower=0, upper=1> prior_only;
}
parameters {
  vector[K] cp_z; vector[K] cm_z; real<lower=0> sig_rw_p; real<lower=0> sig_rw_m;
  matrix[G, K] dp_raw; matrix[G, K] dm_raw; real<lower=0> tau_p; real<lower=0> tau_m;
  vector[S] ba_p; vector[S] ba_m;
  real<lower=0> sy_p; real<lower=0> sy_m; vector[Y] zy_p; vector[Y] zy_m;
  real<lower=0> so_p; real<lower=0> so_m; vector[O] zo_p; vector[O] zo_m;
  vector<lower=0>[G] sigma; real<lower=0> lambda_neg;
  vector[S] bt_p; real bt2_p; vector[S] bt_m; real bt2_m;
  vector[G] a_p_raw; vector[G] a_m_raw;     // unshrunk type intercepts (sum-to-zero)
}
transformed parameters {
  vector[N] eta_p; vector[N] mu; vector[K] cp; vector[K] cm; matrix[G, K] dp; matrix[G, K] dm;
  vector[G] a_p = a_p_raw - mean(a_p_raw); vector[G] a_m = a_m_raw - mean(a_m_raw);
  cp[1] = 1.5 * cp_z[1]; cm[1] = 0.5 + 1.5 * cm_z[1];
  for (k in 2:K) { cp[k] = cp[k - 1] + sig_rw_p * cp_z[k]; cm[k] = cm[k - 1] + sig_rw_m * cm_z[k]; }
  for (gg in 1:G) { dp[gg] = tau_p * cumulative_sum(dp_raw[gg]); dm[gg] = tau_m * cumulative_sum(dm_raw[gg]); }
  {
    matrix[N, G] BP = B * dp'; matrix[N, G] BM = B * dm';
    for (i in 1:N) { eta_p[i] = B[i] * cp + BP[i, g[i]]; mu[i] = B[i] * cm + BM[i, g[i]]; }
  }
  eta_p += a_p[g] + ba_p[s] .* age_c + bt_p[s] .* td + bt2_p * square(td) + sy_p * zy_p[yr] + so_p * zo_p[org];
  mu += a_m[g] + ba_m[s] .* age_c + bt_m[s] .* td + bt2_m * square(td) + sy_m * zy_m[yr] + so_m * zo_m[org];
}
model {
  cp_z ~ std_normal(); cm_z ~ std_normal(); sig_rw_p ~ normal(0, 0.5); sig_rw_m ~ normal(0, 0.5);
  to_vector(dp_raw) ~ std_normal(); to_vector(dm_raw) ~ std_normal();
  tau_p ~ normal(0, 0.3); tau_m ~ normal(0, 0.3);
  ba_p ~ normal(0, 0.5); ba_m ~ normal(0, 0.5);
  sy_p ~ normal(0, 0.3); sy_m ~ normal(0, 0.3); so_p ~ normal(0, 0.3); so_m ~ normal(0, 0.3);
  zy_p ~ std_normal(); zy_m ~ std_normal(); zo_p ~ std_normal(); zo_m ~ std_normal();
  sigma ~ normal(1, 0.5); lambda_neg ~ normal(2, 1);
  a_p_raw ~ normal(0, 1); a_m_raw ~ normal(0, 1);
  bt_p ~ normal(0, 0.5); bt2_p ~ normal(0, 0.2); bt_m ~ normal(0, 0.5); bt2_m ~ normal(0, 0.2);
  if (!prior_only) {
    for (i in 1:N) {
      if (!cens[i]) {
        if (v[i] > 0) target += log_inv_logit(eta_p[i]) + lognormal_lpdf(v[i] | mu[i], sigma[g[i]]);
        else target += log1m_inv_logit(eta_p[i]) + exponential_lpdf(-v[i] | lambda_neg);
      } else {
        if (v[i] > 0) target += log_inv_logit(eta_p[i]) + lognormal_lccdf(v[i] | mu[i], sigma[g[i]]);
        else if (v[i] == 0) target += log_inv_logit(eta_p[i]);
        else target += log_sum_exp(log_inv_logit(eta_p[i]), log1m_inv_logit(eta_p[i]) + exponential_lcdf(-v[i] | lambda_neg));
      }
    }
  }
}
generated quantities {
  vector[N] log_lik; vector[N] log_lik_tier;
  for (i in 1:N) {
    real p = inv_logit(eta_p[i]);
    if (!cens[i]) {
      log_lik[i] = v[i] > 0 ? log(p) + lognormal_lpdf(v[i] | mu[i], sigma[g[i]]) : log1m(p) + exponential_lpdf(-v[i] | lambda_neg);
      log_lik_tier[i] = tier_lpmf(tier[i] | p, mu[i], sigma[g[i]], lambda_neg);
    } else {
      log_lik[i] = v[i] > 0 ? log(p) + lognormal_lccdf(v[i] | mu[i], sigma[g[i]])
                            : (v[i] == 0 ? log(p) : log_sum_exp(log(p), log1m(p) + exponential_lcdf(-v[i] | lambda_neg)));
      log_lik_tier[i] = tier[i] >= 2 ? tier_ge_log(tier[i], p, mu[i], sigma[g[i]]) : 0;
    }
  }
}
