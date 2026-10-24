// Control-period WAR tier given MLB arrival: ordinal logistic.
//   tiers 1: V<=0, 2: (0,2], 3: (2,5], 4: (5,10], 5: >10   (6-season control window; FanGraphs rule-of-thumb scale)
//   eta: P-spline in x (common + group deviations) + age by school + debut timing (linear by school + quadratic)
//        + draft-year RE + org RE. Debut timing is required: recent classes' arrivals are selected fast debuts.
//   censored: partial-sum tier as a lower bound, P(Y >= k)
functions {
  real rw_lpdf(vector v, real scale) { return normal_lpdf(v[2:rows(v)] - v[1:(rows(v) - 1)] | 0, scale); }
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
  vector[K] c_z; real<lower=0> sig_rw; matrix[G, K] d_raw; real<lower=0> tau;
  vector[S] ba; real<lower=0> sy; vector[Y] zy; real<lower=0> so; vector[O] zo;
  ordered[4] cut;
  vector[S] b_td; real b_td2;
  vector[G] a_raw;   // unshrunk type (school x role) intercepts (sum-to-zero; level is carried by the cutpoints)
  real<lower=cut[3] - cut[4]> dtop_p;   // pitcher shift of the top cutpoint (partial proportional odds for tier > 10)
}
transformed parameters {
  vector[N] eta; vector[K] c; matrix[G, K] d; vector[G] a_g = a_raw - mean(a_raw);
  c[1] = 1.5 * c_z[1];
  for (k in 2:K) c[k] = c[k - 1] + sig_rw * c_z[k];
  for (gg in 1:G) d[gg] = tau * cumulative_sum(d_raw[gg]);
  {
    matrix[N, G] BD = B * d';
    for (i in 1:N) eta[i] = B[i] * c + BD[i, g[i]];
  }
  eta += a_g[g] + ba[s] .* age_c + b_td[s] .* td + b_td2 * square(td) + sy * zy[yr] + so * zo[org];
}
model {
  array[2] vector[4] C; C[1] = cut; C[2] = cut; C[1][4] = cut[4] + dtop_p;     // C[1]: pitchers, C[2]: hitters
  c_z ~ std_normal(); sig_rw ~ normal(0, 0.5);
  to_vector(d_raw) ~ std_normal();
  tau ~ normal(0, 0.3); ba ~ normal(0, 0.5);
  sy ~ normal(0, 0.3); zy ~ std_normal(); so ~ normal(0, 0.3); zo ~ std_normal();
  cut ~ normal(0, 3); b_td ~ normal(0, 0.5); b_td2 ~ normal(0, 0.2); a_raw ~ normal(0, 1); dtop_p ~ normal(0, 1);
  if (!prior_only) {
    for (i in 1:N) {
      int r = (g[i] % 2 == 1) ? 1 : 2;            // odd groups (HS-P, 4yr-P, JUCO-P) are pitchers
      if (!cens[i]) tier[i] ~ ordered_logistic(eta[i], C[r]);
      else if (tier[i] >= 2) target += log_inv_logit(eta[i] - C[r][tier[i] - 1]);
    }
  }
}
generated quantities {
  vector[N] log_lik_tier;
  {
    array[2] vector[4] C; C[1] = cut; C[2] = cut; C[1][4] = cut[4] + dtop_p;
    for (i in 1:N) {
      int r = (g[i] % 2 == 1) ? 1 : 2;
      if (!cens[i]) log_lik_tier[i] = ordered_logistic_lpmf(tier[i] | eta[i], C[r]);
      else log_lik_tier[i] = tier[i] >= 2 ? log_inv_logit(eta[i] - C[r][tier[i] - 1]) : 0;
    }
  }
}
