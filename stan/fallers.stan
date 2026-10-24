// Stage 5.2: where a ranked prospect is taken. y = log(overall pick) if taken in rounds 1-10, else right-censored at log(L).
// Two components: with prob pi the player is out of the rounds 1-10 market (signability / college-bound);
// otherwise log pick ~ normal(mu, sigma), mu = spline(log rank) + type + draft-year RE,
// log sigma linear in log rank + school; logit pi linear in log rank + type + draft-year RE.
data {
  int<lower=1> N; int<lower=1> K; matrix[N, K] B; vector[N] lr;
  int<lower=1> G; array[N] int<lower=1, upper=G> g; int<lower=1> S; array[N] int<lower=1, upper=S> s;
  int<lower=1> Y; array[N] int<lower=1, upper=Y> yr;
  array[N] int<lower=0, upper=1> obs; vector[N] y; vector[N] logL;
}
parameters {
  vector[K] bz; real<lower=0> b_sd; vector[G] m_g_raw; real<lower=0> m_ysd; vector[Y] m_yz;
  real s0; real s1; vector[S] s_s_raw;
  real p0; real p1; vector[G] p_g_raw; real<lower=0> p_ysd; vector[Y] p_yz;
}
transformed parameters {
  vector[K] bm; vector[G] m_g = m_g_raw - mean(m_g_raw); vector[S] s_s = s_s_raw - mean(s_s_raw); vector[G] p_g = p_g_raw - mean(p_g_raw);
  bm[1] = 4 * bz[1]; for (k in 2:K) bm[k] = bm[k - 1] + b_sd * bz[k];
}
model {
  bz[1] ~ std_normal(); bz[2:K] ~ std_normal(); b_sd ~ normal(0, 1);
  m_g_raw ~ normal(0, 0.5); m_ysd ~ normal(0, 0.3); m_yz ~ std_normal();
  s0 ~ normal(-0.5, 1); s1 ~ normal(0, 0.5); s_s_raw ~ normal(0, 0.5);
  p0 ~ normal(-2, 1.5); p1 ~ normal(0, 1); p_g_raw ~ normal(0, 1); p_ysd ~ normal(0, 0.5); p_yz ~ std_normal();
  {
    vector[N] mu = B * bm + m_g[g] + m_ysd * m_yz[yr];
    vector[N] sg = exp(s0 + s1 * lr + s_s[s]);
    vector[N] lpi = p0 + p1 * lr + p_g[g] + p_ysd * p_yz[yr];
    for (i in 1:N) {
      if (obs[i]) target += log1m_inv_logit(lpi[i]) + normal_lpdf(y[i] | mu[i], sg[i]);
      else target += log_sum_exp(log_inv_logit(lpi[i]), log1m_inv_logit(lpi[i]) + normal_lccdf(logL[i] | mu[i], sg[i]));
    }
  }
}
