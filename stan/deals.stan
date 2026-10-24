// Stage 5.1: deal distribution by pick. ratio = bonus / slot for signed rounds 1-10 picks.
// Components are observed: 1 deep under (< 0.25, senior-sign range), 2 under [0.25, 0.985), 3 at slot [0.985, 1.015],
// 4 over (> 1.015). Component weights: categorical logit (reference = at slot) with a random-walk B-spline in log pick
// + type + type-specific slope in centered log pick + senior + draft-year RE + pre-draft standing:
//   fall = max(0, log(pick / rank)), rise = max(0, log(rank / pick)) for players in the top-200 list, unr = not in it.
// Magnitudes, each mapped to the real line (data transforms, so the likelihood is a plain normal regression):
//   deep:  z = log(log(0.25 / ratio))                  ratio = 0.25 * exp(-exp(z))
//   under: z = logit((log ratio - log 0.25) / (log 0.985 - log 0.25))
//   over:  z = log(ratio - 1.015)                      ratio = 1.015 + exp(z)   (lognormal excess over slot)
// z ~ normal(spline(log pick) + type (+ senior for deep) (+ year RE for over), exp(ls0 + ls1 * centered log pick)).
data {
  int<lower=1> N; int<lower=1> K; matrix[N, K] B; vector[N] lp;
  int<lower=1> G; array[N] int<lower=1, upper=G> g; vector[N] sr;
  vector[N] fall; vector[N] rise; vector[N] unr;
  int<lower=1> Y; array[N] int<lower=1, upper=Y> yr;
  array[N] int<lower=1, upper=4> comp; vector[N] z;          // z = component-specific transformed magnitude (0 for at slot)
  int<lower=0, upper=1> prior_only;
}
transformed data {
  array[4] int n_c = rep_array(0, 4);
  for (i in 1:N) n_c[comp[i]] += 1;
  array[n_c[1]] int i1; array[n_c[2]] int i2; array[n_c[4]] int i4;
  { int a = 1; int b = 1; int d = 1;
    for (i in 1:N) { if (comp[i] == 1) { i1[a] = i; a += 1; } else if (comp[i] == 2) { i2[b] = i; b += 1; } else if (comp[i] == 4) { i4[d] = i; d += 1; } } }
}
parameters {
  matrix[3, K] w_z; vector<lower=0>[3] w_sd; matrix[3, G] w_g_raw; matrix[3, G] w_gs_raw; vector[3] w_sr; vector<lower=0>[3] w_ysd; matrix[3, Y] w_yz;
  vector<lower=0>[3] mu_sd; matrix[3, K] mu_dz;      // magnitude splines: 1 deep, 2 under, 3 over
  matrix[3, G] m_g_raw; real m_sr; real<lower=0> o_ysd; vector[Y] o_yz;
  vector[3] ls0; vector[3] ls1;
  matrix[3, 3] w_x;      // weights: rows = component (deep, under, over), cols = fall, rise, unr
  matrix[3, 3] m_x;      // magnitudes: rows = component, cols = fall, rise, unr
}
transformed parameters {
  matrix[3, K] w; matrix[3, K] mu_c; matrix[3, G] w_g; matrix[3, G] w_gs; matrix[3, G] m_g;
  for (c in 1:3) {
    w[c, 1] = 2 * w_z[c, 1]; for (k in 2:K) w[c, k] = w[c, k - 1] + w_sd[c] * w_z[c, k];
    mu_c[c, 1] = 2 * mu_dz[c, 1]; for (k in 2:K) mu_c[c, k] = mu_c[c, k - 1] + mu_sd[c] * mu_dz[c, k];
    w_g[c] = w_g_raw[c] - mean(w_g_raw[c]); w_gs[c] = w_gs_raw[c] - mean(w_gs_raw[c]); m_g[c] = m_g_raw[c] - mean(m_g_raw[c]);
  }
}
model {
  to_vector(w_z) ~ std_normal(); w_sd ~ normal(0, 0.5); to_vector(w_g_raw) ~ normal(0, 1.5); to_vector(w_gs_raw) ~ normal(0, 1); w_sr ~ normal(0, 2);
  w_ysd ~ normal(0, 0.5); to_vector(w_yz) ~ std_normal();
  mu_sd ~ normal(0, 0.3); to_vector(mu_dz) ~ std_normal();
  to_vector(m_g_raw) ~ normal(0, 0.5); m_sr ~ normal(0, 1); o_ysd ~ normal(0, 0.3); o_yz ~ std_normal();
  ls0 ~ normal(0, 1); ls1 ~ normal(0, 0.5);
  to_vector(w_x) ~ normal(0, 1.5); to_vector(m_x) ~ normal(0, 1);
  if (!prior_only) {
    matrix[N, 4] eta = rep_matrix(0, N, 4);
    for (c in 1:3) {
      int col = c == 3 ? 4 : c;
      eta[, col] = B * w[c]' + w_g[c, g]' + w_gs[c, g]' .* lp + w_sr[c] * sr + w_ysd[c] * w_yz[c, yr]'
                   + w_x[c, 1] * fall + w_x[c, 2] * rise + w_x[c, 3] * unr;
    }
    for (i in 1:N) target += eta[i, comp[i]] - log_sum_exp(eta[i]);
    matrix[N, 3] X = append_col(append_col(fall, rise), unr);
    z[i1] ~ normal(B[i1] * mu_c[1]' + m_g[1, g[i1]]' + m_sr * sr[i1] + X[i1] * m_x[1]', exp(ls0[1] + ls1[1] * lp[i1]));
    z[i2] ~ normal(B[i2] * mu_c[2]' + m_g[2, g[i2]]' + X[i2] * m_x[2]', exp(ls0[2] + ls1[2] * lp[i2]));
    z[i4] ~ normal(B[i4] * mu_c[3]' + m_g[3, g[i4]]' + o_ysd * o_yz[yr[i4]] + X[i4] * m_x[3]', exp(ls0[3] + ls1[3] * lp[i4]));
  }
}
