// Stage 6: probability that a drafted player does not sign (rounds 1-20, 2014-25 excl. 2020).
// logit P(unsigned) = round-group x school intercepts (partially pooled toward round-group means) + fall (pre-draft rank
// better than pick) + not-in-top-200 + draft-eligible sophomore + cold-weather region + age within school + draft-year RE.
data {
  int<lower=1> N; array[N] int<lower=0, upper=1> y;
  int<lower=1> R; array[N] int<lower=1, upper=R> rg; int<lower=1> S; array[N] int<lower=1, upper=S> sc;
  vector[N] fall; vector[N] unr; vector[N] so; vector[N] cold; vector[N] age_c;
  int<lower=1> Y; array[N] int<lower=1, upper=Y> yr;
}
parameters {
  vector[R] a_r; matrix[R, S] z_rs; real<lower=0> sd_rs; vector[S] a_s_raw;
  real b_fall; real b_unr; real b_so; real b_cold; vector[S] b_age; real<lower=0> sd_y; vector[Y] z_y;
}
transformed parameters {
  vector[S] a_s = a_s_raw - mean(a_s_raw);
}
model {
  a_r ~ normal(-3, 2); to_vector(z_rs) ~ std_normal(); sd_rs ~ normal(0, 1); a_s_raw ~ normal(0, 1.5);
  b_fall ~ normal(0, 1); b_unr ~ normal(0, 1); b_so ~ normal(0, 1); b_cold ~ normal(0, 1); b_age ~ normal(0, 0.5);
  sd_y ~ normal(0, 0.5); z_y ~ std_normal();
  {
    vector[N] eta;
    for (i in 1:N) eta[i] = a_r[rg[i]] + a_s[sc[i]] + sd_rs * z_rs[rg[i], sc[i]] + b_fall * fall[i] + b_unr * unr[i] + b_so * so[i]
                            + b_cold * cold[i] + b_age[sc[i]] * age_c[i] + sd_y * z_y[yr[i]];
    y ~ bernoulli_logit(eta);
  }
}
