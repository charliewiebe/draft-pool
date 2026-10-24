// Stage 5.2: signing bonus of a ranked prospect given where he was taken.
// log bonus (2025 schedule $) ~ Student-t(nu, a + bp * log slot(pick) + br * log slot(rank) + type + senior + year RE, sigma_type)
// bp + br ~ 1 means bonuses scale with the slot scale; br / (bp + br) is the weight of pre-draft standing over pick location.
data {
  int<lower=1> N; vector[N] lb; vector[N] ls_pick; vector[N] ls_rank; int<lower=1> G; array[N] int<lower=1, upper=G> g;
  int<lower=1> Y; array[N] int<lower=1, upper=Y> yr; vector[N] sr; real ls_mean;
}
parameters {
  real a; real bp; real br; vector[G] a_g_raw; real a_sr; real<lower=0> ysd; vector[Y] yz;
  vector[G] ls_g; real<lower=1> nu;
}
transformed parameters { vector[G] a_g = a_g_raw - mean(a_g_raw); }
model {
  a ~ normal(ls_mean, 2); bp ~ normal(0.5, 0.5); br ~ normal(0.5, 0.5); a_g_raw ~ normal(0, 0.5); a_sr ~ normal(0, 1);
  ysd ~ normal(0, 0.3); yz ~ std_normal(); ls_g ~ normal(-1, 1); nu ~ gamma(2, 0.2);
  lb ~ student_t(nu, a + bp * ls_pick + br * ls_rank + a_g[g] + a_sr * sr + ysd * yz[yr], exp(ls_g[g]));
}
generated quantities { real w_rank = br / (bp + br); }
