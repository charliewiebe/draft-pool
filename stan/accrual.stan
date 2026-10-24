// Season-level accrual of control-period value after MLB debut.
//   state_s in {1 none, 2 MiLB only, 3 MLB}: categorical logit (reference = none) on seasons since last MLB season (1 = played
//   last season), MiLB-vs-none last season when not in MLB, MLB seasons so far,
//   seasons since last MLB season, last-MLB-season WAR, mean WAR per MLB season, age, role, calendar-season RE.
//   WAR | MLB season: exponentially modified normal (normal + exponential upside; right-skewed like season WAR), with the
//   normal location, normal log-scale and exponential log-mean on the same history; 2020 rows scaled (60-game season).
data {
  int<lower=1> N; array[N] int<lower=1, upper=3> state; array[N] int<lower=1, upper=3> prev;
  array[N] int<lower=1, upper=5> kp; array[N] int<lower=1, upper=4> gapc;
  vector[N] lastw; vector[N] meanw; vector[N] age; vector[N] hit;
  int<lower=1> Y; array[N] int<lower=1, upper=Y> yr;
  int<lower=1> NW; array[NW] int<lower=1, upper=N> iw; vector[NW] war; vector[NW] y2020; int<lower=1, upper=Y> y20idx;
}
transformed data {
  vector[NW] lw = lastw[iw]; vector[NW] mw = meanw[iw]; vector[NW] ag = age[iw]; vector[NW] ht = hit[iw];
  vector[NW] gp; vector[NW] lwp = fmax(lw, 0); vector[NW] mwp = fmax(mw, 0);
  array[NW] int kw;
  vector[N] is1; vector[N] is2; vector[N] is3; vector[N] y20all;
  for (i in 1:N) { is1[i] = state[i] == 1; is2[i] = state[i] == 2; is3[i] = state[i] == 3; y20all[i] = yr[i] == y20idx; }
  for (i in 1:NW) { gp[i] = gapc[iw[i]] > 1; kw[i] = kp[iw[i]]; }
}
parameters {
  vector[2] a0; vector[2] a_milb; matrix[2, 4] a_k; matrix[2, 3] a_gap;
  vector[2] b_l; vector[2] b_m; vector[2] b_age; vector[2] b_age2; vector[2] b_hit;
  vector<lower=0>[2] sig_s; matrix[2, Y] z_s;
  real c0; vector[4] c_k; real c_l; real c_m; real c_age; real c_age2; real c_hit; real c_gap;
  vector[2] s0; real s_m; real s_l; vector[2] e0; real e_m; real e_l;
  real<lower=0, upper=1> r20; real<lower=0, upper=1.5> rs20;
}
model {
  a0 ~ normal(0, 2); a_milb ~ normal(0, 2); to_vector(a_k) ~ normal(0, 1); to_vector(a_gap) ~ normal(0, 2);
  b_l ~ normal(0, 1); b_m ~ normal(0, 1); b_age ~ normal(0, 0.5); b_age2 ~ normal(0, 0.1); b_hit ~ normal(0, 1);
  sig_s ~ normal(0, 0.5); to_vector(z_s) ~ std_normal();
  c0 ~ normal(0, 1); c_k ~ normal(0, 0.5); c_l ~ normal(0, 0.5); c_m ~ normal(0, 0.5); c_age ~ normal(0, 0.3);
  c_age2 ~ normal(0, 0.05); c_hit ~ normal(0, 0.5); c_gap ~ normal(0, 0.5);
  s0 ~ normal(0, 1); s_m ~ normal(0, 0.3); s_l ~ normal(0, 0.3); e0 ~ normal(-1, 1); e_m ~ normal(0, 0.3); e_l ~ normal(0, 0.3);
  r20 ~ beta(3.7, 6.3); rs20 ~ normal(0.6, 0.3);
  {
    array[2] vector[N] e;
    for (c in 1:2) {
      vector[3] vp = [0, a_milb[c], 0]'; vector[5] vk = append_row(0, a_k[c]'); vector[4] vg = append_row(0, a_gap[c]');
      e[c] = a0[c] + b_l[c] * lastw + b_m[c] * meanw + b_age[c] * age + b_age2[c] * square(age) + b_hit[c] * hit
             + sig_s[c] * to_vector(z_s[c, yr]) + vp[prev] + vk[kp] + vg[gapc];
    }
    // 2020: no minor-league season, so the MiLB-only state is impossible
    vector[N] e1 = e[1] - 1e3 * y20all;
    vector[N] m = fmax(fmax(e1, e[2]), 0);
    vector[N] lse = m + log(exp(-m) + exp(e1 - m) + exp(e[2] - m));
    target += sum(is1 .* (-lse)) + sum(is2 .* (e1 - lse)) + sum(is3 .* (e[2] - lse));
  }
  {
    vector[NW] mu = c0 + c_l * lw + c_m * mw + c_age * ag + c_age2 * square(ag) + c_hit * ht + c_gap * gp;
    vector[NW] sg = exp(s0[1] * (1 - ht) + s0[2] * ht + s_m * mwp + s_l * lwp);
    for (i in 1:NW) if (kw[i] > 1) mu[i] += c_k[kw[i] - 1];
    vector[NW] em = exp(e0[1] * (1 - ht) + e0[2] * ht + e_m * mwp + e_l * lwp);     // mean of the exponential upside
    mu = mu .* (1 - y2020 + y2020 * r20);
    sg = sg .* (1 - y2020 + y2020 * rs20);
    em = em .* (1 - y2020 + y2020 * r20);
    war ~ exp_mod_normal(mu, sg, inv(em));
  }
}
