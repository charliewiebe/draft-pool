// Stage 7 decision engine: one focal team's rounds 1-10 picks inside a simulated draft market.
//
// Other teams take players in the scenario's priority order `pi` (the order in which the calibrated market simulator
// filled every slot, followed by unpicked in-market players by latent position and then signable out-of-market players
// by rank). The focal team holds the slots in `fk`; at each, it chooses among all players not yet taken.
// Each player's ask at a pick comes from the deal model with player-fixed quantiles (u1 for the deal component, z2 for
// the magnitude), so a player's ask is consistent across picks. The focal team pays the ask (bonus-as-value, I1).
//
// Decision rules
//   (mode may differ by pick, e.g. a fixed first-pick cap followed by the Lagrangian rule)
//   mode 0 (cap):        among players whose ask <= cap_f * slot and fits the budget, take the highest ask
//                        (highest market value affordable); optional school/type restriction.
//   mode 1 (Lagrangian): take the player maximizing (1 - pf) * (Vbar(ask) - lambda * ask) + pf * comp_f among those that
//                        fit the budget, where Vbar is the posterior-mean value surface (or the ask itself under I2).
// Budget: planned spend never exceeds budget - reserve, where reserve = sum over later focal picks of rmin * slot.
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

static inline double z_to_ratio(int comp, double z) {
  const double lo = std::log(0.25), hi = std::log(0.985);
  if (comp == 0) return 0.25 * std::exp(-std::exp(z));
  if (comp == 1) return std::exp(lo + (hi - lo) / (1.0 + std::exp(-z)));
  if (comp == 2) return 1.0;
  return 1.015 + std::exp(z);
}

// V flat index: ((g * 3 + age_idx) * nb + bonus_idx); log-bonus grid lbg ascending
static double vlook(const NumericVector& V, int g, double agec, double lb, const NumericVector& lbg) {
  int nb = lbg.size();
  double a = std::min(1.0, std::max(-1.0, agec));
  int a0; double wa;
  if (a < 0) { a0 = 0; wa = a + 1.0; } else { a0 = 1; wa = a; }
  int j; double wb;
  if (lb <= lbg[0]) { j = 0; wb = 0.0; }
  else if (lb >= lbg[nb - 1]) { j = nb - 2; wb = 1.0; }
  else {
    j = (int)(std::upper_bound(lbg.begin(), lbg.end(), lb) - lbg.begin()) - 1;
    wb = (lb - lbg[j]) / (lbg[j + 1] - lbg[j]);
  }
  auto at = [&](int ai, int bi) { return V[(g * 3 + ai) * nb + bi]; };
  double v0 = (1 - wb) * at(a0, j) + wb * at(a0, j + 1);
  double v1 = (1 - wb) * at(a0 + 1, j) + wb * at(a0 + 1, j + 1);
  return (1 - wa) * v0 + wa * v1;
}

// [[Rcpp::export]]
List engine_eval(List sc, List fc, List pol, List val) {
  // scenario
  IntegerVector pi = sc["pi"];                 // 0-based player ids, priority order for other teams (hard no-signs excluded)
  int L = as<int>(sc["L"]);
  NumericVector slot = sc["slot"], lpc = sc["lpc"];
  NumericMatrix Bw = sc["Bw"], Bmu = sc["Bmu"];   // L x 3 (deep, under, over)
  IntegerVector rbin = sc["rbin"];              // 0..3 calibration round bin of each pick
  IntegerVector g = sc["g"], school = sc["school"], ranked = sc["ranked"];
  NumericVector rank = sc["rank"], agec = sc["agec"], sr = sc["sr"], u1 = sc["u1"], z2 = sc["z2"], u3 = sc["u3"];
  NumericMatrix wg = sc["wg"], wgs = sc["wgs"], wx = sc["wx"], mg = sc["mg"], mx = sc["mx"];   // 3 x 6, 3 x 3
  NumericVector wsr = sc["wsr"], wyre = sc["wyre"], ls0 = sc["ls0"], ls1 = sc["ls1"], off_over = sc["off_over"], off_deep = sc["off_deep"];
  double msr = as<double>(sc["msr"]), ore = as<double>(sc["ore"]);
  int npool = g.size();
  // focal team
  IntegerVector fk = fc["fk"];                  // 0-based pick indices held by the focal team (rounds 1-10), ascending
  IntegerVector frg = fc["frg"];                // signing round group index (0..3) of each focal pick
  NumericVector comp = fc["comp"];              // value of a compensation pick if this pick fails to sign (0 if none)
  int nf = fk.size();
  // policy
  IntegerVector modev = pol["mode"];               // per focal pick (length 1 = same rule everywhere)
  NumericVector cap = pol["cap"], rmin = pol["rmin"];
  IntegerVector tr = pol["type_restrict"], srs = pol["school_restrict"];
  double lambda = as<double>(pol["lambda"]), budget = as<double>(pol["budget"]);
  int use_fail = as<int>(pol["use_fail"]); double beta = as<double>(pol["beta"]);
  NumericVector alpha = pol["alpha"];           // 4 round groups x 3 schools (row-major: rg * 3 + school)
  int i2_decide = as<int>(pol["i2_decide"]);
  // value surfaces
  NumericVector lbg = val["lbg"], Vbar = val["Vbar"], Vd = val["Vd"], Vp = val["Vp"];
  int i2_eval = as<int>(val["i2_eval"]);

  std::vector<char> taken(npool, 0);
  int ptr = 0, f = 0;
  IntegerVector pid(nf); NumericVector ask(nf), pf(nf), vbar(nf), ve(nf), vp(nf); LogicalVector signedv(nf);
  double planned = 0.0;
  for (int k = 0; k < L; ++k) {
    bool focal = (f < nf && fk[f] == k);
    if (!focal) {
      while (ptr < (int)pi.size() && taken[pi[ptr]]) ++ptr;
      if (ptr < (int)pi.size()) taken[pi[ptr]] = 1;
      continue;
    }
    double reserve = 0.0;
    for (int f2 = f + 1; f2 < nf; ++f2) reserve += rmin[f2] * slot[fk[f2]];
    double room = budget - planned - reserve;
    int best = -1; double best_score = -1e300, best_ask = 0, best_pf = 0;
    int cheap = -1; double cheap_ask = 1e300, cheap_pf = 0;
    for (int q = 0; q < (int)pi.size(); ++q) {
      int i = pi[q];
      if (taken[i]) continue;
      if (tr[f] >= 0 && g[i] != tr[f]) continue;
      if (srs[f] >= 0 && school[i] != srs[f]) continue;
      double fall = 0, rise = 0, unr = 1;
      if (ranked[i]) { double lr = std::log((k + 1.0) / rank[i]); fall = std::max(0.0, lr); rise = std::max(0.0, -lr); unr = 0; }
      double e[4];
      for (int c = 0; c < 3; ++c) {
        int col = (c == 2) ? 3 : c;
        e[col] = Bw(k, c) + wg(c, g[i]) + wgs(c, g[i]) * lpc[k] + wsr[c] * sr[i] + wyre[c] + wx(c, 0) * fall + wx(c, 1) * rise + wx(c, 2) * unr;
      }
      e[2] = 0.0; e[0] += off_deep[rbin[k]]; e[3] += off_over[rbin[k]];
      double m = std::max(std::max(e[0], e[1]), std::max(e[2], e[3])), s = 0, p[4];
      for (int c = 0; c < 4; ++c) { p[c] = std::exp(e[c] - m); s += p[c]; }
      int cp = 3; double acc = 0;
      for (int c = 0; c < 4; ++c) { acc += p[c] / s; if (u1[i] <= acc) { cp = c; break; } }
      double ratio = 1.0;
      if (cp != 2) {
        int ci = (cp == 3) ? 2 : cp;
        double mu = Bmu(k, ci) + mg(ci, g[i]) + mx(ci, 0) * fall + mx(ci, 1) * rise + mx(ci, 2) * unr;
        if (ci == 0) mu += msr * sr[i];
        if (ci == 2) mu += ore;
        double sd = std::exp(ls0[ci] + ls1[ci] * lpc[k]);
        ratio = z_to_ratio(cp, mu + sd * z2[i]);
      }
      double a = ratio * slot[k];
      double pfi = 0.0;
      if (use_fail) { double eta = alpha[frg[f] * 3 + school[i]] + beta * std::log(std::max(ratio, 1.0)); pfi = 1.0 / (1.0 + std::exp(-eta)); }
      if (a < cheap_ask) { cheap_ask = a; cheap = i; cheap_pf = pfi; }
      if (a > room) continue;
      double score;
      int mode = modev.size() > 1 ? modev[f] : modev[0];
      if (mode == 0) { if (a > cap[f] * slot[k]) continue; score = a - 1e-9 * q; }
      else {
        double vb = i2_decide ? a : vlook(Vbar, g[i], agec[i], std::log(a), lbg);
        score = (1.0 - pfi) * (vb - lambda * a) + pfi * comp[f];
      }
      if (score > best_score) { best_score = score; best = i; best_ask = a; best_pf = pfi; }
    }
    if (best < 0) { best = cheap; best_ask = cheap_ask; best_pf = cheap_pf; }
    taken[best] = 1; planned += best_ask;
    pid[f] = best; ask[f] = best_ask; pf[f] = best_pf;
    signedv[f] = !(use_fail && u3[best] < best_pf);
    double lb = std::log(best_ask);
    vbar[f] = vlook(Vbar, g[best], agec[best], lb, lbg);
    ve[f] = i2_eval ? best_ask : vlook(Vd, g[best], agec[best], lb, lbg);
    vp[f] = i2_eval ? best_ask : vlook(Vp, g[best], agec[best], lb, lbg);
    ++f;
  }
  return List::create(_["pid"] = pid, _["ask"] = ask, _["pf"] = pf, _["signed"] = signedv, _["vbar"] = vbar, _["ve"] = ve, _["vp"] = vp);
}
