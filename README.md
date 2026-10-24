# Spending the Pool

A Bayesian decision model for allocating an MLB Rule 4 draft bonus pool, built in R, Stan and C++ (Rcpp) from public data.

**[Read the report](https://charliewiebe.github.io/draft-pool/analysis.html)**: methods, key code, model checks, results and recommendations.
The report source is `analysis.Rmd`; it renders from the committed checkpoint tables in `data/derived/` without refitting
(`rmarkdown::render("analysis.Rmd")`).

Status: Stages 1-7 complete (value curve, supply and deal models, calibrated draft-market simulator, signing-risk model,
and the decision model answering Q1-Q7).

## Environment
- Toolchain is pinned by `environment.yml` (conda-forge: R 4.5, CmdStan, cmdstanr, brms, targets, quarto).
  CRAN binaries were not reachable from the build host, so conda supplies R packages; `renv.lock` records
  the resulting R package versions.
- Create: `micromamba create -f environment.yml`, then run R inside `micromamba run -n draftpool`.

## Run
```r
targets::tar_make()                       # raw downloads are immutable; existing files are never re-requested
testthat::test_dir("tests/testthat")
```

## Layout
- `R/` collectors, parsers, rules table, slots/pools, reconciliation, checkpoint diagnostics
- `data/raw/` Stats API JSON, B-Ref HTML, MLB.com/BA articles, MLB Pipeline pages, Chadwick register (git clone),
  `request_log.csv` (url, timestamp, status, bytes, sha256 for every live request)
- `data/derived/` `rules_by_year.csv`, `slot_values.csv`, `pool_reconstruction.csv`, `picks.csv`,
  `bonus_reconciliation.csv`, `player_seasons.csv`, `pipeline_rankings.csv`, `checkpoint/*.csv`
- `data/derived/checkpoint4/`, `checkpoint5/` checkpoint tables; `data/derived/stage4/` (2012-16 value classes) and
  `data/derived/stage4_refit/` (2012-21 value classes) value-curve primitives as Parquet (per-draw grids are gitignored)
- `R/accrual.R` season accrual model + imputation; `R/supply.R` deal, faller, price and class-depth models; `R/simulator.R` draft-market simulator
- `R/signing.R` signing-risk model and decision-point failure model; `src/engine.cpp` (Rcpp) focal-team decision engine;
  `R/decision.R` scenarios, value banks, policies, accounting (tax bands, forfeits, compensation picks); `R/experiments7.R` Q2-Q7 experiments
- `data/derived/checkpoint7/` Stage 6-7 tables. The `ce_g02`/`ce_g05`/`sd_realized` columns of the policy tables use a cell-shared
  realization and are superseded by `risk_player_level.csv` (player-level outcome draws)
- `DATA_DICTIONARY.md` every derived column

## Sources and terms
Public sources only: MLB Stats API, MLB.com / MLB Pipeline pages, Baseball America free articles,
Baseball-Reference (draft pages, WAR tables; requests spaced >= 4 s per Sports Reference bot policy),
Chadwick Bureau register. FanGraphs data pages returned HTTP 403 (bot protection) and were not scraped; the Q6 trade-value
view uses two published figures from a FanGraphs article (Clemens, "Introducing an Updated Method for Prospect Valuation",
2 Jul 2026; derivation in `R/experiments7.R`).
