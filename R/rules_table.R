# Versioned, cited rules table. One row per (year, rule); every cell has a source URL and status:
#   verified_source   - the cited source states the rule for that year
#   observed_in_data  - read from the logged Stats API draft file for that year
#   inferred_same_cba - stated for another year under the same CBA
#   current_rule_page - undated reference page describing current rules
# Raw copies of MLB.com/BA sources live in data/raw/articles (see request_log.csv).

rule_sources <- function() c(
  masn2011   = "https://masnsports.com/blog/some-initial-thoughts-on-the-draft-changes-in-the-new-cba",
  rab2012    = "https://riveraveblues.com/resources/2012-draft-pool",
  ba2012     = "https://www.baseballamerica.com/stories/slots-you-can-believe-in-for-2012-mlb-draft/",
  ba2013     = "https://www.baseballamerica.com/stories/draft-bonus-pools-rise-82-percent/",
  mlb2014    = "https://www.mlb.com/news/mlb-draft-international-slot-values-to-increase-by-17-percent/c-70729748",
  mlb2015    = "https://www.mlb.com/news/breakdown-of-2015-draft-signing-bonus-pools/c-126797888",
  mlb2016    = "https://www.mlb.com/news/2016-draft-bonus-pools-pick-values-c170470526",
  mlb2017    = "https://www.mlb.com/news/2017-mlb-draft-bonus-pools-pick-values/c-223686792",
  mlb2018    = "https://www.mlb.com/news/2018-mlb-draft-bonus-pools-pick-values-c269930084",
  mlb2019    = "https://www.mlb.com/news/2019-mlb-draft-pools-and-bonus-values",
  mlb2020    = "https://www.mlb.com/news/2020-mlb-draft-bonus-slots",
  cbs2020    = "https://www.cbssports.com/mlb/news/mlb-agrees-to-reduce-2020-draft-to-five-rounds-with-unlimited-undrafted-signings-for-20k-reports-say/",
  mlb2021    = "https://www.mlb.com/news/2021-mlb-draft-bonus-slots",
  mlbcba2022 = "https://www.mlb.com/news/draft-rule-changes-with-new-cba",
  mlb2022    = "https://www.mlb.com/news/mlb-draft-2022-bonus-pools-pick-values",
  mlb2022d3  = "https://www.mlb.com/news/day-3-of-draft-how-to-watch-best-available",
  mlb2023    = "https://www.mlb.com/news/mlb-draft-2023-bonus-pools-pick-values",
  mlb2023d3  = "https://www.mlb.com/news/2023-mlb-draft-day-3-overview-and-schedule",
  mlb2024    = "https://www.mlb.com/news/mlb-draft-2024-bonus-pick-values",
  mlb2024d3  = "https://www.mlb.com/news/2024-mlb-draft-day-3-overview-and-schedule",
  mlb2025    = "https://www.mlb.com/news/mlb-draft-2025-bonus-pick-values",
  mlb2025d2  = "https://www.mlb.com/mets/news/2025-mlb-draft-day-2-overview-schedule",
  ba2019comp = "https://www.baseballamerica.com/stories/under-current-cba-compensatory-draft-picks-are-protected/",
  ba_unsigned= "https://www.baseballamerica.com/stories/what-has-happened-to-top-picks-who-failed-to-sign/",
  cubpool    = "https://www.thecubreporter.com/book/export/html/3554",
  cubcomp    = "https://www.thecubreporter.com/book/export/html/3555",
  cubr5      = "https://www.thecubreporter.com/book/export/html/3517",
  si2018     = "https://www.si.com/mlb/2018/12/13/rule-5-draft-explained-major-league-baseball",
  jb2026     = "https://www.justbaseball.com/mlb/offseason-schedule-key-dates-lockout/"
)

# Pool article that states the tax bands / unsigned-pick rule for each year
pool_article_key <- function(y) {
  k <- c(`2012` = "masn2011", `2013` = "ba2013", `2014` = "mlb2014", `2015` = "mlb2015", `2016` = "mlb2016",
         `2017` = "mlb2017", `2018` = "mlb2018", `2019` = "mlb2019", `2020` = "mlb2020", `2021` = "mlb2021",
         `2022` = "mlb2022", `2023` = "mlb2023", `2024` = "mlb2024", `2025` = "mlb2025")
  unname(k[as.character(y)])
}

cba_of <- function(y) ifelse(y <= 2016, "2012-2016", ifelse(y <= 2021, "2017-2021", "2022-2026"))

# observed_rounds: named integer vector (year -> max numeric round in Stats API draft file)
build_rules_table <- function(observed_rounds, years = 2012:2025) {
  U <- rule_sources(); rows <- list()
  add <- function(y, rule, value, src, status, note = "") {
    url <- if (src %in% names(U)) U[[src]] else src
    v <- if (is.numeric(value)) format(value, scientific = FALSE, trim = TRUE) else as.character(value)
    rows[[length(rows) + 1]] <<- data.frame(year = y, cba = cba_of(y), rule = rule, value = v,
                                           source_url = url, status = status, note = note, stringsAsFactors = FALSE)
  }
  r11_src <- c(`2012` = "ba2012", `2013` = "ba2013", `2014` = "mlb2014", `2015` = "mlb2015", `2016` = "mlb2016",
               `2017` = "mlb2017", `2018` = "mlb2018", `2019` = "mlb2019", `2022` = "mlb2022d3",
               `2023` = "mlb2023d3", `2024` = "mlb2024d3", `2025` = "mlb2025d2")
  for (y in years) {
    sa_url <- sprintf("https://statsapi.mlb.com/api/v1/draft/%d", y)
    add(y, "n_rounds", observed_rounds[[as.character(y)]], sa_url, "observed_in_data", "max numeric pickRound in draft file")
    # rounds 11+ threshold
    if (y == 2020) add(y, "r11_threshold_usd", NA, "mlb2020", "verified_source", "five-round draft; no rounds 11+; see udfa_cap_usd")
    else if (y == 2021) add(y, "r11_threshold_usd", 125000, "mlbcba2022", "inferred_same_cba", "2022 CBA article: threshold rises from $125,000 (2017-21 CBA)")
    else if (y == 2022) add(y, "r11_threshold_usd", 125000, "mlb2022d3", "verified_source",
      "MLB.com 2022-07-19: up to $125,000 ($150,000 in 2023); the March 2022 CBA summary implied $150,000 already in 2022; R11+ bonuses in 2022 bunch at $125,000 (data check)")
    else add(y, "r11_threshold_usd", if (y <= 2016) 100000 else if (y <= 2021) 125000 else 150000,
             r11_src[[as.character(y)]], "verified_source")
    add(y, "pool_rounds", if (y == 2020) "1-5 incl. CB/comp picks" else "1-10 incl. CB/comp/supplemental picks",
        pool_article_key(y), "verified_source")
    add(y, "unsigned_slot_removed_from_pool", TRUE, if (y == 2012) "rab2012" else pool_article_key(y), "verified_source")
    tax_src <- pool_article_key(y)
    add(y, "band1_0_5pct", "tax=0.75;forfeit=none", tax_src, "verified_source")
    add(y, "band2_5_10pct", "tax=0.75;forfeit=next 1st", tax_src, "verified_source")
    add(y, "band3_10_15pct", "tax=1.00;forfeit=next 1st+2nd", tax_src, "verified_source")
    add(y, "band4_gt15pct", "tax=1.00;forfeit=next two 1sts", tax_src, "verified_source")
    add(y, "draft_lottery_top6", y >= 2023, "mlbcba2022", if (y == 2023) "verified_source" else if (y > 2023) "inferred_same_cba" else "verified_source",
        "lottery begins 2023; 18 non-playoff clubs")
    add(y, "unsigned_comp_rounds", "1-3 incl. comp picks between rounds 1-4; excl. CB, QO (XX-B), PPI, forfeited-pick lottery picks",
        if (y <= 2016) "ba_unsigned" else if (y <= 2021) "ba2019comp" else "cubcomp",
        if (y <= 2021) "verified_source" else "current_rule_page",
        "BA: comp for unsigned top-3-round picks since 2012; exclusions per current rule page")
    add(y, "unsigned_comp_protected_second_year", TRUE, if (y <= 2021) "ba2019comp" else "cubcomp",
        if (y >= 2017 && y <= 2021) "verified_source" else if (y <= 2016) "inferred_same_cba" else "current_rule_page")
    add(y, "unsigned_comp_min_offer_frac_slot", 0.40, "cubcomp", "current_rule_page", "per-year history not independently verified")
    add(y, "rule5_eligibility", "age<=18 on June 5 before first contract: 5th Rule 5 draft; age>=19: 4th", "cubr5", "current_rule_page",
        "consistent with SI 2018 explainer")
    if (y %in% c(2020, 2021)) add(y, "slot_values_frozen_at_2019", TRUE, "mlb2021", "verified_source")
    if (y >= 2022) {
      add(y, "top300_physical_guarantee_frac_slot", 0.75, "mlbcba2022", if (y == 2022) "verified_source" else "inferred_same_cba")
      add(y, "draft_and_follow_counts_above_usd", 225000, "mlbcba2022", if (y == 2022) "verified_source" else "inferred_same_cba")
    }
  }
  add(2020, "udfa_cap_usd", 20000, "mlb2020", "verified_source")
  add(2020, "bonus_max_upfront_usd", 100000, "mlb2020", "verified_source", "remainder deferred: 50% 2021-07-01, 50% 2022-07-01")
  add(2017, "pick1_2_slot_reduced_by_cba", TRUE, "mlb2017", "verified_source", "No. 1 slot $7,770,700 vs $9,015,000 in 2016")
  add(2026, "cba_expiration", "2026-12-01T23:59 ET", "jb2026", "verified_source", "article dated 2026-10-13; all rules are model parameters")
  do.call(rbind, rows)
}
