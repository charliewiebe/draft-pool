# Stage 3 type cleanup: region, signing status categories, documented signing failures.

us_regions <- function() {
  r <- list(
    Northeast = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
    Midwest = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
    South = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
    West = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA"))
  stats::setNames(rep(names(r), lengths(r)), unlist(r))
}

clean_types <- function(p, failures_path = file.path("data", "manual", "signing_failures.csv")) {
  # state from B-Ref "School (City, ST)" (all years); Stats API school/home state as fallback
  st <- stringr::str_match(p$br_came_from, ",\\s*([A-Z]{2})\\)\\s*$")[, 2]
  p$state <- dplyr::coalesce(st, p$school_state, p$home_state)
  p$state <- ifelse(nchar(p$state) == 2, p$state, NA_character_)
  reg <- us_regions()
  p$region <- dplyr::case_when(p$state %in% names(reg) ~ unname(reg[p$state]), p$state == "PR" ~ "Puerto Rico",
                               !is.na(p$state) ~ "Canada/other", TRUE ~ NA_character_)
  p$cold_weather <- p$state %in% c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "MI", "IN", "IL", "WI", "MN", "IA",
                                   "ND", "SD", "NE", "MT", "WY", "ID", "WA", "OR", "AK")
  f <- utils::read.csv(failures_path, stringsAsFactors = FALSE)
  p <- dplyr::left_join(p, f |> dplyr::select(year, pick_overall, failure_class, failure_source = source_url), by = c("year", "pick_overall"))
  p$signing_status <- dplyr::case_when(
    p$signed %in% TRUE ~ "signed",
    is.na(p$signed) ~ "unresolved",
    p$failure_class %in% "medical" ~ "unsigned_medical",
    p$failure_class %in% "pool_constraint" ~ "unsigned_pool_constraint",
    p$failure_class %in% c("bonus_or_college", "agreed_then_withdrew") ~ "unsigned_bonus_or_school",
    TRUE ~ "unsigned_reason_not_documented")
  # unsigned-pick compensation: rounds 1-3 and the unsigned-pick comp picks (SUP*); not CB (C-A/C-B/CB-A/CB-B), QO (1C/2C/4C) or PPI picks
  p$comp_eligible_round <- p$round_label %in% c("1", "2", "3", "SUP", "SUP-2", "SUP-3")
  p
}
