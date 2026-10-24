# 2020 non-drafted free agents (bonus capped at $20,000). Identified from Stats API transactions:
# typeCode SFA ("signed free agent") from the first legal signing date (2020-06-14) through 2020-09-30,
# restricted to players with no affiliated pro season before 2021, not signed out of any earlier Rule 4 draft,
# and age <= 25 on 2020-06-14 (amateurs; excludes released veterans and international signees by age/pro history).

fetch_transactions <- function(start, end, tag) {
  dest <- file.path("data", "raw", "statsapi", "transactions", sprintf("transactions_%s.json", tag))
  polite_get(sprintf("https://statsapi.mlb.com/api/v1/transactions?startDate=%s&endDate=%s", start, end), dest, min_interval = 1)
  dest
}

udfa_2020 <- function(picks) {
  f <- c(fetch_transactions("2020-06-14", "2020-07-31", "2020_06_14_07_31"), fetch_transactions("2020-08-01", "2020-09-30", "2020_08_01_09_30"))
  tr <- dplyr::bind_rows(lapply(f, function(x) {
    j <- jsonlite::fromJSON(x, simplifyVector = FALSE)$transactions
    dplyr::bind_rows(lapply(j, function(t) tibble::tibble(date = as.Date(t$date %||% NA_character_), type = t$typeCode %||% NA_character_,
      mlbam_id = as.integer(t$person$id %||% NA), team_id = as.integer(t$toTeam$id %||% NA), team = t$toTeam$name %||% NA_character_,
      description = t$description %||% NA_character_)))
  }))
  sfa <- dplyr::filter(tr, type == "SFA", !is.na(mlbam_id)) |> dplyr::distinct(mlbam_id, .keep_all = TRUE)
  pf <- fetch_people_stats(sfa$mlbam_id, prefix = "udfa2020")
  pp <- parse_people_stats(pf)
  prior_pro <- pp$seasons |> dplyr::filter(sport_id %in% c(1L, 11:16, 5442L, 17L), season < 2021) |> dplyr::distinct(mlbam_id)
  signed_before <- picks |> dplyr::filter(signed %in% TRUE, year < 2020) |> dplyr::distinct(mlbam_id)
  x <- sfa |> dplyr::left_join(pp$people, by = "mlbam_id") |>
    dplyr::mutate(age_2020 = as.numeric(as.Date("2020-06-14") - people_birth_date) / 365.25,
                  prior_pro = mlbam_id %in% prior_pro$mlbam_id, signed_rule4_before = mlbam_id %in% signed_before$mlbam_id,
                  minor_league_contract = grepl("minor league contract", description, fixed = TRUE),
                  udfa_2020 = !prior_pro & !signed_rule4_before & !is.na(age_2020) & age_2020 >= 17 & age_2020 <= 25)
  list(candidates = x, n_sfa = nrow(sfa))
}
