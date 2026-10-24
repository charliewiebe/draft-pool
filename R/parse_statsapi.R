# Stats API draft JSON -> one row per pick.
# Field availability (verified by inspection, 2026-10-20):
#   signingBonus, pickValue, rank, scoutingReport: 2017+ only (pickValue rounds 1-10 only from 2018)
#   school.schoolClass: 2018 and 2020-2025 only
#   person.birthDate, primaryPosition, batSide, pitchHand, mlbDebutDate: all years

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

parse_statsapi_draft <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  yr <- as.integer(j$drafts$draftYear)
  rows <- lapply(j$drafts$rounds, function(rd) lapply(rd$picks, function(p) {
    pe <- p$person %||% list()
    tibble::tibble(
      year = yr,
      round_label = p$pickRound %||% NA_character_,
      round_block = rd$round %||% NA_character_,
      pick_overall = as.integer(p$pickNumber %||% NA),
      pick_display = as.integer(p$displayPickNumber %||% NA),
      pick_in_round = as.integer(p$roundPickNumber %||% NA),
      team_id = as.integer(p$team$id %||% NA),
      team_name = p$team$name %||% NA_character_,
      mlbam_id = as.integer(pe$id %||% NA),
      bis_id = as.integer(p$bisPlayerId %||% NA),
      full_name = pe$fullName %||% NA_character_,
      birth_date = as.Date(pe$birthDate %||% NA_character_),
      birth_country = pe$birthCountry %||% NA_character_,
      pos_code = pe$primaryPosition$code %||% NA_character_,
      pos_abbr = pe$primaryPosition$abbreviation %||% NA_character_,
      pos_type = pe$primaryPosition$type %||% NA_character_,
      bats = pe$batSide$code %||% NA_character_,
      throws = pe$pitchHand$code %||% NA_character_,
      mlb_debut = as.Date(pe$mlbDebutDate %||% NA_character_),
      school_name = p$school$name %||% NA_character_,
      school_class = p$school$schoolClass %||% NA_character_,
      school_state = p$school$state %||% NA_character_,
      school_country = p$school$country %||% NA_character_,
      home_state = p$home$state %||% NA_character_,
      home_country = p$home$country %||% NA_character_,
      pre_draft_rank = as.integer(p$rank %||% NA),
      sa_pick_value = as.numeric(p$pickValue %||% NA),
      sa_signing_bonus = as.numeric(p$signingBonus %||% NA),
      draft_type = p$draftType$code %||% NA_character_,
      is_drafted = isTRUE(p$isDrafted),
      is_pass = isTRUE(p$isPass)
    )
  }))
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

statsapi_picks <- function(files) dplyr::bind_rows(lapply(files, parse_statsapi_draft))
