# Player outcomes from the Stats API: season-by-season MLB and MiLB appearances (yearByYear).
# Verified (2026-10-20): /api/v1/people?personIds=...&hydrate=stats(group=[hitting,pitching],type=[yearByYear],sportId=1)
# returns MLB seasons; leagueListId=milb_all returns MiLB seasons by level (sport.id 11 AAA, 12 AA, 13 A+/A(Adv),
# 14 A/A(Full), 15 A(Short), 16 ROK; 21 = all-minors aggregate).

people_batch_url <- function(ids, scope = c("mlb", "milb")) {
  scope <- match.arg(scope)
  h <- if (scope == "mlb") "stats(group=[hitting,pitching],type=[yearByYear],sportId=1)"
       else "stats(group=[hitting,pitching],type=[yearByYear],leagueListId=milb_all)"
  sprintf("https://statsapi.mlb.com/api/v1/people?personIds=%s&hydrate=%s", paste(ids, collapse = ","), h)
}

fetch_people_stats <- function(ids, batch = 100, prefix = "people") {
  ids <- sort(unique(ids[!is.na(ids)]))
  chunks <- split(ids, ceiling(seq_along(ids) / batch))
  out <- character()
  for (i in seq_along(chunks)) for (sc in c("mlb", "milb")) {
    dest <- file.path("data", "raw", "statsapi", "people", sprintf("%s_%s_%03d.json", prefix, sc, i))
    polite_get(people_batch_url(chunks[[i]], sc), dest, min_interval = 1)
    out <- c(out, dest)
  }
  # manifest records which ids were requested in each batch file
  man <- data.frame(batch = rep(seq_along(chunks), lengths(chunks)), mlbam_id = unlist(chunks))
  utils::write.csv(man, file.path("data", "raw", "statsapi", "people", paste0(prefix, "_batch_manifest.csv")), row.names = FALSE)
  out
}

parse_people_stats <- function(files) {
  nz <- function(x, na) if (is.null(x)) na else x
  ppl <- list(); rows <- list()
  for (f in files) {
    j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    for (p in j$people) {
      if (grepl("_mlb_", basename(f))) ppl[[length(ppl) + 1]] <- list(
        mlbam_id = p$id, people_birth_date = nz(p$birthDate, NA_character_),
        people_debut = nz(p$mlbDebutDate, NA_character_), people_last_played = nz(p$lastPlayedDate, NA_character_),
        people_active = isTRUE(p$active), people_draft_year = as.integer(nz(p$draftYear, NA)))
      for (st in nz(p$stats, list())) for (s in nz(st$splits, list())) {
        rows[[length(rows) + 1]] <- list(
          mlbam_id = p$id, group = st$group$displayName, season = as.integer(s$season),
          sport_id = as.integer(nz(s$sport$id, NA)), sport_abbr = nz(s$sport$abbreviation, NA_character_),
          team_id = as.integer(nz(s$team$id, NA)), games = as.integer(nz(s$stat$gamesPlayed, NA)),
          pa = as.integer(nz(s$stat$plateAppearances, NA)), ip = as.character(nz(s$stat$inningsPitched, NA)),
          bf = as.integer(nz(s$stat$battersFaced, NA)))
      }
    }
  }
  pe <- unique(data.table::rbindlist(ppl))
  for (v in c("people_birth_date", "people_debut", "people_last_played")) pe[[v]] <- as.Date(pe[[v]])
  list(people = tibble::as_tibble(pe), seasons = tibble::as_tibble(data.table::rbindlist(rows)))
}
