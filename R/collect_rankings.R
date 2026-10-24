# MLB Pipeline rankings (public pages). Verified (2026-10-20): each page embeds an HTML-escaped JSON payload
# with getPlayerRankingsFromSelection({"slug":"sel-pr-{year}-{top100|draft}", ...}) arrays of
# {rank, playerEntity: {player: {__ref: "Person:<mlbam>"}, position, ...}}.
# One list per year (the version the page serves); intra-year re-rankings are not recoverable from this source.

pipeline_url <- function(y, list) sprintf("https://www.mlb.com/milb/prospects/%d/%s", y, list)

fetch_pipeline <- function(top100_years = 2012:2026, draft_years = 2012:2025) {
  jobs <- rbind(data.frame(year = top100_years, list = "top100"), data.frame(year = draft_years, list = "draft"))
  jobs$dest <- file.path("data", "raw", "pipeline", sprintf("pipeline_%s_%d.html", jobs$list, jobs$year))
  for (i in seq_len(nrow(jobs))) polite_get(pipeline_url(jobs$year[i], jobs$list[i]), jobs$dest[i], min_interval = 3)
  jobs
}

html_unescape_min <- function(s) {
  s <- gsub("&quot;", "\"", s, fixed = TRUE); s <- gsub("&#x27;", "'", s, fixed = TRUE)
  s <- gsub("&#x3D;", "=", s, fixed = TRUE); s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE); gsub("&amp;", "&", s, fixed = TRUE)
}

# Return the JSON value (array/object) starting at char position `start` (1-based) of string s.
json_value_at <- function(s, start) {
  cp <- utf8ToInt(substr(s, start, nchar(s)))
  depth <- 0L; in_str <- FALSE; esc <- FALSE
  for (k in seq_along(cp)) {
    c <- cp[k]
    if (in_str) { if (esc) esc <- FALSE else if (c == 92L) esc <- TRUE else if (c == 34L) in_str <- FALSE; next }
    if (c == 34L) in_str <- TRUE
    else if (c == 91L || c == 123L) depth <- depth + 1L
    else if (c == 93L || c == 125L) { depth <- depth - 1L; if (depth == 0L) return(intToUtf8(cp[1:k])) }
  }
  NA_character_
}

parse_pipeline <- function(path, year, list) {
  u <- html_unescape_min(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  key <- "\"getPlayerRankingsFromSelection("
  pos <- gregexpr(key, u, fixed = TRUE)[[1]]
  out <- list()
  for (p in pos[pos > 0]) {
    hdr_end <- regexpr(")\":", substr(u, p, p + 400), fixed = TRUE)
    st <- p + hdr_end + 2
    js <- json_value_at(u, st)
    arr <- tryCatch(jsonlite::fromJSON(js, simplifyVector = FALSE), error = function(e) list())
    if (!length(arr)) next
    out[[length(out) + 1]] <- dplyr::bind_rows(lapply(arr, function(a) tibble::tibble(
      rank = as.integer(a$rank),
      mlbam_id = as.integer(sub("^Person:", "", a$playerEntity$player$`__ref` %||% NA_character_)),
      position = a$playerEntity$position %||% NA_character_)))
  }
  d <- dplyr::distinct(dplyr::bind_rows(out))
  if (nrow(d)) { d$list_year <- year; d$list <- list }
  d
}

# Bio fields for ranked draft prospects (birth date, committed school, education) from the same raw pages.
# Used to type ranked players who were not drafted in that year (no pick-table school type).
parse_pipeline_bio <- function(path, year) {
  u <- html_unescape_min(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  key <- "\"getPlayerRankingsFromSelection("
  pos <- gregexpr(key, u, fixed = TRUE)[[1]]
  out <- list()
  for (p in pos[pos > 0]) {
    hdr_end <- regexpr(")\":", substr(u, p, p + 400), fixed = TRUE)
    js <- json_value_at(u, p + hdr_end + 2)
    arr <- tryCatch(jsonlite::fromJSON(js, simplifyVector = FALSE), error = function(e) list())
    if (!length(arr)) next
    out[[length(out) + 1]] <- dplyr::bind_rows(lapply(arr, function(a) tibble::tibble(
      rank = as.integer(a$rank),
      mlbam_id = as.integer(sub("^Person:", "", a$playerEntity$player$`__ref` %||% NA_character_)),
      committed = a$playerEntity$prospectSchoolCommitted %||% NA_character_)))
  }
  d <- dplyr::distinct(dplyr::bind_rows(out))
  if (!nrow(d)) return(d)
  m <- regmatches(u, gregexpr("\"Person:[0-9]+\":\\{\"__typename\":\"Person\".*?\"birthDate\":\"[0-9-]+\"", u, perl = TRUE))[[1]]
  bd <- tibble::tibble(mlbam_id = as.integer(sub("^\"Person:([0-9]+)\".*", "\\1", m)), birth_date = as.Date(sub(".*\"birthDate\":\"([0-9-]+)\"$", "\\1", m)))
  d |> dplyr::left_join(dplyr::distinct(bd, mlbam_id, .keep_all = TRUE), by = "mlbam_id") |> dplyr::mutate(list_year = year)
}
