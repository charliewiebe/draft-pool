# Chadwick Bureau register crosswalk (MLBAM <-> FanGraphs <-> Baseball-Reference)

read_chadwick <- function(files) {
  cols <- c("key_person", "key_mlbam", "key_bbref", "key_bbref_minors", "key_fangraphs", "name_last", "name_first",
            "birth_year", "birth_month", "birth_day", "mlb_played_first", "mlb_played_last")
  d <- dplyr::bind_rows(lapply(files, function(f) data.table::fread(f, select = cols, colClasses = "character", showProgress = FALSE)))
  d[d == ""] <- NA
  d$key_mlbam <- suppressWarnings(as.integer(d$key_mlbam))
  d
}

crosswalk_link <- function(picks, chad) {
  ch <- dplyr::filter(chad, !is.na(key_mlbam)) |> dplyr::distinct(key_mlbam, .keep_all = TRUE)
  x <- dplyr::left_join(picks, dplyr::select(ch, mlbam_id = key_mlbam, chad_bbref = key_bbref, chad_bbref_minors = key_bbref_minors,
                                             chad_fangraphs = key_fangraphs), by = "mlbam_id")
  x$chad_linked <- !is.na(x$chad_bbref) | !is.na(x$chad_bbref_minors) | !is.na(x$chad_fangraphs)
  x$chad_bbref_minors_agree <- ifelse(!is.na(x$bbref_minors_id) & !is.na(x$chad_bbref_minors),
                                      x$bbref_minors_id == x$chad_bbref_minors, NA)
  x
}
