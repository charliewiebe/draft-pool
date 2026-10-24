# Baseball-Reference draft round pages -> one row per pick.
# Verified layout (2026-10-20): table#draft_stats; supplemental/CB picks appear on the
# preceding round's page; columns keyed by data-stat.

parse_bbref_round <- function(path) {
  doc <- xml2::read_html(path)
  tb <- xml2::xml_find_first(doc, "//table[@id='draft_stats']")
  if (inherits(tb, "xml_missing")) return(NULL)
  trs <- xml2::xml_find_all(tb, ".//tbody/tr[not(contains(@class,'thead'))]")
  cell <- function(tr, stat) {
    n <- xml2::xml_find_first(tr, sprintf("./*[@data-stat='%s']", stat))
    if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
  }
  href <- function(tr, pat) {
    a <- xml2::xml_find_all(tr, "./*[@data-stat='player']//a")
    h <- xml2::xml_attr(a, "href")
    h <- h[grepl(pat, h)]
    if (length(h)) h[1] else NA_character_
  }
  out <- lapply(trs, function(tr) {
    tibble::tibble(
      year = as.integer(cell(tr, "year_ID")),
      br_round = cell(tr, "draft_round"),
      br_round_abb = cell(tr, "draft_abb"),
      pick_overall = suppressWarnings(as.integer(cell(tr, "overall_pick"))),
      br_round_pick = suppressWarnings(as.integer(cell(tr, "round_pick"))),
      br_team = cell(tr, "team_ID"),
      br_signed = cell(tr, "signed"),
      br_bonus_raw = cell(tr, "bonus"),
      br_player = sub(" ?\\(minors\\)$", "", cell(tr, "player")),
      br_pos = cell(tr, "pos"),
      br_war_career = suppressWarnings(as.numeric(cell(tr, "WAR"))),
      br_from_type = cell(tr, "from_type"),
      br_came_from = cell(tr, "came_from"),
      bbref_id = sub("^/players/./(.*)\\.shtml$", "\\1", href(tr, "^/players/")),
      bbref_minors_id = sub("^.*id=", "", href(tr, "register/player"))
    )
  })
  d <- dplyr::bind_rows(out)
  d$br_bonus <- suppressWarnings(as.numeric(gsub("[$,]", "", d$br_bonus_raw)))
  d$src_file <- basename(path)
  d
}

bbref_picks <- function(files) {
  d <- dplyr::bind_rows(lapply(files, parse_bbref_round))
  d[!is.na(d$pick_overall), ]
}
