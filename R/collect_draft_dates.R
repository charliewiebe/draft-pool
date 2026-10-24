# Draft start dates from MLB.com announcements (B-Ref Bullpen returned 403 to scripted requests).
draft_date_sources <- function() tibble::tribble(
  ~year, ~url,
  2012L, "https://www.mlb.com/news/brad-ausmus-will-sit-at-the-padres-table-for-the-first-year-player-draft-on-june-4/c-31907624",
  2013L, "https://www.mlb.com/news/2013-first-year-player-draft-order-announced/c-46497760",
  2014L, "https://www.mlb.com/news/2014-first-year-player-draft-order-announced/c-73975638",
  2015L, "https://www.mlb.com/news/breakdown-of-2015-draft-signing-bonus-pools/c-126797888",
  2016L, "https://www.mlb.com/press-release/2016-mlb-draft-order-announced-176201990",
  2017L, "https://www.mlb.com/news/mlb-draft-starts-june-12-on-mlb-com-network-c229385474",
  2018L, "https://www.mlb.com/press-release/2018-mlb-draft-details-announced-276832460",
  2019L, "https://www.mlb.com/press-release/2019-mlb-draft-details-announced",
  2020L, "https://www.mlb.com/news/2020-mlb-draft-schedule",
  2021L, "https://www.mlb.com/press-release/press-release-eight-amateur-players-seventeen-former-all-stars-scheduled-to-appe",
  2022L, "https://www.mlb.com/news/2022-mlb-draft-mlb-draft-combine-details",
  2023L, "https://www.mlb.com/press-release/press-release-seven-amateur-players-to-attend-the-2023-mlb-draft-presented-by-ni",
  2024L, "https://www.mlb.com/news/mlb-draft-overview-and-schedule",
  2025L, "https://www.mlb.com/news/2025-mlb-draft-details"
)

fetch_draft_date_pages <- function(src = draft_date_sources()) {
  src$dest <- file.path("data", "raw", "articles", sprintf("dates_%d_MLB.html", src$year))
  for (i in seq_len(nrow(src))) {
    if (src$year[i] == 2015L) { src$dest[i] <- file.path("data", "raw", "articles", "pools_2015_MLB.html"); next }
    try(polite_get(src$url[i], src$dest[i], min_interval = 3))
  }
  src
}

# Candidate date sentences: "(June|July) D" within 120 chars of "Draft"; first day = earliest day in the matched span
extract_draft_dates <- function(src) {
  mon <- c(June = 6L, July = 7L)
  dplyr::bind_rows(lapply(seq_len(nrow(src)), function(i) {
    y <- src$year[i]; f <- src$dest[i]
    if (!file.exists(f)) return(tibble::tibble(year = y, draft_start = as.Date(NA), evidence = "page not retrieved", source_url = src$url[i]))
    t <- paste(unique(article_lines(f)), collapse = " ")
    rx <- "(?i)(draft[^.]{0,120}?|)\\b(June|July) (\\d{1,2})(?:st|nd|rd|th)?(?:\\s*(?:-|–|through|to)\\s*(\\d{1,2}))?"
    m <- stringr::str_match_all(t, rx)[[1]]
    m <- m[grepl("(?i)draft", m[, 2]) | grepl("(?i)draft", substr(t, 1, 1)), , drop = FALSE]
    if (!nrow(m)) return(tibble::tibble(year = y, draft_start = as.Date(NA), evidence = "no dated draft sentence", source_url = src$url[i]))
    tibble::tibble(year = y, draft_start = as.Date(sprintf("%d-%02d-%02d", y, mon[[tools::toTitleCase(tolower(m[1, 3]))]], as.integer(m[1, 4]))),
                   evidence = substr(m[1, 1], max(1, nchar(m[1, 1]) - 140), nchar(m[1, 1])), source_url = src$url[i])
  }))
}
