# Documented reasons for unsigned top-10-round picks (and signing-deadline context).
# 2012-2017: BA "What has happened to top picks who failed to sign" (top 3 rounds, with reasons);
# 2018-2025: MLB.com signing-deadline roundups / bonus trackers.

signing_sources <- function() tibble::tribble(
  ~year, ~key, ~url,
  2017L, "ba_unsigned_2012_17", "https://www.baseballamerica.com/stories/what-has-happened-to-top-picks-who-failed-to-sign/",
  2018L, "mlb_deadline_2018", "https://www.mlb.com/news/draft-picks-go-unsigned-at-deadline-c284523732",
  2019L, "mlb_deadline_2019", "https://www.mlb.com/news/2019-mlb-draft-signing-deadline-roundup",
  2021L, "mlb_deadline_2021", "https://www.mlb.com/news/mlb-draft-signing-deadline-picks-unsigned",
  2022L, "mlb_deadline_2022", "https://www.mlb.com/news/2022-mlb-draft-signing-deadline-picks-unsigned",
  2023L, "mlb_deadline_2023", "https://www.mlb.com/news/2023-mlb-draft-signing-deadline-passes-quietly",
  2024L, "mlb_deadline_2024", "https://www.mlb.com/news/mlb-draft-signing-deadline-2024",
  2025L, "mlb_tracker_2025", "https://www.mlb.com/news/2025-draft-signing-and-bonus-tracker"
)

fetch_signing_sources <- function(src = signing_sources()) {
  src$dest <- file.path("data", "raw", "articles", paste0("signing_", src$key, ".html"))
  src$dest[src$key == "ba_unsigned_2012_17"] <- file.path("data", "raw", "articles", "ba_unsigned_top.html")
  for (i in seq_len(nrow(src))) try(polite_get(src$url[i], src$dest[i], min_interval = 3))
  src$ok <- file.exists(src$dest)
  src
}

# For each unsigned pick in rounds 1-10, find sentences naming the player (last name + first name or initial)
# in that year's source (BA article covers 2012-17) and classify the stated reason.
classify_unsigned <- function(p, src) {
  txt <- lapply(seq_len(nrow(src)), function(i) if (src$ok[i]) unique(unlist(strsplit(paste(article_lines(src$dest[i]), collapse = " "), "(?<=[.!?])\\s+", perl = TRUE))) else character())
  names(txt) <- src$key
  u <- dplyr::filter(p, pool_round, signed %in% FALSE, signed_basis %in% c("B-Ref N", "unsigned: drafted again later", "unsigned: drafted again later (overrides source 'signed')"))
  res <- lapply(seq_len(nrow(u)), function(k) {
    y <- u$year[k]; nm <- dplyr::coalesce(u$full_name[k], u$br_player[k])
    last <- sub(".* ", "", nm); first <- sub(" .*", "", nm)
    key <- if (y <= 2017) "ba_unsigned_2012_17" else src$key[src$year == y]
    s <- if (length(key) && key %in% names(txt)) txt[[key]] else character()
    hit <- s[grepl(last, s, fixed = TRUE) & (grepl(first, s, fixed = TRUE) | grepl(paste0("\\b", last, "\\b"), s))]
    hit <- hit[nchar(hit) < 800]
    med <- any(grepl("(?i)physical|medical|MRI|elbow|shoulder|Tommy John|injur|ligament|UCL|surgery|health", hit))
    tibble::tibble(year = y, pick_overall = u$pick_overall[k], name = nm,
      source_key = if (length(key)) key else NA_character_, mentioned = length(hit) > 0,
      reason_class = dplyr::case_when(!length(hit) ~ "not documented in source", med ~ "medical/physical", TRUE ~ "other/signability"),
      evidence = if (length(hit)) substr(paste(hit, collapse = " | "), 1, 400) else NA_character_)
  })
  dplyr::bind_rows(res)
}
