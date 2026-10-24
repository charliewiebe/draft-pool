# Published slot values and team bonus pools (pre-draft releases).
# content: "pools" = team pool totals; "picks" = per-pick assigned values.

pool_articles <- function() {
  tibble::tribble(
    ~year, ~outlet, ~content, ~url,
    2012L, "BA",  "picks",       "https://www.baseballamerica.com/stories/slots-you-can-believe-in-for-2012-mlb-draft/",
    2013L, "BA",  "pools",       "https://www.baseballamerica.com/stories/draft-bonus-pools-rise-82-percent/",
    2014L, "MLB", "pools",       "https://www.mlb.com/news/mlb-draft-international-slot-values-to-increase-by-17-percent/c-70729748",
    2015L, "MLB", "pools",       "https://www.mlb.com/news/breakdown-of-2015-draft-signing-bonus-pools/c-126797888",
    2016L, "MLB", "pools+picks", "https://www.mlb.com/news/2016-draft-bonus-pools-pick-values-c170470526",
    2017L, "MLB", "pools+picks", "https://www.mlb.com/news/2017-mlb-draft-bonus-pools-pick-values/c-223686792",
    2018L, "MLB", "pools+picks", "https://www.mlb.com/news/2018-mlb-draft-bonus-pools-pick-values-c269930084",
    2019L, "MLB", "pools+picks", "https://www.mlb.com/news/2019-mlb-draft-pools-and-bonus-values",
    2020L, "MLB", "pools+picks", "https://www.mlb.com/news/2020-mlb-draft-bonus-slots",
    2021L, "MLB", "pools+picks", "https://www.mlb.com/news/2021-mlb-draft-bonus-slots",
    2022L, "MLB", "pools+picks", "https://www.mlb.com/news/mlb-draft-2022-bonus-pools-pick-values",
    2023L, "MLB", "pools+picks", "https://www.mlb.com/news/mlb-draft-2023-bonus-pools-pick-values",
    2024L, "MLB", "pools+picks", "https://www.mlb.com/news/mlb-draft-2024-bonus-pick-values",
    2025L, "MLB", "pools+picks", "https://www.mlb.com/news/mlb-draft-2025-bonus-pick-values"
  )
}

fetch_pool_articles <- function(arts = pool_articles()) {
  arts$dest <- file.path("data", "raw", "articles", sprintf("pools_%d_%s.html", arts$year, arts$outlet))
  for (i in seq_len(nrow(arts))) polite_get(arts$url[i], arts$dest[i], min_interval = 3)
  arts
}

# Article body -> clean lines of text
article_lines <- function(path) {
  doc <- xml2::read_html(path)
  xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style|//noscript"))
  txt <- xml2::xml_text(xml2::xml_find_all(doc, "//body//*[not(*)] | //body//p | //body//li"))
  txt <- trimws(unlist(strsplit(txt, "\n")))
  txt[nzchar(txt)]
}
