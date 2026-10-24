# Raw collection: Stats API draft, Baseball-Reference draft rounds, Chadwick register.

draft_years <- function() 2012:2025

max_round_by_year <- function(y) {
  # Mirrors rules_by_year.csv n_rounds; kept here only to enumerate B-Ref round pages.
  rules <- read_rules()
  as.integer(rules$value[rules$rule == "n_rounds" & rules$year == y])
}

read_rules <- function(path = file.path("data", "derived", "rules_by_year.csv")) {
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
}

fetch_statsapi_draft <- function(years = draft_years()) {
  vapply(years, function(y) {
    dest <- file.path("data", "raw", "statsapi", "draft", sprintf("draft_%d.json", y))
    polite_get(sprintf("https://statsapi.mlb.com/api/v1/draft/%d", y), dest, min_interval = 1)
    dest
  }, character(1))
}

bbref_round_url <- function(y, r) {
  sprintf("https://www.baseball-reference.com/draft/index.fcgi?year_ID=%d&draft_round=%d&draft_type=junreg&query_type=year_round", y, r)
}

fetch_bbref_draft <- function(years = draft_years()) {
  # Sports Reference bot policy: <= 20 requests/minute. 4 s spacing = 15/min.
  out <- character()
  for (y in years) for (r in seq_len(max_round_by_year(y))) {
    dest <- file.path("data", "raw", "bbref", "draft", sprintf("bbref_%d_r%02d.html", y, r))
    polite_get(bbref_round_url(y, r), dest, min_interval = 4)
    out <- c(out, dest)
  }
  out
}

fetch_chadwick <- function(dest_dir = file.path("data", "raw", "chadwick")) {
  if (!dir.exists(file.path(dest_dir, ".git"))) {
    system2("git", c("clone", "--depth", "1", "https://github.com/chadwickbureau/register", dest_dir))
    sha <- system2("git", c("-C", dest_dir, "rev-parse", "HEAD"), stdout = TRUE)
    log_request("https://github.com/chadwickbureau/register", dest_dir, 200, NA, sha, "git clone --depth 1; sha is commit")
  }
  sort(list.files(file.path(dest_dir, "data"), pattern = "^people-.*\\.csv$", full.names = TRUE))
}

# Baseball-Reference season-level WAR tables (include mlb_ID). Used for bWAR.
fetch_bbref_war <- function() {
  d <- file.path("data", "raw", "bbref", "war")
  f <- c(bat = file.path(d, "war_daily_bat.txt"), pitch = file.path(d, "war_daily_pitch.txt"))
  polite_get("https://www.baseball-reference.com/data/war_daily_bat.txt", f[["bat"]], min_interval = 4)
  polite_get("https://www.baseball-reference.com/data/war_daily_pitch.txt", f[["pitch"]], min_interval = 4)
  f
}
