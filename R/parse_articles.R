# Parse published team pools and per-pick assigned values from pool articles.

team_nicks <- function() {
  tibble::tribble(
    ~nick, ~team_id,
    "Angels", 108L, "Astros", 117L, "Athletics", 133L, "A's", 133L, "Blue Jays", 141L,
    "Braves", 144L, "Brewers", 158L, "Cardinals", 138L, "Cubs", 112L, "Diamondbacks", 109L,
    "D-backs", 109L, "Dodgers", 119L, "Giants", 137L, "Guardians", 114L, "Indians", 114L,
    "Mariners", 136L, "Marlins", 146L, "Mets", 121L, "Nationals", 120L, "Orioles", 110L,
    "Padres", 135L, "Phillies", 143L, "Pirates", 134L, "Rangers", 140L, "Rays", 139L,
    "Red Sox", 111L, "Reds", 113L, "Rockies", 115L, "Royals", 118L, "Tigers", 116L,
    "Twins", 142L, "White Sox", 145L, "Yankees", 147L
  )
}

money <- function(x) as.numeric(gsub("[$,]", "", x))

nick_regex <- function() {
  n <- team_nicks()$nick
  n <- n[order(-nchar(n))]
  paste0("(", paste(gsub("([.'-])", "\\\\\\1", n), collapse = "|"), ")")
}

# All "[pick) |pick. ]Team: $value" / "Team -- $value" matches in document order.
team_money_matches <- function(txt) {
  rx <- paste0("(?:(\\d{1,3})[).]\\s*)?", nick_regex(), "\\s*(?::|--)\\s*\\$([0-9]{1,3}(?:,[0-9]{3})+)")
  m <- stringr::str_match_all(txt, rx)[[1]]
  tibble::tibble(pick_num = as.integer(m[, 2]), nick = m[, 3], value = money(m[, 4]))
}

article_text <- function(path) paste(article_lines(path), collapse = "\n")

# Returns list(pools = tibble(team_id, nick, pool, n_picks), picks = tibble(pick_overall, team_id, value), total = stated total)
parse_pool_article <- function(year, path) {
  l <- article_lines(path)
  nk <- team_nicks()
  pools <- NULL; picks <- NULL
  if (year == 2012) {
    i0 <- which(l == "VALUE")[1]
    v <- l[(i0 + 1):length(l)]
    stop_at <- which(!grepl("^(\\d+|\\$[0-9,]+|\\d+%)$", v) & !(v %in% nk$nick))[1]
    v <- v[seq_len(stop_at - 1)]
    m <- matrix(v, ncol = 4, byrow = TRUE)
    picks <- tibble::tibble(pick_overall = as.integer(m[, 1]), nick = m[, 2], value = money(m[, 4]))
  } else if (year == 2013) {
    i0 <- which(l == "Team Total")[1]; i1 <- which(l == "2013 Total")[1]
    m <- matrix(l[(i0 + 1):(i1 - 1)], ncol = 3, byrow = TRUE)
    pools <- tibble::tibble(nick = m[, 1], n_picks = as.integer(m[, 2]), pool = money(m[, 3]))
  } else if (year == 2014) {
    i0 <- which(l == "Average")[1]; i1 <- which(l == "TOTAL")[1]
    m <- matrix(l[(i0 + 1):(i1 - 1)], ncol = 4, byrow = TRUE)
    pools <- tibble::tibble(nick = m[, 1], pool = money(m[, 2]), n_picks = as.integer(m[, 3]))
  } else {
    txt <- paste(l, collapse = "\n")
    mm <- team_money_matches(txt)
    if (year == 2019) {
      # 2019 lists pools as "$16,093,700 - D-backs"
      pm <- stringr::str_match_all(txt, paste0("\\$([0-9]{1,3}(?:,[0-9]{3})+) - ", nick_regex()))[[1]]
      pools <- tibble::tibble(nick = pm[, 3], pool = money(pm[, 2]), n_picks = NA_integer_)
      pools <- pools[!duplicated(pools$nick), ]
      pk <- mm
    } else {
      pools <- dplyr::transmute(mm[1:30, ], nick, pool = value, n_picks = NA_integer_)
      pk <- mm[-(1:30), ]
    }
    if (year >= 2016) {
      # Unnumbered entries (e.g., 2016-18 lists, some first-round blocks) take previous + 1.
      for (k in seq_len(nrow(pk))) if (is.na(pk$pick_num[k])) pk$pick_num[k] <- if (k == 1) 1L else pk$pick_num[k - 1] + 1L
      picks <- dplyr::transmute(pk, pick_overall = pick_num, nick, value)
    }
  }
  fix <- function(d) if (is.null(d)) NULL else dplyr::left_join(d, nk, by = "nick")
  list(pools = fix(pools), picks = fix(picks))
}
