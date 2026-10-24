# Season-level outcomes: bWAR (B-Ref war_daily files) + Stats API MLB/MiLB appearances.

parse_bbref_war <- function(files) {
  rd <- function(f, cols) {
    d <- data.table::fread(f, select = cols, na.strings = c("NULL", ""), showProgress = FALSE)
    d$WAR <- suppressWarnings(as.numeric(d$WAR)); d
  }
  files <- unname(files)
  bat <- rd(files[grepl("war_daily_bat", files)], c("mlb_ID", "player_ID", "year_ID", "stint_ID", "PA", "G", "WAR", "pitcher"))
  pit <- rd(files[grepl("war_daily_pitch", files)], c("mlb_ID", "player_ID", "year_ID", "stint_ID", "G", "IPouts", "WAR"))
  b <- bat[, .(bwar_bat = sum(WAR, na.rm = TRUE), pa = sum(as.numeric(PA), na.rm = TRUE), g_bat = sum(as.numeric(G), na.rm = TRUE)),
           by = .(mlb_ID, player_ID, year_ID)]
  p <- pit[, .(bwar_pitch = sum(WAR, na.rm = TRUE), ipouts = sum(as.numeric(IPouts), na.rm = TRUE), g_pitch = sum(as.numeric(G), na.rm = TRUE)),
           by = .(mlb_ID, player_ID, year_ID)]
  m <- merge(b, p, by = c("mlb_ID", "player_ID", "year_ID"), all = TRUE)
  for (v in c("bwar_bat", "bwar_pitch", "pa", "g_bat", "ipouts", "g_pitch")) m[[v]][is.na(m[[v]])] <- 0
  m$bwar <- m$bwar_bat + m$bwar_pitch
  tibble::as_tibble(m) |> dplyr::rename(mlbam_id = mlb_ID, bbref_id = player_ID, season = year_ID) |>
    dplyr::mutate(mlbam_id = as.integer(mlbam_id), season = as.integer(season))
}

# One row per player-season: MLB games (Stats API), highest MiLB level, bWAR.
player_seasons <- function(people, war) {
  s <- people$seasons
  # sport ids: 11 AAA, 12 AA, 13 A+ (A-Adv), 14 A (A-Full), 15 A- (short season), 5442 Rookie Advanced, 16 Rookie
  lvl_rank <- c(`11` = 7L, `12` = 6L, `13` = 5L, `14` = 4L, `15` = 3L, `5442` = 2L, `16` = 1L)
  lvl_lab <- c("ROK", "ROA", "A-", "A", "A+", "AA", "AAA")
  mlb <- s |> dplyr::filter(sport_id == 1) |> dplyr::group_by(mlbam_id, season) |>
    dplyr::summarise(mlb_games = max(games, na.rm = TRUE), .groups = "drop")
  milb <- s |> dplyr::filter(as.character(sport_id) %in% names(lvl_rank)) |> dplyr::mutate(lvl = lvl_rank[as.character(sport_id)]) |>
    dplyr::group_by(mlbam_id, season) |>
    dplyr::summarise(milb_top_level = lvl_lab[max(lvl)], milb_games = sum(games, na.rm = TRUE), .groups = "drop")
  w <- dplyr::select(war, mlbam_id, season, bwar, bwar_bat, bwar_pitch, pa, ipouts)
  dplyr::full_join(mlb, milb, by = c("mlbam_id", "season")) |> dplyr::full_join(w, by = c("mlbam_id", "season")) |>
    dplyr::filter(mlbam_id %in% unique(people$people$mlbam_id)) |> dplyr::arrange(mlbam_id, season)
}
