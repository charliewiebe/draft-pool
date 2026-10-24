# Per-pick slot values (rounds 1-10) and team pool reconstruction.
#
# Sources by year (see report for verification):
#   2012       BA per-pick list (published)
#   2013-2015  derived: 2012 per-position schedule x year scale (no public per-pick list located).
#              The schedule is position-based (BA 2012: "every pick ... expressed as a percentage of the No. 1 pick");
#              checked on 2016, where published values = 2012 x (9,015,000/7,200,000) within +/-$200 at all 316 picks.
#   2016       MLB.com per-pick list (published)
#   2017-2025  Stats API pickValue, rounds 1-10 (cross-checked against MLB.com per-pick lists)

is_top10_label <- function(lbl) {
  r <- suppressWarnings(as.integer(lbl))
  ifelse(is.na(r), TRUE, r <= 10)   # CB-A, CB-B, C-A, SUP, 1C, 2C, 4C, PPI are all within rounds 1-10
}

# scale anchors: pick-1 value (cited) or stated growth over prior year
slot_scale <- function() tibble::tribble(
  ~year, ~pick1, ~basis,
  2012L, 7200000, "BA 2012 per-pick list",
  2013L, 7200000 * 1.082, "BA 2013: assigned values +8.2% vs 2012 (pick-1 value not published in source)",
  2014L, 7922100, "MLB.com 2014: No. 1 pick valued at $7,922,100",
  2015L, 8616900, "MLB.com 2015: No. 1 pick valued at $8,616,900",
  2016L, 9015000, "MLB.com 2016 per-pick list"
)

build_slots <- function(sa, art) {
  sched12 <- art[["2012"]]$picks |> dplyr::select(pick_overall, v12 = value)
  top10 <- sa |> dplyr::filter(is_top10_label(round_label)) |> dplyr::select(year, pick_overall, team_id)
  out <- list()
  # 2012 & 2016: published lists
  for (y in c(2012L, 2016L)) {
    p <- art[[as.character(y)]]$picks
    out[[length(out) + 1]] <- dplyr::transmute(p, year = y, pick_overall, slot = value, list_team_id = team_id,
      slot_source = if (y == 2012) "BA per-pick list" else "MLB.com per-pick list", slot_status = "published")
  }
  # 2013-2015: derived from schedule
  sc <- slot_scale()
  for (y in 2013:2015) {
    n <- sum(top10$year == y)
    s <- sc$pick1[sc$year == y] / 7200000
    d <- dplyr::filter(sched12, pick_overall <= n)
    out[[length(out) + 1]] <- dplyr::transmute(d, year = y, pick_overall, slot = round(v12 * s / 100) * 100, list_team_id = NA_integer_,
      slot_source = sprintf("2012 schedule x %.6f (%s)", s, sc$basis[sc$year == y]), slot_status = "derived")
  }
  # 2017+: Stats API
  sav <- sa |> dplyr::filter(year >= 2017, is_top10_label(round_label), !is.na(sa_pick_value), sa_pick_value > 0)
  out[[length(out) + 1]] <- dplyr::transmute(sav, year, pick_overall, slot = sa_pick_value, list_team_id = team_id,
    slot_source = "Stats API pickValue", slot_status = "published")
  dplyr::bind_rows(out)
}

# Compare article per-pick lists with the slot table (2017-2025)
slot_crosscheck <- function(slots, art) {
  dplyr::bind_rows(lapply(2017:2025, function(y) {
    a <- art[[as.character(y)]]$picks
    s <- dplyr::filter(slots, year == y)
    j <- dplyr::full_join(dplyr::select(a, pick_overall, art_value = value, art_team = team_id),
                          dplyr::select(s, pick_overall, slot, list_team_id), by = "pick_overall")
    tibble::tibble(year = y, n_article = nrow(a), n_statsapi = nrow(s),
                   n_both = sum(!is.na(j$art_value) & !is.na(j$slot)),
                   n_value_equal = sum(j$art_value == j$slot, na.rm = TRUE),
                   n_team_equal = sum(j$art_team == j$list_team_id, na.rm = TRUE),
                   first_mismatch_pick = suppressWarnings(min(j$pick_overall[is.na(j$art_value) | is.na(j$slot) | j$art_value != j$slot])))
  }))
}

# Team pools: reconstructed from draft-day pick ownership vs published pre-draft totals
pool_reconstruction <- function(sa, slots, art) {
  own <- sa |> dplyr::filter(is_top10_label(round_label)) |> dplyr::select(year, pick_overall, team_id, round_label)
  j <- dplyr::left_join(own, dplyr::select(slots, year, pick_overall, slot), by = c("year", "pick_overall"))
  rec <- j |> dplyr::group_by(year, team_id) |>
    dplyr::summarise(n_picks_draft = dplyr::n(), n_missing_slot = sum(is.na(slot)), pool_reconstructed = sum(slot, na.rm = TRUE), .groups = "drop")
  pub <- dplyr::bind_rows(lapply(names(art), function(y) {
    p <- art[[y]]$pools
    if (is.null(p)) {   # 2012: published per-pick list only -> pools are sums by listed team
      p <- art[[y]]$picks |> dplyr::group_by(team_id) |> dplyr::summarise(pool = sum(value), n_picks = dplyr::n())
    }
    dplyr::transmute(p, year = as.integer(y), team_id, pool_published = pool, n_picks_published = n_picks)
  }))
  dplyr::full_join(rec, pub, by = c("year", "team_id")) |>
    dplyr::mutate(diff = pool_reconstructed - pool_published, rel_diff = diff / pool_published)
}
