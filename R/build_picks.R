# Join Stats API and Baseball-Reference picks; reconcile bonus and signed status; derive types.

norm_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^A-Za-z ]", "", x))
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}

# Name agreement: same last token and same first initial, or identical normalized strings
name_match <- function(a, b) {
  a <- norm_name(a); b <- norm_name(b)
  last <- function(s) sub(".* ", "", s); first1 <- function(s) substr(s, 1, 1)
  !is.na(a) & !is.na(b) & (a == b | (last(a) == last(b) & first1(a) == first1(b)))
}

join_sources <- function(sa, br) {
  j <- dplyr::full_join(sa, br, by = c("year", "pick_overall"))
  j$in_sa <- !is.na(j$mlbam_id); j$in_br <- !is.na(j$br_player)
  j$name_agree <- name_match(j$full_name, j$br_player)
  j
}

school_type_from_class <- function(cls) dplyr::case_when(
  grepl("^HS", cls) ~ "HS", grepl("^JC", cls) ~ "JUCO",
  grepl("^4YR", cls) | cls %in% c("JR", "SR", "SO", "FR", "5S", "GR") ~ "4yr", TRUE ~ NA_character_)

school_type_from_br <- function(t) dplyr::case_when(t == "HS" ~ "HS", t == "4Yr" ~ "4yr", t == "JC" ~ "JUCO", TRUE ~ NA_character_)

# Bonus reconciliation. Stats API reports 0 for some picks (2017-19); treat 0 as "not reported", never as a bonus,
# unless B-Ref also reports the pick as unsigned (then it carries no bonus information either way).
# Requires j$slot (rounds 1-10). Disagreement rule: Stats API (MLB) preferred, except two documented Stats API
# error patterns, where B-Ref is used: (a) Stats API value equals the pick's slot exactly while B-Ref differs by >10%
# (slot stored in place of bonus); (b) unit error (Stats API < $10,000 and B-Ref ~1,000x it).
reconcile_bonus <- function(j, tol_abs = 1000) {
  sa_b <- ifelse(!is.na(j$sa_signing_bonus) & j$sa_signing_bonus > 0, j$sa_signing_bonus, NA_real_)
  br_b <- j$br_bonus
  status <- dplyr::case_when(
    !is.na(sa_b) & !is.na(br_b) & abs(sa_b - br_b) <= tol_abs ~ "agree",
    !is.na(sa_b) & !is.na(br_b) ~ "disagree",
    !is.na(sa_b) ~ "sa_only",
    !is.na(br_b) ~ "br_only",
    TRUE ~ "neither")
  slot_placeholder <- status == "disagree" & !is.na(j$slot) & sa_b == j$slot & abs(sa_b - br_b) / br_b > 0.10
  unit_error <- status == "disagree" & sa_b < 10000 & dplyr::between(br_b / sa_b, 900, 1100)
  use_br <- status == "br_only" | slot_placeholder | unit_error
  chosen <- dplyr::case_when(use_br ~ br_b, status %in% c("agree", "disagree", "sa_only") ~ sa_b, TRUE ~ NA_real_)
  reason <- dplyr::case_when(
    status == "agree" ~ "sources agree (<= $1,000)",
    slot_placeholder ~ "B-Ref used: Stats API equals slot exactly, B-Ref differs >10% (suspected slot placeholder)",
    unit_error ~ "B-Ref used: Stats API ~1/1000 of B-Ref (suspected unit error)",
    status == "disagree" ~ "Stats API (MLB) preferred; B-Ref differs",
    status == "sa_only" ~ "Stats API only",
    status == "br_only" ~ "B-Ref only (Stats API has no bonus field before 2017 or reports none)",
    TRUE ~ "no bonus reported")
  j$sa_bonus_pos <- sa_b; j$bonus_status <- status; j$bonus <- chosen; j$bonus_source_reason <- reason
  j$bonus_diff <- ifelse(status == "disagree", sa_b - br_b, NA_real_)
  j
}

# Signed status. Requires pro_by_next_season and later_drafted.
# Order of evidence: (1) drafted again in a later year -> this pick was not signed (Rule 4 eligibility);
# (2) B-Ref Y or a positive Stats API bonus -> signed; B-Ref N with a Stats API bonus is resolved as signed
#     only when an affiliated pro appearance follows by the next season (otherwise left unresolved);
# (3) B-Ref N -> unsigned; (4) B-Ref unknown + pro appearance by next season -> signed.
reconcile_signed <- function(j) {
  br_y <- j$br_signed %in% "Y"; br_n <- j$br_signed %in% "N"
  sa_pos <- !is.na(j$sa_bonus_pos); pro <- j$pro_by_next_season %in% TRUE; later <- j$later_drafted %in% TRUE
  j$signed_conflict <- br_n & sa_pos
  j$signed_basis <- dplyr::case_when(
    later & (br_y | sa_pos) ~ "unsigned: drafted again later (overrides source 'signed')",
    later ~ "unsigned: drafted again later",
    br_y & sa_pos ~ "B-Ref Y + Stats API bonus",
    br_y ~ "B-Ref Y",
    br_n & sa_pos & pro ~ "signed: Stats API bonus + pro appearance by next season (B-Ref N overridden)",
    br_n & sa_pos ~ "UNRESOLVED: B-Ref N, Stats API bonus > 0, no pro appearance",
    sa_pos ~ "Stats API bonus > 0 (B-Ref unknown)",
    br_n ~ "B-Ref N",
    pro ~ "signed: pro appearance by next season (B-Ref unknown)",
    TRUE ~ "unknown")
  j$signed <- dplyr::case_when(
    later ~ FALSE, br_y ~ TRUE, br_n & sa_pos & pro ~ TRUE, br_n & sa_pos ~ NA, sa_pos ~ TRUE, br_n ~ FALSE, pro ~ TRUE, TRUE ~ NA)
  j
}

derive_types <- function(j) {
  st_br <- school_type_from_br(j$br_from_type); st_sa <- school_type_from_class(j$school_class)
  j$school_type <- dplyr::coalesce(st_br, st_sa)
  j$school_type_basis <- dplyr::case_when(!is.na(st_br) ~ "B-Ref from_type", !is.na(st_sa) ~ "Stats API schoolClass", TRUE ~ "unknown")
  j$school_type_agree <- ifelse(!is.na(st_br) & !is.na(st_sa), st_br == st_sa, NA)
  # role at draft: B-Ref drafted position first (as drafted); Stats API primary position is current, not at draft
  br_p <- grepl("hp$|^p$", j$br_pos, ignore.case = TRUE); br_h <- !is.na(j$br_pos) & nzchar(j$br_pos) & !br_p
  sa_p <- j$pos_code %in% "1"
  j$role <- dplyr::case_when(br_p ~ "P", br_h ~ "H", sa_p ~ "P", !is.na(j$pos_code) ~ "H", TRUE ~ NA_character_)
  j$role_basis <- ifelse(!is.na(j$br_pos) & nzchar(j$br_pos), "B-Ref drafted position", "Stats API current primary position")
  j$role_agree <- ifelse(!is.na(j$br_pos) & nzchar(j$br_pos) & !is.na(j$pos_code), (j$role == "P") == sa_p, NA)
  # two-way: Stats API lists TWP (current) or B-Ref drafted position combines pitcher and hitter tokens
  j$two_way_flag <- j$pos_abbr %in% "TWP" | (grepl("[/-]", j$br_pos) & grepl("hp", j$br_pos, ignore.case = TRUE))
  j$college_class <- dplyr::case_when(
    grepl("SO$", j$school_class) ~ "SO", grepl("JR$", j$school_class) ~ "JR", grepl("SR$", j$school_class) ~ "SR",
    grepl("5S$|GR$", j$school_class) ~ "5S/GR", grepl("^JC", j$school_class) ~ sub("^JC ", "", j$school_class), TRUE ~ NA_character_)
  j
}
