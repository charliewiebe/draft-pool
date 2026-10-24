# Data dictionary (Stage 2)

Money is nominal USD in the draft year unless stated. "SA" = MLB Stats API; "B-Ref" = Baseball-Reference.
Section 3 normalizations (bonus_rel, CPI, log offsets) are not yet built.

## data/derived/picks.csv — one row per drafted player-pick (year x pick_overall), 2012-2025

| column | definition | source |
|---|---|---|
| year, pick_overall | draft year; overall pick number | SA `/draft/{year}` (B-Ref for 4 picks absent from SA) |
| round_label | SA pickRound (1-40, CB-A/CB-B/C-A/C-B, SUP, 1C/2C/4C, PPI); B-Ref round for B-Ref-only rows | SA / B-Ref |
| round_block, pick_display, pick_in_round | SA round container, displayed pick, pick within round | SA |
| team_id, team_name | drafting club (draft-day owner of the pick) | SA |
| mlbam_id, bis_id, full_name | MLBAM person id, BIS id, name | SA |
| birth_date, birth_country | from SA draft person object | SA |
| pos_code, pos_abbr, pos_type | SA **current** primary position (not necessarily as drafted) | SA |
| bats, throws, mlb_debut | handedness; MLB debut date in draft file | SA |
| school_name, school_class, school_state, school_country, home_state, home_country | school_class only 2018, 2020-2025 | SA |
| pre_draft_rank | SA draft-file rank (2017+) | SA |
| sa_pick_value | SA pickValue (rounds 1-10; 0 for 11+ in 2017/2025) | SA |
| sa_signing_bonus | SA signingBonus as reported (2017+; 0 = not reported/unsigned) | SA |
| br_round, br_round_abb, br_round_pick, br_team, br_player, br_pos | B-Ref round (supplemental picks filed under preceding round), drafted position | B-Ref draft pages |
| br_signed | B-Ref signed flag Y/N/Unk | B-Ref |
| br_bonus_raw, br_bonus | B-Ref bonus text and numeric | B-Ref |
| br_war_career, br_from_type, br_came_from | B-Ref career WAR at scrape; HS/4Yr/JC; school text | B-Ref |
| bbref_id, bbref_minors_id | B-Ref MLB and register ids from player links | B-Ref |
| in_sa, in_br, name_agree | row present in each source; names agree (normalized, or same last name + first initial) | derived |
| slot, slot_status, slot_source | assigned pick value (rounds 1-10); `published` or `derived` (2013-15) | see slot_values.csv |
| pool_round | pick counts toward the pool (rounds 1-10 incl. CB/comp/supplemental/PPI) | derived from round_label |
| people_debut | MLB debut date (people endpoint, pull 2026-10-20) | SA people |
| first_pro_season | first season with an affiliated MLB/MiLB appearance | SA people stats |
| pro_by_next_season | first_pro_season <= year + 1 | derived |
| player_key, draft_instance, n_times_drafted, later_drafted | repeat-draftee linkage; later_drafted = drafted again in a later year | derived |
| sa_bonus_pos | sa_signing_bonus if > 0, else NA | derived |
| bonus_status | agree (<= $1,000) / disagree / sa_only / br_only / neither | derived |
| bonus | reconciled bonus (USD); NA if unreported | derived (rule in bonus_source_reason) |
| bonus_source_reason | which source was used and why | derived |
| bonus_diff | SA - B-Ref where they disagree | derived |
| signed, signed_basis, signed_conflict | reconciled signed status, evidence used, B-Ref N with SA bonus > 0 | derived |
| signing_pick, n_signed_picks | the pick at which the player signed (outcomes belong here); count per player | derived |
| school_type, school_type_basis, school_type_agree | HS / 4yr / JUCO; B-Ref from_type primary, SA schoolClass fallback; agreement where both exist | derived |
| role, role_basis, role_agree | P / H as drafted (B-Ref drafted position; SA current position fallback) | derived |
| two_way_flag | SA TWP or B-Ref position combining pitcher and hitter | derived |
| college_class | SO / JR / SR / 5S-GR / J1-J3 (JUCO) where SA schoolClass exists | SA |
| r11_threshold | rounds 11+ threshold for the year | rules_by_year.csv |
| ratio_to_slot | bonus / slot (pool rounds) | derived |
| r11_at_or_below_threshold, r11_excess_counts | rounds 11+: bonus <= threshold; max(0, bonus - threshold) | derived |
| draft_start | first day of the draft | draft_dates.csv |
| age_at_draft | (draft_start - birth_date) / 365.25 | derived |
| age_june5, rule5_young | age on June 5 of draft year; age <= 18 on that date (5th Rule 5 draft eligibility) | derived; rule in rules_by_year.csv |
| pipeline_draft_rank | MLB Pipeline pre-draft rank (supply modeling only) | pipeline_rankings.csv |
| chad_bbref, chad_bbref_minors, chad_fangraphs, chad_linked, chad_bbref_minors_agree | Chadwick register ids by MLBAM id; agreement with B-Ref register id | Chadwick register |
| src_file | B-Ref page the row came from | B-Ref |

## data/derived/rules_by_year.csv
year, cba, rule, value, source_url, status (verified_source / observed_in_data / inferred_same_cba / current_rule_page), note.

## data/derived/slot_values.csv
year, pick_overall, slot (USD), list_team_id (team on the published list), slot_source, slot_status.

## data/derived/pool_reconstruction.csv
year, team_id, n_picks_draft (draft-day pool picks), n_missing_slot, pool_reconstructed (sum of slots), pool_published,
n_picks_published (2013-14 only), diff, rel_diff.

## data/derived/bonus_reconciliation.csv
Per player-pick: both sources' names, teams, bonuses, signed flags; status, chosen bonus and reason; pro-appearance and
repeat-draft evidence; reconciled signed status and basis.

## data/derived/player_seasons.csv
mlbam_id, season, mlb_games (SA), milb_top_level (ROK/ROA/A-/A/A+/AA/AAA), milb_games (sum of hitting+pitching lines;
presence indicator), bwar, bwar_bat, bwar_pitch, pa, ipouts (B-Ref war_daily). Restricted to drafted players.

## data/derived/pipeline_rankings.csv
rank, mlbam_id, position, list_year, list (`draft` = pre-draft list; `top100` = MLB Pipeline Top 100 as served for that year).

## data/derived/draft_dates.csv
year, draft_start, evidence (sentence matched), source_url.

# Stage 3 additions

## data/derived/year_index.csv
| column | definition |
|---|---|
| slot1 | slot of pick 1 |
| n_pool_picks | picks in rounds 1-10 (pool picks) |
| basket_sum | sum of slots at picks 1-300 (2020 set to 2019: 160 pool picks, slots frozen at 2019 values) |
| sched_index | basket_sum / basket_sum(2012) — **primary money index** |
| league_pool | sum of published team pools |
| cpi, cpi_to_base | CPI-U annual average (BLS CUUR0000SA0 via datasets/cpi-us mirror); factor to 2025 dollars |
| sched_to_2025 | sched_index(2025) / sched_index(year) |

## data/derived/picks_clean.csv (picks.csv plus)
| column | definition |
|---|---|
| bonus_rel | bonus / (sched_index x $7,200,000): units of the 2012 No. 1 slot at that year's schedule level (primary) |
| bonus_rel_p1, bonus_rel_pool | bonus / slot of pick 1; bonus / league pool (spec alternatives; break in 2017 and 2020 respectively) |
| slot_rel, slot_rel_p1, slot_rel_pool | same normalizations of the slot |
| bonus_2025cpi | bonus in 2025 CPI dollars (reporting only) |
| bonus_2025sched | bonus at the 2025 draft-market level (bonus x sched_to_2025) |
| bonus_pv, bonus_rel_pv | 2020: present value under the mandated deferral at r = 0.08 (`pv_2020_bonus()`); other years = bonus |
| log_bonus_rel_o{500,1000,5000,10000} | log((bonus + offset) / (sched_index x 7.2M)), offsets in dollars |
| bonus_cens | exact / right_at_threshold (R11+ bonus == threshold: value >= threshold) / missing (signed, unreported) / not_signed |
| bonus_lo, bonus_hi | censoring interval in dollars (Inf = unbounded) |
| senior_nominal | signed with bonus <= $10,000 |
| state, region, cold_weather | state from B-Ref "School (City, ST)" (Stats API fallback); Census region; cold-weather state flag (signing-model covariate only) |
| failure_class, failure_source | documented reason for unsigned rounds 1-10 picks (data/manual/signing_failures.csv) |
| signing_status | signed / unsigned_medical / unsigned_pool_constraint / unsigned_bonus_or_school / unsigned_reason_not_documented / unresolved |
| comp_eligible_round | rounds 1-3 or unsigned-pick comp pick (SUP*); excludes CB, QO and PPI picks |

## data/derived/bonus_imputations.csv
year, pick_overall, m (1-20), bonus_imp (USD), method. Covers every signed pick with no reported bonus.

## data/derived/imputation_reporting_model.csv
Per year (2012-16) and bonus band (threshold units): fbar (expected share from 2017-19 donors), expected_n, reported_n,
pi (estimated B-Ref reporting probability, capped at 1), capped.

## data/derived/udfa_2020_candidates.csv
Stats API SFA transactions 2020-06-14..09-30 with filters: prior_pro, signed_rule4_before, age_2020, minor_league_contract,
udfa_2020 (final flag). Bonus capped at $20,000 (not reported individually).

## data/derived/player_outcomes.csv — one row per signing pick
| column | definition |
|---|---|
| debut_season, debuted, seasons_to_debut | debut year; debut year - draft year |
| first/last_mlb_season, mlb_seasons | distinct seasons with an MLB appearance |
| v_war_control_obs, control_seasons_obs, control_last_season | bWAR summed over the first <= 6 MLB seasons observed (control-period approximation) |
| war_to_date, peak_war | all bWAR through 2026; best single season |
| first/last_pro_season, out_of_pro | affiliated seasons; out_of_pro = no affiliated appearance in 2025-26 |
| arrival_status | arrived / never_arrived (out of pro without debut) / censored |
| control_status | complete (>= 6 MLB seasons) / exited_before_6 / censored / no_debut_resolved / no_debut_censored |
| v_war_resolved, v_war, v_war_lower_bound | v_war defined only when resolved; lower bound = observed to date |
| top100_best_rank, top100_lists, top100_first_year, ever_top100 | MLB Pipeline Top 100 history |

## data/derived/player_progress.csv — signed player x season (draft year..2026)
years_since_draft, milb_top_level, mlb_games, bwar, top100_rank.
