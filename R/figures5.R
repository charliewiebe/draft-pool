# Stage 5 figures. Palette slots 1-3 (validated all-pairs): HS blue, four-year orange, junior college aqua.
school_cols <- function() c(HS = "#2a78d6", `4yr` = "#eb6834", JUCO = "#1baf7a")

theme_pool <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "#e6e5e0", linewidth = 0.3),
                   strip.text = ggplot2::element_text(face = "bold", colour = "#0b0b0b"),
                   plot.title = ggplot2::element_text(face = "bold", colour = "#0b0b0b"),
                   plot.subtitle = ggplot2::element_text(colour = "#52514e", size = 9),
                   axis.text = ggplot2::element_text(colour = "#52514e"),
                   plot.background = ggplot2::element_rect(fill = "#fcfcfb", colour = NA))
}

plot_deals <- function(dg, path = "reports/figures/stage5_deals_by_pick.png") {
  d <- dg |> dplyr::filter(!senior, !grepl("^JUCO", type)) |> dplyr::mutate(school = sub("-.*", "", type), role = ifelse(grepl("-P$", type), "Pitchers", "Hitters"))
  long <- dplyr::bind_rows(
    d |> dplyr::transmute(pick, school, role, panel = "P(over-slot deal)", y = p_over, lo = p_over_lo, hi = p_over_hi),
    d |> dplyr::transmute(pick, school, role, panel = "Mean bonus / slot", y = mean_ratio, lo = mean_ratio_lo, hi = mean_ratio_hi))
  long$panel <- factor(long$panel, levels = c("P(over-slot deal)", "Mean bonus / slot"))
  long$school <- factor(long$school, levels = c("HS", "4yr"))
  p <- ggplot2::ggplot(long, ggplot2::aes(pick, y, colour = school, fill = school)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) + ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_grid(panel ~ role, scales = "free_y", switch = "y") +
    ggplot2::scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300)) +
    ggplot2::scale_colour_manual(values = school_cols()[1:2], name = NULL, labels = c("High school", "Four-year (non-senior)")) +
    ggplot2::scale_fill_manual(values = school_cols()[1:2], name = NULL, labels = c("High school", "Four-year (non-senior)")) +
    ggplot2::labs(x = "Overall pick (log scale)", y = NULL, title = "Deal structure by pick, rounds 1-10 (2014-25 signed picks)",
                  subtitle = "A top-200 prospect taken at his pre-draft rank. Posterior mean and 90% band for a new draft year.\nOver-slot = bonus > 101.5% of slot. Junior-college curves are in the tables (few observations).") +
    theme_pool() + ggplot2::theme(strip.placement = "outside")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = 9.5, height = 6.2, dpi = 160, bg = "#fcfcfb"); path
}

plot_availability <- function(ag, path = "reports/figures/stage5_availability.png", ranks = c(5, 10, 20, 30, 50, 100)) {
  d <- ag |> dplyr::filter(rank %in% ranks, grepl("^(HS|4yr)", type)) |>
    dplyr::mutate(school = factor(sub("-.*", "", type), levels = c("HS", "4yr")), role = ifelse(grepl("-P$", type), "Pitcher", "Hitter"),
                  rank_lab = factor(paste0("Pre-draft rank ", rank), levels = paste0("Pre-draft rank ", ranks)))
  p <- ggplot2::ggplot(d, ggplot2::aes(pick, p_avail, colour = school, fill = school, linetype = role)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.10, colour = NA) + ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_wrap(~rank_lab, nrow = 2) + ggplot2::scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_colour_manual(values = school_cols()[1:2], name = NULL, labels = c("High school", "Four-year")) +
    ggplot2::scale_fill_manual(values = school_cols()[1:2], name = NULL, labels = c("High school", "Four-year")) +
    ggplot2::scale_linetype_manual(values = c(Hitter = "solid", Pitcher = "22"), name = NULL) +
    ggplot2::labs(x = "Overall pick (log scale)", y = "P(still available at this pick)",
                  title = "Faller supply: chance a ranked prospect is still on the board",
                  subtitle = "MLB Pipeline pre-draft rank, 2014-25 classes; includes players who leave the rounds 1-10 market. Posterior mean, 90% band.") +
    theme_pool()
  ggplot2::ggsave(path, p, width = 10, height = 6.4, dpi = 160, bg = "#fcfcfb"); path
}

plot_sim_validation <- function(val, path = "reports/figures/stage5_simulator_validation.png") {
  src_cols <- c(historical = "#0b0b0b", simulated = "#2a78d6", `simulated, no budget rule` = "#eb6834")
  src_shp <- c(historical = 16, simulated = 17, `simulated, no budget rule` = 4)
  pb <- c("1-10", "11-30", "31-75", "76-150", "151-225", "226+")
  rp <- val$ratio_by_pick |> tidyr::pivot_longer(c(q10, q50, q90), names_to = "stat", values_to = "y") |>
    dplyr::mutate(stat = factor(stat, c("q10", "q50", "q90"), c("10th percentile", "Median", "90th percentile")), x = factor(pbin, pb))
  rb <- val$by_round |> tidyr::pivot_longer(c(p_over, p_deep, hs_share), names_to = "stat", values_to = "y") |>
    dplyr::mutate(stat = factor(stat, c("p_over", "p_deep", "hs_share"), c("P(over-slot)", "P(deep under-slot)", "HS share of signees")),
                  x = factor(rbin, c("R1+C", "R2+CBB", "R3-5", "R6-10")))
  ut <- val$utilization |> tidyr::pivot_longer(c(q05, q25, q50, q75, q95), names_to = "stat", values_to = "y") |>
    dplyr::mutate(x = factor(stat, c("q05", "q25", "q50", "q75", "q95"), c("5th", "25th", "50th", "75th", "95th")))
  mk <- function(d, ttl, facet = TRUE) {
    g <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = source, shape = source)) +
      ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.55)) +
      ggplot2::scale_colour_manual(values = src_cols, name = NULL) + ggplot2::scale_shape_manual(values = src_shp, name = NULL) +
      ggplot2::labs(x = NULL, y = NULL, subtitle = ttl) + theme_pool()
    if (facet) g <- g + ggplot2::facet_wrap(~stat, nrow = 1, scales = "free_y")
    g
  }
  p1 <- mk(rp, "Bonus / slot by overall pick (log scale)") + ggplot2::scale_y_log10() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 8))
  p2 <- mk(rb, "By round group") + ggplot2::theme(legend.position = "none")
  p3 <- mk(ut, "Team rounds 1-10 spend / slot total, percentiles across team-drafts", facet = FALSE) + ggplot2::theme(legend.position = "none")
  g <- gridExtra::arrangeGrob(p1, p2, p3, ncol = 1, heights = c(1.15, 1, 0.8),
    top = grid::textGrob("Draft-market simulator vs history (rounds 1-10, 2014-25 excluding 2020)", x = 0.01, hjust = 0,
                         gp = grid::gpar(fontface = "bold", fontsize = 13, col = "#0b0b0b")))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, g, width = 11, height = 10, dpi = 150, bg = "#fcfcfb"); path
}

patchwork_or_grid <- function(ps) {
  if (requireNamespace("patchwork", quietly = TRUE)) return(Reduce(`/`, ps) + patchwork::plot_annotation(title = "Draft-market simulator vs history (rounds 1-10, 2014-25 excl. 2020)"))
  gridExtra::arrangeGrob(grobs = ps, ncol = 1)
}
