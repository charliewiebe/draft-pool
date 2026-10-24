# Stage 7 figures
plot_q3 <- function(q3, qf, path = "reports/figures/stage7_transition_pick.png") {
  long <- dplyr::bind_rows(
    q3 |> dplyr::transmute(p, rule = "Best policy of any class", gain = best_gain / 1e6, lo = best_gain_lo / 1e6, hi = best_gain_hi / 1e6),
    q3 |> dplyr::transmute(p, rule = "Best save-at-top (static)", gain = save_gain / 1e6, lo = save_gain_lo / 1e6, hi = save_gain_hi / 1e6),
    q3 |> dplyr::transmute(p, rule = "Adaptive (re-optimizes each pick)", gain = adaptive_gain / 1e6, lo = adaptive_gain_lo / 1e6, hi = adaptive_gain_hi / 1e6))
  cols <- c(`Best policy of any class` = "#0b0b0b", `Best save-at-top (static)` = "#eb6834", `Adaptive (re-optimizes each pick)` = "#2a78d6")
  p1 <- ggplot2::ggplot(long, ggplot2::aes(p, gain, colour = rule, fill = rule)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#8a8984", linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) + ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) + ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::labs(x = "First pick (overall)", y = "Gain over BPA ($M expected surplus)",
                  subtitle = "Gain in expected class surplus over best-available-at-slot; 90% band across posterior draws") + theme_pool()
  qf <- dplyr::bind_rows(qf)
  opt <- qf |> dplyr::group_by(p) |> dplyr::slice_max(e_surplus, n = 1, with_ties = FALSE) |> dplyr::ungroup()
  sel <- qf |> dplyr::filter(p %in% c(1, 10, 24, 40)) |> dplyr::mutate(pos = factor(paste0("First pick #", p), paste0("First pick #", c(1, 10, 24, 40))))
  p2 <- ggplot2::ggplot(sel, ggplot2::aes(s1, gain_vs_first_at_slot / 1e6, colour = pos)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#8a8984", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a", "#6250d6"), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%")) +
    ggplot2::labs(x = "Share of slot saved at the first pick (rest of the draft adaptive)", y = "vs first pick at slot ($M)",
                  subtitle = "Value of the first-pick savings alone: same adaptive continuation, first pick at slot vs under slot") + theme_pool()
  g <- gridExtra::arrangeGrob(p1, p2, ncol = 1, heights = c(1.3, 1),
    top = grid::textGrob("Where saving at the top pays: gain over BPA by first-pick position (2025 order and slots)", x = 0.01, hjust = 0,
                         gp = grid::gpar(fontface = "bold", fontsize = 13, col = "#0b0b0b")))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, g, width = 10, height = 8.5, dpi = 150, bg = "#fcfcfb"); path
}

plot_exchange <- function(ex, path = "reports/figures/stage7_exchange_rate.png") {
  d <- ex |> dplyr::mutate(dest = ifelse(is.na(to_pick), to_round, paste0(to_round, " (#", to_pick, ")")),
                           from = paste0("Moving $1M from pick ", from_pick), school = factor(school, c("any", "4yr", "HS")))
  d$dest <- factor(d$dest, unique(d$dest[order(ifelse(is.na(d$to_pick), 999, d$to_pick))]))
  p <- ggplot2::ggplot(d, ggplot2::aes(gain / 1e6, dest, colour = school)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#8a8984", linewidth = 0.4) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = gain_lo / 1e6, xmax = gain_hi / 1e6), height = 0, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::facet_wrap(~from, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(any = "#0b0b0b", `4yr` = "#eb6834", HS = "#2a78d6"), name = "Player bought with the $1M",
                                 labels = c("best available", "four-year", "high school")) +
    ggplot2::labs(x = "Change in expected class surplus ($M), 90% band across posterior draws", y = NULL,
                  title = "Exchange rate: what $1M of pool space is worth when moved from the first pick") + theme_pool()
  ggplot2::ggsave(path, p, width = 11, height = 6.5, dpi = 150, bg = "#fcfcfb"); path
}
