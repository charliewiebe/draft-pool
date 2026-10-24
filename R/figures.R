# Stage 4 figure: arrival probability and expected surplus vs bonus, by school type and role (default valuation).
plot_value_curves <- function(gs, path = "reports/figures/stage4_value_curves.png", value_classes = "2012-16 classes") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  d <- gs[gs$age_c == 0, ]
  d$school <- factor(sub("-.*", "", d$type), levels = c("HS", "4yr", "JUCO"), labels = c("High school", "Four-year college", "Junior college"))
  d$role <- factor(ifelse(grepl("-P$", d$type), "Pitcher", "Hitter"), levels = c("Hitter", "Pitcher"))
  long <- rbind(
    data.frame(d[, c("school", "role", "bonus")], panel = "P(MLB arrival)", y = d$p_arr, lo = d$p_arr_lo, hi = d$p_arr_hi),
    data.frame(d[, c("school", "role", "bonus")], panel = "E[surplus] ($M)", y = d$e_surplus / 1e6,
               lo = d$e_surplus_lo / 1e6, hi = d$e_surplus_hi / 1e6))
  long$panel <- factor(long$panel, levels = c("P(MLB arrival)", "E[surplus] ($M)"))
  cols <- c(Hitter = "#2a78d6", Pitcher = "#eb6834")
  p <- ggplot2::ggplot(long, ggplot2::aes(bonus, y, colour = role, fill = role)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_grid(panel ~ school, scales = "free_y", switch = "y") +
    ggplot2::scale_x_log10(labels = function(x) ifelse(x >= 1e6, paste0("$", x / 1e6, "M"), paste0("$", x / 1e3, "k")),
                           breaks = c(1e4, 1e5, 1e6, 1e7)) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) + ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::labs(x = "Signing bonus (2025 draft-market dollars, log scale)", y = NULL,
                  title = "Value of a signed draftee by bonus, school type and role",
                  subtitle = paste0("Posterior mean with 90% band; surplus = discounted control-period value minus bonus (2025 $).\nJoint arrival model (2012-24 classes) x control-period WAR tiers (", value_classes, ");\n$8M per WAR above minimum, 8% discount, arbitration at 20/35/55% of market. Age at the school-type mean.")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "#e6e5e0", linewidth = 0.3),
                   strip.placement = "outside", strip.text = ggplot2::element_text(face = "bold", colour = "#0b0b0b"),
                   plot.title = ggplot2::element_text(face = "bold", colour = "#0b0b0b"),
                   plot.subtitle = ggplot2::element_text(colour = "#52514e", size = 9),
                   axis.text = ggplot2::element_text(colour = "#52514e"),
                   plot.background = ggplot2::element_rect(fill = "#fcfcfb", colour = NA))
  ggplot2::ggsave(path, p, width = 10, height = 6.8, dpi = 160, bg = "#fcfcfb")
  path
}
