# 1) Prepare weekly confirmed case counts
trend_data <- epi_summ %>%
  filter(epi_year %in% (report_year - 4):report_year) %>%
  group_by(epi_year, epi_week) %>%
  summarise(suspected_cases = sum(suspected, na.rm = TRUE), .groups = "drop") %>%
  filter(epi_year != report_year | epi_week <= report_week)

# Optional but recommended: ensure all weeks 1:53 appear per year (missing weeks -> 0)
trend_data_full <- trend_data %>%
  tidyr::complete(epi_year, epi_week = 1:53, fill = list(suspected_cases = 0)) %>%
  filter(epi_year != report_year | epi_week <= report_week)

# --- Colors: distinct year colors kept, current reporting year stays red.
# Older years are faded via transparency instead of grey, so the current year
# stands out while past years remain identifiable by color.
palette_by_recency <- c("red", "#138750", "orange", "#341539", "blue", "purple")
plot_years  <- sort(unique(trend_data_full$epi_year), decreasing = TRUE)
year_colors <- setNames(palette_by_recency[seq_along(plot_years)], plot_years)

# current year fully opaque, older years faded but still clearly visible
year_alphas <- setNames(c(1, rep(0.6, length(plot_years) - 1)), plot_years)

# current year thicker, older years thinner
line_widths <- setNames(c(1.8, rep(0.7, length(plot_years) - 1)), plot_years)
point_sizes <- setNames(c(1.8, rep(0.8, length(plot_years) - 1)), plot_years)

# y-axis breaks: nice round numbers up to just above the peak. The highest
# break becomes the top of the plot, so a label is always shown there.
y_breaks <- pretty(c(0, max(trend_data_full$suspected_cases, na.rm = TRUE)), n = 4)

# 2) Plot with one line per year
trend_year_sus <- ggplot(
  trend_data_full,
  aes(x = epi_week, y = suspected_cases, color = factor(epi_year))
) +
  geom_line(aes(linewidth = factor(epi_year), alpha = factor(epi_year))) +
  geom_point(aes(size = factor(epi_year), alpha = factor(epi_year))) +
  scale_color_manual(name = "", values = year_colors) +
  scale_alpha_manual(values = year_alphas, guide = "none") +
  scale_linewidth_manual(values = line_widths, guide = "none") +
  scale_size_manual(values = point_sizes, guide = "none") +
  scale_x_continuous(
    breaks = seq(1, 53, by = 2),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    breaks = y_breaks,
    limits = c(0, max(y_breaks)),
    expand = c(0, 0)
  ) +
  labs(
    x = "Epidemiological Week",
    y = "Confirmed Cases"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(face = "bold"),
    
    plot.background = element_rect(color = scales::alpha("gray60", 0.6), fill = NA)
  )

trend_year_sus