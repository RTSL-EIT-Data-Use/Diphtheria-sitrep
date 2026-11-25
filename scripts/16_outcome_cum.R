
cum_data <- merged_diph %>% 
  filter(epi_year %in% c(2023, 2024, 2025, 2026))


year_month_labels <- function(x) {
  month_day <- format(x, "%d %b")
  year      <- format(x, "%Y")
  show_year <- c(TRUE, year[-1] != head(year, -1))
  label     <- ifelse(show_year, paste0(month_day, "\n", year), month_day)
  ifelse(is.na(x), "", label)
}

# explicit breaks starting exactly at 2023-01-01, every 12 weeks
custom_breaks <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = max(cum_data$dateofonset_day_mnth_yr, na.rm = TRUE),
  by   = "12 weeks"
)

outcome_epicurve <- ggplot(data = cum_data)+
  geom_histogram(
    mapping = aes(x = dateofonset_day_mnth_yr,
                  group = outcome,
                  fill = outcome),
    breaks = weekly_breaks_all,
    closed = "left",
    color = "gray31")+
  scale_x_date(
    expand = c(0,4),
    limits = c(as.Date("2023-01-01"), NA),   # <- force axis to start at 2023
    breaks = custom_breaks,                   # <- explicit breaks, no auto-anchoring
    minor_breaks = "4 week",
    labels = year_month_labels)+
  scale_fill_manual(
    values = c("#00B050", "#C00000"),
    name = "") +
  labs(x = "Epiweek/yr",
       y = "# of cases") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(hjust = 0.5, lineheight = 0.9),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(8, 8, 18, 8),
    plot.background = element_rect(color = scales::alpha("gray60", 0.6), fill = NA)
  )

outcome_epicurve
