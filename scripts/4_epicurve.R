
# ---- 1) Choose the window ---------------------------------------------------
# Before week 12: cross-year view (wk45+ of last year through the current
# week). From week 12 onward: current year to date only.
use_prior_year <- report_week < 12

# ---- 1a) Sum actual counts per week (only weeks that have data) -------------
weekly_counts <- epi_summ %>%
  filter(
    (use_prior_year & epi_year == report_year - 1 & epi_week >= 45) |
      (epi_year == report_year & epi_week <= report_week)
  ) %>%
  group_by(epi_year, epi_week) %>%
  summarise(
    suspected = sum(suspected, na.rm = TRUE),
    deaths    = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 1b) Build EVERY week in that window using real Mondays (handles 52- vs
#          53-week years). The SAME conditional filter drops the prior-year
#          tail automatically once report_week >= 12. -------------------------
all_weeks <- tibble(
  wk_date = seq(monday(report_date) - lubridate::weeks(70),
                monday(report_date),
                by = "1 week")
) %>%
  mutate(
    epi_year = isoyear(wk_date),
    epi_week = isoweek(wk_date)
  ) %>%
  filter(
    (use_prior_year & epi_year == report_year - 1 & epi_week >= 45) |
      (epi_year == report_year & epi_week <= report_week)
  ) %>%
  distinct(epi_year, epi_week)

# ---- 1c) Join counts onto the full week list; missing weeks become zero -----
diph_epicurv <- all_weeks %>%
  left_join(weekly_counts, by = c("epi_year", "epi_week")) %>%
  mutate(
    suspected = replace_na(suspected, 0),
    deaths    = replace_na(deaths, 0),
    alive     = suspected - deaths
  ) %>%
  arrange(epi_year, epi_week) %>%
  mutate(week_index = row_number())

# ---- 2) Pivot longer for plotting ----
epicurve_data <- diph_epicurv %>%
  pivot_longer(
    cols = c(alive, deaths),
    names_to = "case_type",
    values_to = "count"
  )

# Max stacked total per week (suspected + deaths)

# Tallest weekly total (bars are stacked alive + deaths = suspected)
y_max <- max(diph_epicurv$suspected, 1, na.rm = TRUE)

# Gridline spacing grows with the size of the outbreak, then the axis ceiling is
# rounded UP to that spacing. Keeps the scale stable across weeks and only changes
# the step when counts cross 100, 300, 1000, or 5000.
#   y_max < 100      -> ticks every 10
#   y_max 100-300    -> ticks every 50
#   y_max 301-1000   -> ticks every 100
#   y_max 1001-5000  -> ticks every 500
#   y_max > 5000     -> ticks every 2000
y_step <- dplyr::case_when(
  y_max < 100   ~ 10,
  y_max <= 300  ~ 50,
  y_max <= 1000 ~ 100,
  y_max <= 5000 ~ 500,
  TRUE          ~ 2000
)
y_limit <- ceiling(y_max / y_step) * y_step

# --- Put YEAR labels exactly at the split (week 45 of prev year, week 1 of report year) ---

# ticks (epi_week by 2 + endpoints) stays the same
tick_df <- diph_epicurv %>%
  filter(
    (epi_year == report_year - 1 & epi_week %in% seq(45, 53, by = 2)) |
      (epi_year == report_year     & epi_week %in% seq(1, report_week, by = 2)) |
      epi_week %in% c(45, report_week)
  ) %>%
  distinct(week_index, epi_week, epi_year) %>%
  arrange(week_index)

# year labels at the first week present for each year (robust to quiet weeks)
# - prev year label at epi_week 45
# - report year label at epi_week 1 (if present in window)
year_tick_df <- diph_epicurv %>%
  group_by(epi_year) %>%
  slice_min(week_index, n = 1) %>%   # first week present for each year
  ungroup() %>%
  transmute(week_index, year_lbl = as.character(epi_year))

tick_df <- tick_df %>%
  left_join(year_tick_df, by = "week_index") %>%
  mutate(
    year_lbl = replace_na(year_lbl, ""),
    x_lbl = paste0(epi_week, "\n", year_lbl)  # week on top, year below at split only
  )

# --- split position (first row of report_year in the cross-year series) ---
year_split_index <- diph_epicurv %>%
  filter(epi_year == report_year) %>%
  summarise(split = min(week_index, na.rm = TRUE)) %>%
  pull(split)

# Only draw the year divider in the cross-year view; NULL adds nothing to a ggplot.
split_line <- if (use_prior_year) {
  geom_vline(xintercept = year_split_index - 0.5,
             linewidth = 0.8, linetype = "dashed", color = "gray50")
} else {
  NULL
}

epicurve <- ggplot(epicurve_data, aes(x = week_index, y = count, fill = case_type)) +
  geom_col() +
  split_line +
  scale_fill_manual(
    values = c("deaths" = "#FFC82D", "alive" = "#138750"),
    name = "",
    labels = c("deaths" = "Dead", "alive" = "Alive")
  ) +
  scale_x_continuous(
    breaks = tick_df$week_index,
    labels = tick_df$x_lbl,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, y_limit),
    breaks = seq(0, y_limit, by = y_step),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Epidemiological Week of Symptom Onset",
    y = "Number of Suspected Cases"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(hjust = 0.5, lineheight = 0.9),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(8, 8, 18, 8),
    plot.background = element_rect(color = scales::alpha("gray60", 0.6), fill = NA)
  )
epicurve
