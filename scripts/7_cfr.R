
# --- 1) Summarise cases + CFR (keep all state_of_residences with suspected > 0) ---
case_data_processed <- epi_summ %>%
  filter(epi_year == report_year, epi_week <= report_week) %>%
  group_by(state_of_residence) %>%
  summarise(
    suspected = sum(suspected, na.rm = TRUE),
    deaths    = sum(deaths,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(suspected > 0) %>%           # keep reporting state_of_residences
  mutate(cfr = deaths / suspected)

# dataset for CFR points only (only where CFR > 0)
cfr_points <- case_data_processed %>%
  filter(!is.na(cfr), cfr > 0)

# --- 2) Scaling (set your sec axis max range; example 10%) ---
# Floor of 1 so an empty case_data_processed yields 1 instead of -Inf,
# which would otherwise break scaling_factor and the y-axis limits below.
max_cases_count <- max(case_data_processed$suspected, 1, na.rm = TRUE)
sec_axis_max <- max(0.10, case_data_processed$cfr, na.rm = TRUE)
cases_top <- max(pretty(c(0, max_cases_count), n = 4))  # nice top for LEFT axis
cfr_top   <- max(pretty(c(0, sec_axis_max),   n = 4))   # nice top for RIGHT axis
scaling_factor <- cases_top / cfr_top

case_data_scaled <- case_data_processed %>%
  mutate(cfr_scaled = cfr * scaling_factor)

cfr_points_scaled <- cfr_points %>%
  mutate(cfr_scaled = cfr * scaling_factor)

# Secondary axis breaks (0–10% by 2%)
cfr_decimal_breaks <- pretty(c(0, cfr_top), n = 4)

# Primary-axis (cases) breaks: nice round numbers up to just above the tallest
# bar, so the top label (e.g. 150) is shown and the top of the graph is set to
# that break instead of being clamped to the exact data maximum.
cases_breaks <- pretty(c(0, max_cases_count), n = 4)

# --- 3) National CFR (computed from all reporting state_of_residences) ---
national_summary <- case_data_processed %>%
  summarise(
    total_suspected = sum(suspected, na.rm = TRUE),
    total_deaths    = sum(deaths,    na.rm = TRUE)
  ) %>%
  mutate(national_cfr = if_else(total_suspected > 0, total_deaths / total_suspected, NA_real_))

nat_cfr_scaled <- national_summary$national_cfr * scaling_factor

target_cfr <- 0.01
target_cfr_scaled <- target_cfr * scaling_factor

# --- 4) Plot: bars for all reporting state_of_residences; CFR dots only for CFR>0 state_of_residences ---
combo_chart <- ggplot(case_data_scaled, aes(x = reorder(state_of_residence, -suspected))) +
  
  # Bars (all suspected > 0)
  geom_col(aes(y = suspected, fill = "Cases"), alpha = 0.85) +
  
  # CFR points (ONLY CFR > 0)
  geom_point(
    data = cfr_points_scaled,
    aes(y = cfr_scaled, color = "CFR"),
    size = 2
  ) +
  
  # Reference lines (optional)
  geom_hline(aes(yintercept = nat_cfr_scaled, linetype = "National CFR"),
             color = "yellow", linewidth = 1.2, na.rm = TRUE) +
  geom_hline(aes(yintercept = target_cfr_scaled, linetype = "Target CFR"),
             color = "black", linewidth = 1, na.rm = TRUE) +
  
  # Axes
  scale_y_continuous(
    name = "Number of Suspected Cases",
    breaks = cases_breaks,
    limits = c(0, max(cases_breaks)),
    expand = c(0, 0),
    sec.axis = sec_axis(
      trans  = ~ . / scaling_factor,
      breaks = cfr_decimal_breaks,
      labels = percent_format(accuracy = 1),
      name   = "Case Fatality Rate (CFR)"
    )
  ) +
  
  # Unified legend
  scale_fill_manual(name = "", values = c("Cases" = "#138750")) +
  scale_color_manual(name = "", values = c("CFR" = "#d62728")) +
  scale_linetype_manual(name = "", values = c("National CFR" = "solid", "Target CFR" = "dashed")) +
  guides(
    fill     = guide_legend(order = 1, override.aes = list(alpha = 1)),
    color    = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  
  labs(x = "state_of_residence") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 9),
    axis.title.y.left  = element_text(color = "#138750", face = "bold"),
    axis.title.y.right = element_text(color = "#d62728", face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(color = alpha("gray60", 0.6), fill = NA)
  )

combo_chart
