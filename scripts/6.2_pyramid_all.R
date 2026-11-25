diph_pyramid_data <- merged_diph %>%
  # filter(epi_year == report_year) %>%
  filter(!is.na(age_yrs) & !is.na(sex_m_f)) %>%
  filter(result_fc == "Positive") %>% 
  
  mutate(
    age_grp = case_when(
      age_yrs >= 0 & age_yrs < 1    ~ "<1",
      age_yrs >= 1 & age_yrs <= 4   ~ "1-4",
      age_yrs >= 5 & age_yrs <= 9   ~ "5-9",
      age_yrs >= 10 & age_yrs <= 14 ~ "10-14",
      age_yrs >= 15 & age_yrs <= 19 ~ "15-19",
      age_yrs >= 20 & age_yrs <= 29 ~ "20-29",
      age_yrs >= 30 & age_yrs <= 39 ~ "30-39",
      age_yrs >= 40                 ~ "40+",
      TRUE                          ~ "Unknown"
    ),
    age_grp = factor(
      age_grp,
      levels = c("<1", "1-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40+", "Unknown")
    )
  ) %>%
  
  group_by(age_grp, sex_m_f) %>%
  summarise(count = n(), .groups = 'drop') %>%
  
  mutate(
    count = ifelse(sex_m_f == "Male", -count, count)
  )

# --- Compute Female/Male percentages (AFTER diph_pyramid_data exists) ---
sex_pct <- diph_pyramid_data %>%
  group_by(sex_m_f) %>%
  summarise(n = sum(abs(count)), .groups = "drop") %>%
  mutate(pct = round(100 * n / sum(n), 1))

female_pct <- sex_pct$pct[sex_pct$sex_m_f == "Female"]
male_pct   <- sex_pct$pct[sex_pct$sex_m_f == "Male"]

# --- 2. Generate the Age-Sex Pyramid Plot ---
max_count <- max(abs(diph_pyramid_data$count), 1)
max_age_level <- max(as.numeric(factor(diph_pyramid_data$age_grp)), 1)   # fixed: age_grp not age_group

age_pyramid_all <- diph_pyramid_data %>%
  ggplot(aes(x = age_grp, y = count, fill = sex_m_f)) +
  geom_bar(stat = "identity",
           width = 1,
           color = "black") +
  
  annotate(
    "label",
    x = max_age_level + 1.5,
    y = -max_count * 0.9,
    label = paste0("Male: ", male_pct, "%"),
    fill = "#FFC82D",
    color = "black",
    fontface = "bold",
    size = 4,
    label.size = 0
  ) +
  annotate(
    "label",
    x = max_age_level + 1.5,
    y = max_count * 0.9,
    label = paste0("Female: ", female_pct, "%"),
    fill = "#A8D18D",
    color = "black",
    fontface = "bold",
    size = 4,
    label.size = 0
  ) +
  
  coord_flip(clip = "off") +   # fixed: allow labels above plot area to render
  scale_fill_manual(values = c("Female" = "#FFC82D", "Male" = "#138750")) +
  scale_y_continuous(
    labels = abs,
    breaks = scales::pretty_breaks(n = 6),
    limits = max_count * c(-1, 1)
  ) +
  labs(
    x = "Age Group",
    y = "# of confirmed Cases",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = scales::alpha("gray60", 0.6), fill = NA),
    plot.margin = margin(30, 10, 10, 10)   # fixed: extra top margin so labels aren't clipped
  )

age_pyramid_all
