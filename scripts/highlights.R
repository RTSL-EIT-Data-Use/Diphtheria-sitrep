
# Highlight 1 -------------------------------------------------------------


# Summarise both weeks
wk_summary <- epi_summ %>%
  filter(
    (epi_year == report_year & epi_week == report_week) |
      (epi_year == prev_week_year   & epi_week == prev_week)
  ) %>%
  group_by(epi_year, epi_week) %>%
  summarise(
    cases  = sum(suspected, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Extract values safely
report_cases  <- wk_summary %>%
  filter(epi_year == report_year, epi_week == report_week) %>%
  pull(cases)

report_deaths <- wk_summary %>%
  filter(epi_year == report_year, epi_week == report_week) %>%
  pull(deaths)

prev_cases <- wk_summary %>%
  filter(epi_year == prev_week_year, epi_week == prev_week) %>%
  pull(cases)

# Replace missing with 0
report_cases  <- ifelse(length(report_cases)  == 0, 0, report_cases)
report_deaths <- ifelse(length(report_deaths) == 0, 0, report_deaths)
prev_cases    <- ifelse(length(prev_cases)    == 0, 0, prev_cases)

# CFR (%)
report_cfr <- ifelse(report_cases > 0,
                     (report_deaths / report_cases) * 100,
                     NA_real_)

# Percent change (%)
percent_change <- ifelse(prev_cases > 0,
                         ((report_cases - prev_cases) / prev_cases) * 100,
                         NA_real_)

# Determine direction
change_word <- ifelse(is.na(percent_change), "change (N/A)",
                      ifelse(percent_change > 0, "increase",
                             ifelse(percent_change < 0, "decrease",
                                    "no change")))

highlight1 <- glue(
  "In week {report_week}, there were {english(report_cases)} ({report_cases}) suspected cases and {english(report_deaths)} ({report_deaths}) death{ifelse(report_deaths == 1, '', 's')} with a case fatality rate of {ifelse(is.na(report_cfr), 'N/A', sprintf('%.1f%%', report_cfr))}. ",
  "In comparison to week {prev_week} ({prev_week_year}), there is an observed {ifelse(is.na(percent_change), 'N/A', sprintf('%.0f%%', abs(percent_change)))} {change_word} in cases reported."
)


# Highlight 2 -------------------------------------------------------------

cum_data <- epi_summ %>%
  filter(epi_year == report_year, epi_week <= report_week)

cum_cases  <- sum(cum_data$suspected, na.rm = TRUE)
cum_deaths <- sum(cum_data$deaths, na.rm = TRUE)

cum_cfr <- ifelse(cum_cases > 0,
                  (cum_deaths / cum_cases) * 100,
                  NA_real_)

cum_states <- cum_data %>%
  group_by(state_of_residence) %>%
  summarise(total = sum(suspected, na.rm = TRUE), .groups = "drop") %>%
  filter(total > 0) %>%
  nrow()

highlight2 <- glue(
  "As of epidemiological week {report_week}, {report_year}, a cumulative total of {comma(cum_cases)} suspected case{ifelse(cum_cases == 1,'', 's')} and {comma(cum_deaths)} death{ifelse(cum_deaths == 1, '', 's')} (CFR {ifelse(is.na(cum_cfr), 'N/A', sprintf('%.1f%%', cum_cfr))}) have been reported across {cum_states} state{ifelse(cum_states == 1, '', 's')}."
)


# Highlight 3 -------------------------------------------------------------

# sex and age_grp are already standardized in 2_data_cleaning.R — use them as-is.

# ---- Age group ranking (aggregate across sexes) ----
age_counts <- merged_diph %>%
  filter(epi_year == report_year, epi_week <= report_week) %>% 
  filter(!is.na(age_yrs_grp), age_yrs_grp != "", age_yrs_grp != "Unknown") %>%
  count(age_yrs_grp, name = "n") %>%
  arrange(desc(n))

top_age1 <- age_counts$age_yrs_grp[1]
# top_age2 <- ifelse(nrow(age_counts) >= 2, age_counts$age_grp[2], NA_character_)
top_age2 <- if (nrow(age_counts) >= 2) as.character(age_counts$age_yrs_grp[2]) else NA_character_

# ---- Sex distribution ----
sex_counts <- merged_diph %>%
  filter(epi_year == report_year, epi_week <= report_week) %>%
  filter(!is.na(sex_m_f)) %>%
  count(sex_m_f, name = "n") %>%
  mutate(pct = 100 * n / sum(n))

male_pct   <- sex_counts %>% filter(sex_m_f == "Male")   %>% pull(pct)
female_pct <- sex_counts %>% filter(sex_m_f == "Female") %>% pull(pct)

male_pct   <- ifelse(length(male_pct) == 0, NA_real_, male_pct)
female_pct <- ifelse(length(female_pct) == 0, NA_real_, female_pct)

# Rounded percentages (whole numbers like 49/51)
male_pct_r   <- ifelse(is.na(male_pct), NA, round(male_pct))
female_pct_r <- ifelse(is.na(female_pct), NA, round(female_pct))

group_noun <- ifelse(top_age1 %in% c("<5", "5-14"), "children", "individuals")
highlight3 <- glue::glue("Since the beginning of the year, {group_noun} aged {top_age1} account for the highest proportion of suspected cases{ifelse(is.na(top_age2),'.', glue::glue(', followed by those aged {top_age2}.'))} across both males and females.")

# Highlight 4 -------------------------------------------------------------

highlight4 <- glue::glue("Of all suspected cases, {ifelse(is.na(male_pct_r), 'N/A',
          paste0(male_pct_r, '%'))} are male and {ifelse(is.na(female_pct_r), 'N/A', 
          paste0(female_pct_r, '%'))} are female.")


# Highlight 5 -------------------------------------------------------------

state_totals <- merged_diph %>%
  filter(epi_year == report_year, epi_week <= report_week) %>%
  filter(!is.na(state_of_residence), state_of_residence != "") %>%
  count(state_of_residence, name = "total_cases") %>%
  arrange(desc(total_cases))

national_total_states <- sum(state_totals$total_cases)
reporting_states <- nrow(state_totals)

top5_states <- state_totals %>%
  slice_head(n = 5) %>%
  mutate(pct = round(100 * total_cases / national_total_states))

highlight5 <-glue::glue(
  "{top5_states$state[1]} ({comma(top5_states$total_cases[1])}) accounts for {top5_states$pct[1]}% of all suspected cases in the country, among the {reporting_states} state{ifelse(reporting_states==1,'','s')} that have reported Diphtheria cases. "
)

highlight5 <- if (reporting_states == 0) {
  glue::glue("No states have reported suspected Diphtheria cases as of epidemiological week {report_week}, {report_year}.")
} else {
  glue::glue(
    "{top5_states$state[1]} ({comma(top5_states$total_cases[1])}) accounts for ",
    "{top5_states$pct[1]}% of all suspected cases in the country, among the ",
    "{reporting_states} state{ifelse(reporting_states==1,'','s')} that have reported Diphtheria cases."
  )
}
# Highlight 6 -------------------------------------------------------------

lgas_totals <- merged_diph %>%
  filter(epi_year == report_year, epi_week <= report_week) %>%
  filter(!is.na(lga_of_residence), lga_of_residence != "") %>%
  count(lga_of_residence, name = "tot_cases") %>%
  arrange(desc(tot_cases))

national_total_lga <- sum(lgas_totals$tot_cases)
reporting_lgas <- nrow(lgas_totals)

top5_lgas <- lgas_totals %>%
  slice_head(n = 5) %>%
  mutate(pct = round(100 * tot_cases / national_total_lga))

highlight6 <- if (reporting_lgas == 0) {
  glue::glue("No LGAs have reported suspected Diphtheria cases as of epidemiological week {report_week}, {report_year}.")
} else {
  glue::glue(
    "{top5_lgas$lga[1]} ({comma(top5_lgas$tot_cases[1])}) accounts for ",
    "{top5_lgas$pct[1]}% of all suspected cases in the country, among the ",
    "{reporting_lgas} LGA{ifelse(reporting_lgas==1,'','s')} that have reported Diphtheria cases."
  )
}

# Highlight 7 -------------------------------------------------------------

# Current year cumulative (up to report_week)
current_cum <- epi_summ %>%
  filter(epi_year == report_year,
         epi_week <= report_week) %>%
  summarise(
    suspected = sum(suspected, na.rm = TRUE),
    deaths    = sum(deaths, na.rm = TRUE)
  )

# Comparison year cumulative (same epi-week cutoff)
compare_cum <- epi_summ %>%
  filter(epi_year == prev_year &
         epi_week <= report_week) %>%
  summarise(
    suspected = sum(suspected, na.rm = TRUE),
    deaths    = sum(deaths, na.rm = TRUE)
  )

curr_cases  <- current_cum$suspected
curr_deaths <- current_cum$deaths

prev_cases  <- compare_cum$suspected
prev_deaths <- compare_cum$deaths

# % change (safe)
case_pct_change <- ifelse(prev_cases > 0,
                          100 * (curr_cases - prev_cases) / prev_cases,
                          NA_real_)

death_pct_change <- ifelse(prev_deaths > 0,
                           100 * (curr_deaths - prev_deaths) / prev_deaths,
                           NA_real_)

case_direction  <- ifelse(is.na(case_pct_change), "change (N/A)",
                          ifelse(case_pct_change > 0, "increased",
                                 ifelse(case_pct_change < 0, "decreased",
                                        "remained unchanged")))

death_direction <- ifelse(is.na(death_pct_change), "change (N/A)",
                          ifelse(death_pct_change > 0, "increased",
                                 ifelse(death_pct_change < 0, "decreased",
                                        "remained unchanged")))


highlight7 <- glue::glue(
  "Comparatively, suspected cases of Diphtheria in {report_year} have {case_direction} by ",
  ifelse(is.na(case_pct_change), "N/A", sprintf('%.0f%%', abs(case_pct_change))),
  " compared to what was reported as at epidemiological week {report_week} in {prev_year}. ",
  "Similarly, cumulative deaths recorded have {death_direction} by ",
  ifelse(is.na(death_pct_change), "N/A", sprintf('%.0f%%', abs(death_pct_change))),
  " in {report_year}."
)

