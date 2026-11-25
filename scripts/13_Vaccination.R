library(dplyr)
library(stringr)
library(tidyr)

epi_summ1 <- merged_diph %>%
  filter(data_year == 2026) %>% 
  # Clean vaccination_status first (before grouping)
  mutate(vaccination_status = str_to_lower(vaccination_status)) %>%
  mutate(vaccination_status = replace_na(vaccination_status, "unknown")) %>%
  mutate(vaccination_status2 = recode(vaccination_status,
                                      "yes" = "fully vacc",
                                      "vaccinated" = "fully vacc",
                                      "unvaccinated" = "not vacc",
                                      "fully vaccinated" = "fully vacc",
                                      "no" = "not vacc",
                                      .default = vaccination_status)) %>%
  # Now group by the cleaned variables
  group_by(epi_year, epi_week, state_of_residence, result_fc,
           vaccination_status, vaccination_status2, no_of_doses,age_yrs_grp) %>%
  summarise(
    suspected = n(),
    confirmed = sum(result_fc == "Positive", na.rm = TRUE),
    deaths = sum(outcome == "Dead" & result_fc == "Positive", na.rm = TRUE),
    number_lga_of_residence = n_distinct(lga_of_residence),
    .groups = "drop"
  )

library(ggplot2)

vacc_sta_2026 <- epi_summ1 %>% 
  ggplot(aes(x = age_yrs_grp, y = confirmed, fill = vaccination_status2)) +
  geom_col() +
  labs(
    title = "Confirmed Cases by Vaccination Status",
    x = "Age group",
    y = "Number of Suspected Cases",
    fill = "Vaccination Status"
  ) +
  scale_fill_manual(
    values = c("not vacc" = "#FF0000","partly vacc" = "#FFFF00", 
               "fully vacc" = "#138750", "unknown" = "#A6A6A6"),
    name = "",
    labels = c("not vacc" = "Unvaccinated", "partly vacc" = "Patially vaccinated",
               "fully vacc" = "Fully vaccinated", "unknown" = "Unknown")
  )+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

vacc_sta_2026


epi_summ2 <- merged_diph %>%
  # Clean vaccination_status first (before grouping)
  mutate(vaccination_status = str_to_lower(vaccination_status)) %>%
  mutate(vaccination_status = replace_na(vaccination_status, "unknown")) %>%
  mutate(vaccination_status2 = recode(vaccination_status,
                                      "yes" = "fully vacc",
                                      "vaccinated" = "fully vacc",
                                      "unvaccinated" = "not vacc",
                                      "fully vaccinated" = "fully vacc",
                                      "no" = "not vacc",
                                      .default = vaccination_status)) %>%
  # Now group by the cleaned variables
  group_by(epi_year, epi_week, state_of_residence, result_fc,
           vaccination_status, vaccination_status2, no_of_doses,age_yrs_grp) %>%
  summarise(
    suspected = n(),
    confirmed = sum(result_fc == "Positive", na.rm = TRUE),
    deaths = sum(outcome == "Dead" & result_fc == "Positive", na.rm = TRUE),
    number_lga_of_residence = n_distinct(lga_of_residence),
    .groups = "drop"
  )


vacc_sta_cum <- epi_summ2 %>% 
  ggplot(aes(x = age_yrs_grp, y = confirmed, fill = vaccination_status2)) +
  geom_col() +
  labs(
    title = "Confirmed Cases by Vaccination Status",
    x = "Age group",
    y = "Number of Suspected Cases",
    fill = "Vaccination Status"
  ) +
  scale_fill_manual(
    values = c("not vacc" = "#FF0000","partly vacc" = "#FFFF00", 
               "fully vacc" = "#138750", "unknown" = "#A6A6A6"),
    name = "",
    labels = c("not vacc" = "Unvaccinated", "partly vacc" = "Patially vaccinated",
               "fully vacc" = "Fully vaccinated", "unknown" = "Unknown")
  )+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

vacc_sta_cum
