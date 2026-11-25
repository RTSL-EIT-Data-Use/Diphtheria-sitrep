library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(patchwork)   # install.packages("patchwork") if needed

# --- Reusable function to build the colored status table ---
make_vacc_legend_table <- function(data) {
  
  pct_data <- data %>%
    group_by(vaccination_status2) %>%
    summarise(n = sum(confirmed, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    mutate(
      label = case_when(
        vaccination_status2 == "not vacc"   ~ "Unvaccinated",
        vaccination_status2 == "partly vacc" ~ "Partially vaccinated",
        vaccination_status2 == "fully vacc" ~ "Fully vaccinated",
        vaccination_status2 == "unknown"    ~ "Unknown",
        TRUE ~ vaccination_status2
      ),
      fill_color = case_when(
        vaccination_status2 == "not vacc"    ~ "#FF0000",
        vaccination_status2 == "partly vacc" ~ "#FFFF00",
        vaccination_status2 == "fully vacc"  ~ "#138750",
        vaccination_status2 == "unknown"     ~ "#A6A6A6",
        TRUE ~ "grey90"
      ),
      # order rows to match the image: Unvaccinated, Partially, Fully, Unknown
      row_order = case_when(
        vaccination_status2 == "not vacc"    ~ 4,
        vaccination_status2 == "partly vacc" ~ 3,
        vaccination_status2 == "fully vacc"  ~ 2,
        vaccination_status2 == "unknown"     ~ 1,
        TRUE ~ 0
      )
    )
  
  ggplot(pct_data, aes(y = row_order)) +
    geom_tile(aes(x = 1, fill = fill_color), width = 2, height = 0.9, color = "white") +
    geom_text(aes(x = 1, label = label), hjust = 0, nudge_x = -0.95,
              fontface = "bold", color = "white", size = 3.6) +
    geom_tile(aes(x = 2.6, fill = fill_color), width = 1, height = 0.9, color = "white") +
    geom_text(aes(x = 2.6, label = paste0(pct, "%")), fontface = "bold", size = 3.6,
              color = ifelse(pct_data$fill_color == "#FFFF00", "black", "white")) +
    scale_fill_identity() +
    xlim(0, 3.3) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 5, 0))
}

# --- 2026 data prep ---
epi_summ1 <- merged_diph %>%
  filter(data_year == 2026) %>% 
  mutate(vaccination_status = str_to_lower(vaccination_status)) %>%
  mutate(vaccination_status = replace_na(vaccination_status, "unknown")) %>%
  mutate(vaccination_status2 = recode(vaccination_status,
                                      "yes" = "fully vacc",
                                      "vaccinated" = "fully vacc",
                                      "unvaccinated" = "not vacc",
                                      "fully vaccinated" = "fully vacc",
                                      "no" = "not vacc",
                                      .default = vaccination_status)) %>%
  group_by(epi_year, epi_week, state_of_residence, result_fc,
           vaccination_status, vaccination_status2, no_of_doses, age_yrs_grp) %>%
  summarise(
    suspected = n(),
    confirmed = sum(result_fc == "Positive", na.rm = TRUE),
    deaths = sum(outcome == "Dead" & result_fc == "Positive", na.rm = TRUE),
    number_lga_of_residence = n_distinct(lga_of_residence),
    .groups = "drop"
  )

vacc_sta_2026_bar <- epi_summ1 %>% 
  ggplot(aes(x = age_yrs_grp, y = confirmed, fill = vaccination_status2)) +
  geom_col() +
  labs(
    x = "Age group",
    y = "Number of Confirmed Cases",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("not vacc" = "#FF0000","partly vacc" = "#FFFF00", 
               "fully vacc" = "#138750", "unknown" = "#A6A6A6"),
    labels = c("not vacc" = "Unvaccinated", "partly vacc" = "Partially vaccinated",
               "fully vacc" = "Fully vaccinated", "unknown" = "Unknown")
  )+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

# --- Combine table legend + bar chart, stacked vertically ---
legend_table_2026 <- make_vacc_legend_table(epi_summ1)

vacc_sta_2026 <- legend_table_2026 / vacc_sta_2026_bar + 
  plot_layout(heights = c(1, 4))

vacc_sta_2026


# --- Cumulative data prep ---
epi_summ2 <- merged_diph %>%
  mutate(vaccination_status = str_to_lower(vaccination_status)) %>%
  mutate(vaccination_status = replace_na(vaccination_status, "unknown")) %>%
  mutate(vaccination_status2 = recode(vaccination_status,
                                      "yes" = "fully vacc",
                                      "vaccinated" = "fully vacc",
                                      "unvaccinated" = "not vacc",
                                      "fully vaccinated" = "fully vacc",
                                      "no" = "not vacc",
                                      .default = vaccination_status)) %>%
  group_by(epi_year, epi_week, state_of_residence, result_fc,
           vaccination_status, vaccination_status2, no_of_doses, age_yrs_grp) %>%
  summarise(
    suspected = n(),
    confirmed = sum(result_fc == "Positive", na.rm = TRUE),
    deaths = sum(outcome == "Dead" & result_fc == "Positive", na.rm = TRUE),
    number_lga_of_residence = n_distinct(lga_of_residence),
    .groups = "drop"
  )

vacc_sta_cum_bar <- epi_summ2 %>% 
  ggplot(aes(x = age_yrs_grp, y = confirmed, fill = vaccination_status2)) +
  geom_col() +
  labs(
    x = "Age group",
    y = "Number of Confirmed Cases",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("not vacc" = "#FF0000","partly vacc" = "#FFFF00", 
               "fully vacc" = "#138750", "unknown" = "#A6A6A6"),
    labels = c("not vacc" = "Unvaccinated", "partly vacc" = "Partially vaccinated",
               "fully vacc" = "Fully vaccinated", "unknown" = "Unknown")
  )+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

legend_table_cum <- make_vacc_legend_table(epi_summ2)

vacc_sta_cum <- legend_table_cum / vacc_sta_cum_bar + 
  plot_layout(heights = c(1, 4))

vacc_sta_cum