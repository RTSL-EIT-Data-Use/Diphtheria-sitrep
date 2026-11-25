library(dplyr)
library(scales)
library(flextable)
library(officer)

# --- Reusable function: build one cumulative summary row for a given year ---
build_cumulative_row <- function(yr, week_limit = NULL) {
  
  if (!is.null(week_limit)) {
    df_state <- merged_diph %>% filter(epi_year == yr & epi_week <= week_limit)
    df_summ  <- epi_summ    %>% filter(epi_year == yr & epi_week <= week_limit)
    period_label <- paste0(yr, " Cumulative\n(Week 1-", week_limit, ")")
  } else {
    df_state <- merged_diph %>% filter(epi_year == yr)
    df_summ  <- epi_summ    %>% filter(epi_year == yr)
    period_label <- paste0(yr, " Cumulative")
  }
  
  state_info <- df_state %>%
    summarise(state_of_residences = paste("State(s):", n_distinct(state_of_residence),
                                          "\nLGA(s):", n_distinct(lga_of_residence)))
  
  df_summ %>%
    summarise(
      cases_sus  = sum(suspected, na.rm = TRUE),
      cases_con  = sum(confirmed, na.rm = TRUE),
      deaths_sus = sum(deaths, na.rm = TRUE)
    ) %>%
    mutate(
      cfr    = ifelse(cases_sus > 0, percent(deaths_sus / cases_sus, accuracy = 0.1), "N/A"),
      period = period_label
    ) %>%
    bind_cols(state_info)
}

# --- Build one row per year (2026 down to 2022), matching the image ---
cumm_2026 <- build_cumulative_row(2026, report_week)
cumm_2025 <- build_cumulative_row(2025, report_week)
cumm_2024 <- build_cumulative_row(2024, report_week)
cumm_2023 <- build_cumulative_row(2023, report_week)
cumm_2022 <- build_cumulative_row(2022, NULL)   # 2022: full-year total, no week cap (per image)

# --- Combine into one summary table ---
Summary_table <- bind_rows(cumm_2026, cumm_2025, cumm_2024, cumm_2023, cumm_2022) %>%
  select(period, cases_sus, cases_con, deaths_sus, cfr, state_of_residences)

Summary_table


# --- Build flextable ---
col_widths <- c(1.1, 0.9, 0.9, 1.0, 1.0, 1.3)   # widened to fit "Cumulative (Week 1-31)" labels

Summary_table_comp <- flextable(Summary_table) %>%
  theme_vanilla() %>%
  
  flextable::width(width = col_widths) %>%
  
  set_header_labels(
    period               = "Reporting period",
    cases_sus            = "Suspected cases",
    cases_con            = "Confirmed cases",
    deaths_sus           = "Deaths\n(Confirmed cases)",
    cfr                  = "Case Fatality Ratio\n(CFR)",
    state_of_residences  = "States and LGAs Affected\n(Confirmed cases)"
  ) %>%
  
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#138750") %>%
  bold(part = "header") %>%
  flextable::align(align = "center", part = "header") %>%
  valign(part = "header", valign = "center") %>%
  
  flextable::align(align = "center", part = "body", j = c("cases_sus", "cases_con", "deaths_sus", "cfr")) %>%
  flextable::align(align = "left", part = "body", j = c("period", "state_of_residences")) %>%
  bold(j = "period", part = "body") %>%
  
  colformat_num(
    j = c("cases_sus", "cases_con", "deaths_sus"),
    big.mark = ",",
    digits = 0
  ) %>%
  
  fontsize(size = 12, part = "all") %>%
  
  autofit() %>%
  
  border_remove() %>% 
  hline(part = "all", border = fp_border(color = "gray70", width = 1)) %>% 
  vline(part = "all", border = fp_border(color = "gray70", width = 1)) %>% 
  border_outer(part = "all", border = fp_border(color = "black", width = 1.5))

Summary_table_comp