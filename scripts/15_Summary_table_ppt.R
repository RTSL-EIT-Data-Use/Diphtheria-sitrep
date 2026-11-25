## ---------------------------------------------------------------
## Define reporting period objects
## ---------------------------------------------------------------
report_year <- 2026
# report_week <- <your current epi week>   # e.g., 37
  
  ## Roll back to previous week, or fall back to last epi week of prior year
  prev_week <- if (report_week > 1) {
    report_week - 1
  } else {
    max(epi_summ$epi_week[epi_summ$epi_year == report_year - 1], na.rm = TRUE)
  }

## ---------------------------------------------------------------
## Filter dataset for the CURRENT week
## ---------------------------------------------------------------
current_week_state <- merged_diph %>% 
  filter(epi_year == report_year & epi_week == report_week) %>%  
  summarise(
    state_of_residences = paste("States:", n_distinct(state_of_residence),
                                "\nLGAs:", n_distinct(lga_of_residence)),
    state_list          = paste(sort(unique(state_of_residence)), collapse = ", ")
  ) %>% 
  mutate(period = "Current week")

current_week <- epi_summ %>% 
  filter(epi_year == report_year & epi_week == report_week) %>%  
  summarise(cases_sus  = sum(suspected, na.rm = TRUE),
            cases_con  = sum(confirmed, na.rm = TRUE),
            deaths_sus = sum(deaths,    na.rm = TRUE)) %>% 
  mutate(cfr = ifelse(cases_con > 0,
                      percent(deaths_sus / cases_con, accuracy = 0.1),
                      "N/A"),
         period = "Current week") %>% 
  left_join(current_week_state, by = "period")

## ---------------------------------------------------------------
## Filter dataset for the PREVIOUS week
## (handles the week 1 → prior year rollback automatically)
## ---------------------------------------------------------------
prev_week_year <- if (report_week > 1) report_year else report_year - 1

previous_week_state <- merged_diph %>% 
  filter(epi_year == prev_week_year & epi_week == prev_week) %>%  
  summarise(
    state_of_residences = paste("States:", n_distinct(state_of_residence),
                                "\nLGAs:", n_distinct(lga_of_residence)),
    state_list          = paste(sort(unique(state_of_residence)), collapse = ", ")
  ) %>% 
  mutate(period = "Previous week")

previous_week <- epi_summ %>% 
  filter(epi_year == prev_week_year & epi_week == prev_week) %>% 
  summarise(cases_sus  = sum(suspected, na.rm = TRUE),
            cases_con  = sum(confirmed, na.rm = TRUE),
            deaths_sus = sum(deaths,    na.rm = TRUE)) %>% 
  mutate(cfr = ifelse(cases_con > 0,
                      percent(deaths_sus / cases_con, accuracy = 0.1),
                      "N/A"),
         period = "Previous week") %>% 
  left_join(previous_week_state, by = "period")

## ---------------------------------------------------------------
## Combine into a single summary table
## ---------------------------------------------------------------
Summary_table <- rbind(current_week, previous_week) %>% 
  select(period, cases_sus, cases_con, deaths_sus, cfr,
         state_of_residences, state_list)

Summary_table

## ---------------------------------------------------------------
## Build flextable
## ---------------------------------------------------------------
# Custom widths for the 7 columns
col_widths <- c(0.5, 0.4, 0.4, 0.5, 0.5, 0.6, 1.5)

Summary_table_ppt <- flextable(Summary_table) %>%
  theme_vanilla() %>%
  
  # Explicit widths so headers wrap correctly
  flextable::width(width = col_widths) %>%
  
  # Header labels with wrapped lines
  set_header_labels(
    period              = "Reporting\nperiod",
    cases_sus           = "Suspected\ncases\u00b9",
    cases_con           = "Confirmed\ncases\u00b9",
    deaths_sus          = "Deaths\n(confirmed)",
    cfr                 = "Case Fatality\nRatio (%)",
    state_of_residences = "States & LGAs\naffected",
    state_list          = "States affected"
  ) %>%
  
  # Header styling
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#138750") %>%
  bold(part = "header") %>%
  flextable::align(align = "center", part = "header") %>%
  valign(part = "header", valign = "center") %>%
  
  # Body styling
  flextable::align(align = "center", part = "body") %>%
  flextable::align(j = "state_list", align = "left", part = "body") %>%
  
  # Format numbers
  colformat_num(
    j = c("cases_sus", "cases_con", "deaths_sus"),
    big.mark = "",
    digits = 0
  ) %>%
  
  # Font sizes — smaller font for the long state list
  fontsize(size = 11, part = "all") %>%
  fontsize(j = "state_list", size = 9, part = "body") %>%
  
  autofit() %>%
  
  # Borders
  border_remove() %>% 
  hline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
  vline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
  border_outer(part = "all", border = fp_border(color = "black", width = 1))

Summary_table_ppt
