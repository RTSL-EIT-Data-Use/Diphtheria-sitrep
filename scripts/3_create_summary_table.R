## Filter dataset for the current (latest) week
current_week_state <- merged_diph %>% 
  filter(epi_year==report_year & epi_week==report_week) %>%  
  
  summarise(state_of_residences = paste("States:", n_distinct(state_of_residence),
                           "\nLGAs:",n_distinct(lga_of_residence))) %>% 
  mutate(period = "Current week")

current_week <- epi_summ %>% 
  filter(epi_year==report_year & epi_week==report_week) %>%  
  summarise(cases_sus = sum(suspected, na.rm=T),
            cases_con = sum(confirmed, na.rm=T),
            # cases_prob = sum(probable_cases, na.rm=T),
            deaths_sus = sum(deaths, na.rm = T)) %>% 
  
  mutate(cfr = ifelse(cases_sus > 0, percent(deaths_sus / cases_sus, accuracy = 0.1), "N/A"),
         period = "Current week") %>% 
  left_join(current_week_state)

## Filter dataset for the current (latest) year-week
current_year_state <- merged_diph %>% 
  filter(epi_year==report_year & epi_week<=report_week) %>%  
  
  summarise(state_of_residences = paste("States:", n_distinct(state_of_residence),
                           "\nLGAs:",n_distinct(lga_of_residence))) %>% 
  mutate(period = paste(report_year, "Cumulative"))


cumm_2026 <- epi_summ %>% 
  filter(epi_year==report_year & epi_week<=report_week) %>% 
  summarise(cases_sus = sum(suspected, na.rm=T),
            cases_con = sum(confirmed, na.rm=T),
            # cases_prob = sum(probable_cases, na.rm=T),
            deaths_sus = sum(deaths, na.rm = T)) %>% 
  mutate(cfr = ifelse(cases_sus > 0, percent(deaths_sus / cases_sus, accuracy = 0.1), "N/A"),
         period = paste(report_year, "Cumulative"))%>% 
  left_join(current_year_state)

## Filter dataset for the 2025 year-week
previous_year_state <- merged_diph %>% 
  filter(epi_year==report_year-1 & epi_week<= report_week) %>%  
  
  summarise(state_of_residences = paste("States:", n_distinct(state_of_residence),
                           "\nLGAs:",n_distinct(lga_of_residence))) %>% 
  mutate(period = paste(report_year - 1, "Cumulative"))

cumm_2025 <- epi_summ %>% 
  filter(epi_year==report_year-1 & epi_week<= report_week) %>% 
  summarise(cases_sus = sum(suspected, na.rm=T),
            cases_con = sum(confirmed, na.rm=T),
            # cases_prob = sum(probable_cases, na.rm=T),
            deaths_sus = sum(deaths, na.rm = T)) %>% 
  mutate(cfr = ifelse(cases_con > 0, percent(deaths_sus / cases_con, accuracy = 0.1), "N/A"),
         period = paste(report_year - 1, "Cumulative"))%>% 
  left_join(previous_year_state)




Summary_table <- rbind(current_week,cumm_2026,cumm_2025) %>% 
  select(period,cases_sus,cases_con,deaths_sus,cfr,state_of_residences) 

Summary_table


# Build flextable
# Define custom widths for the 7 columns (totaling ~6.0 inches)
col_widths <- c(0.5, 0.4, 0.4, 0.5, 0.5, 0.5)

# Build flextable
Summary_table_ft <- flextable(Summary_table) %>%
  theme_vanilla() %>%
  
  # Set explicit widths so headers wrap correctly
  flextable::width(width = col_widths) %>%
  
  # Header labels with wrapped lines
  set_header_labels(
    period       = "Reporting\nperiod",
    cases_sus    = "Suspected\ncases\u00b9", #\u00b9 = superscript "1" text
    cases_con    = "Confirmed\ncases\u00b9",
    # cases_prob   = "Probable\ncases",
    deaths_sus   = "Deaths\n(confirmed)",
    cfr          = "Case Fatality\nRatio (%)",
    state_of_residences       = "States & LGAs\naffected"
  ) %>%
  
  # Header styling
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#138750") %>%
  bold(part = "header") %>%
  flextable::align(align = "center", part = "header") %>%
  valign(part = "header", valign = "center") %>%
  
  # Body styling
  flextable::align(align = "center", part = "body") %>%
  
  # Format numbers
  colformat_num(
    j = c("cases_sus", "cases_con", "deaths_sus"),
    big.mark = "",
    digits = 0
  ) %>%
  
  # 🔹 Set output font size to 12
  fontsize(size = 11, part = "all") %>%
  
  # Fit content (but keep fixed width)
  autofit() %>%
  
  # Borders
  border_remove() %>% 
  hline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
  vline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
  border_outer(part = "all", border = fp_border(color = "black", width = 1))

Summary_table_ft

