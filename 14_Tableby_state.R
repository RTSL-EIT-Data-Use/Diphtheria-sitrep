
merged_diph_t <- merged_diph %>% 
  select(data_year, state_of_residence, final_classification,outcome) %>% 
  mutate(
    final_classification2 = case_when(
      final_classification %in% c("1_Lab confirmed", "3_Clin compatible", "2_Epid linked") ~ "Positive",
      TRUE ~ "Negative"  # Fixed spelling: "Negative" not "Negetive"
    )
  ) %>% 
  group_by(state_of_residence) %>% 
  summarise(
    case_sus = n(),
    case_con = sum(final_classification2 == "Positive", na.rm = TRUE),
    pct_case_con = percent (case_con / case_sus),
    dead_con = sum(outcome == "Dead" & final_classification2 == "Positive", na.rm = TRUE),
                   cfr_con = ifelse(dead_con == 0, "0%", percent(dead_con / case_con))
  ) %>% 
  arrange(desc(case_sus)) %>% 
  qflextable() %>% 
  theme_vanilla() %>% 
  set_header_labels(
    state_of_residence = "State",
    case_sus = "Suspected Cases",
    case_con = "Confirmed Cases",
    pct_case_con = "% Confirmed",
    dead_con = "Deaths",
    cfr_con = "CFR (%)") %>% 
  # Add grid lines (horizontal and vertical)
  border_outer(border = fp_border(color = "black", width = 1)) %>%
  border_inner_h(border = fp_border(color = "gray80", width = 0.5)) %>%
  border_inner_v(border = fp_border(color = "gray80", width = 0.5)) %>%
  # Color the header green
  bg(part = "header", bg = "#138750") %>%
  # Make header text white for better contrast
  color(part = "header", color = "white") %>%
  # Bold header text
  bold(part = "header") %>% 
  flextable::align(j = 1:6, align = "center", part = "all")
 

merged_diph_t





