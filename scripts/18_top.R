

diph_state <- merged_diph %>% 
  filter(data_year == 2026) %>% 
  group_by(state_of_residence) %>% 
  summarise(
    suspect   = n(),
    confirmed = sum(result_fc == "Positive", na.rm = TRUE),
    .groups   = "drop"
  ) %>% 
  arrange(desc(suspect)) %>% 
  mutate(
    SN        = row_number(),
    total     = sum(suspect),                            # grand total
    pct_num   = confirmed / total,                       # numeric proportion
    pct_con   = percent(pct_num, accuracy = 0.1),       # "55.46%"
    cum_pct   = percent(cumsum(pct_num), accuracy = 0.1) # "55.5%"
  ) %>% 
  select(SN, 
         State = state_of_residence,
         Confirmed = confirmed, 
         "% Confirmed cases" = pct_con, 
         "Cum.%" = cum_pct)

diph_state_table <- diph_state %>% 
  head(n = 8) %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#138750") %>%
  bold(part = "header") %>%
  flextable::align(align = "center", part = "header") %>%
  valign(part = "header", valign = "center") %>% 
  # Body styling
  flextable::align(align = "center", part = "body") %>%
  flextable::align(j = "Cum.%", align = "left", part = "body") %>% 
  autofit() %>%
  
  # Borders
  border_remove() %>% 
  hline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
  vline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
  border_outer(part = "all", border = fp_border(color = "black", width = 1))


diph_state_table




diph_lga <- merged_diph %>% 
  mutate(lga_of_residence = str_to_sentence(lga_of_residence)) %>% 
  mutate(lga_of_residence = recode(lga_of_residence, 
                                   "Tofa" = "Dawakin tofa",
                                   "D/tofa" = "Dawakin tofa",
                                   "Ungoggo" = "Ungogo",
                                   "Kmc" = "Kano municipal",
                                   "Garum mallam" = "Garun mallam")) %>% 
  filter(data_year == 2026) %>% 
  group_by(lga_of_residence, state_of_residence) %>% 
  summarise(suspect = n(),
            confirmed = sum(result_fc == "Positive"),
            .groups = "drop") %>%    # <- explicitly drop ALL grouping
  arrange(desc(confirmed)) %>%       # <- order rows before cumsum (Pareto-style)
  mutate(
    total   = sum(confirmed),                          # grand total across ALL LGAs now
    pct_num = confirmed / total,                        # numeric proportion
    pct_con = percent(pct_num, accuracy = 0.1),
    cum_pct = percent(cumsum(pct_num), accuracy = 0.1)  # now accumulates correctly, row by row
  ) %>% 
  select( 
         lga_of_residence,
         state_of_residence,
         confirmed, 
         pct_con, 
         cum_pct) %>% 
  arrange(desc(confirmed))

diph_lga$SN <- 1:nrow(diph_lga)
  
  diph_lga_table <- diph_lga %>% 
    select(
      SN,
      LGA          = lga_of_residence,
      State        = state_of_residence,
      Confirmed    = confirmed,
      `% Confirmed case`          = pct_con,
      `Cum. %`     = cum_pct
    ) %>% 
  head(n = 10) %>% 
    flextable() %>% 
    theme_vanilla() %>% 
    color(part = "header", color = "white") %>%
    bg(part = "header", bg = "#138750") %>%
    bold(part = "header") %>%
    flextable::align(align = "center", part = "header") %>%
    valign(part = "header", valign = "center") %>% 
    # Body styling
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = "Cum.%", align = "left", part = "body") %>% 
    autofit() %>%
    
    # Borders
    border_remove() %>% 
    hline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
    vline(part = "all", border = fp_border(color = "black", width = 1)) %>% 
    border_outer(part = "all", border = fp_border(color = "black", width = 1))

  
 diph_lga_table
 
