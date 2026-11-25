

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
    pct_con   = percent(pct_num, accuracy = 0.01),       # "55.46%"
    cum_pct   = percent(cumsum(pct_num), accuracy = 0.1) # "55.5%"
  ) %>% 
  select(SN, 
         state_of_residence,
         suspect,
         confirmed, 
         pct_con, 
         cum_pct)


diph_lga <- merged_diph %>% 
  mutate(lga_of_residence = str_to_sentence(lga_of_residence)) %>% 
  mutate(lga_of_residence = recode(lga_of_residence, "Tofa" = "Dawakin tofa",
                                   "D/tofa" = "Dawakin tofa",
                                   "Ungoggo" = "Ungogo",
                                   "Kmc" = "Kano municipal",
                                   "Garum mallam" = "Garun mallam")) %>% 
  filter(data_year == 2026) %>% 
  group_by(lga_of_residence,state_of_residence) %>% 
  summarise(suspect = n(),
            confirmed = sum(result_fc == "Positive")) %>% 
  arrange(desc(suspect))

 
 # Distribution of suspected cases by state

 diph_bar_sus <- diph_state %>% 
   ggplot(aes(x = reorder( state_of_residence, suspect
                           ), y = suspect)) +
   geom_col(fill = "#138750") +
   coord_flip() +
   labs(x = 'State',
        y = "# of Suspected cases") +
   geom_text(aes(label = suspect), hjust = -0.2) +
   theme_classic()

 diph_bar_sus
 
 # Distribution of confirmed cases by state
 
 diph_bar_con <-  diph_state %>% 
   filter(if_all(everything(), ~ . != 0)) %>% 
   ggplot(aes(x = reorder( state_of_residence, confirmed
   ), y = confirmed)) +
   geom_col(fill = "#138750") +
   coord_flip() +
   labs(x = 'State',
        y = "# of confirmed cases") +
   geom_text(aes(label = confirmed), hjust = -0.2) +
   theme_classic()

 
 diph_bar_con 
 