sp <- merged_diph %>% 
  filter(data_year == "2026") %>% 
  mutate(months_d = month(dateofonset_day_mnth_yr)) %>% 
  filter(months_d %in% c(9, 8)) %>% 
  group_by(state_of_residence, lga_of_residence,) %>% 
  summarise(suspected_case = n(),
            Death = sum(outcome == "Dead")) %>% 
  qflextable()

sp

save_as_docx(sp, path = here("report", "table.docx"))


sp2 <- merged_diph %>% 
  filter(data_year == "2026") %>% 
  mutate(months_d = month(dateofonset_day_mnth_yr)) %>% 
  filter(months_d %in% c(9, 8)) %>% 
  group_by(state_of_residence, lga_of_residence,) %>% 
  summarise(confirmed = sum(final_classification %in% c("1_Lab confirmed", 
                                                        "3_Clin compatible",
                                                        "2_Epid linked")),
            date_last = max(date_seen_at_hf_day_mnth_yr, na.rm = T)) %>% 
  qflextable()
sp2

save_as_docx(sp2, path = here("report", "table2.docx"))


sp3 <- merged_diph %>%filter(data_year == "2026") %>% 
  mutate(months_d = month(dateofonset_day_mnth_yr)) %>% 
  filter(months_d %in% c(9, 8)) %>%  
  group_by(age_yrs_grp) %>% 
  summarise(suspected = n(),
           confirmed = sum(final_classification %in% c("1_Lab confirmed", 
                                                         "3_Clin compatible",
                                                         "2_Epid linked")),
             Death = sum(outcome == "Dead")) %>% 
  adorn_totals() %>% 
  qflextable()


sp3

save_as_docx(sp3, path = here("report", "table3.docx"))
