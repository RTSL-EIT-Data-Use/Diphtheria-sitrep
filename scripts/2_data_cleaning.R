# Data Cleaning --------------------------------------------------------

# Select variables and Merge all years --------------------------------------------------------
# Select the relevant variables used in the sitrep

# Keep only the variables used in the sitrep (same set for every year), then
# row-bind all years into one table. `.id = "data_year"` records which sheet
# each row came from — used by the data-quality checks and 00_Run the report.
merged_diph <- diph_by_year %>%
  lapply(function(df) {
    df %>%
      select(
        state_of_residence, lga_of_residence, sex_m_f,age_yrs, dateofonset_day_mnth_yr,
        date_seen_at_hf_day_mnth_yr, sample_collected_yes_no,
        final_classification, final_classification, outcome,vaccination_status,no_of_doses) %>% 
      mutate(
        dateofonset_day_mnth_yr = as.Date(dateofonset_day_mnth_yr),
        date_seen_at_hf_day_mnth_yr = if (is.character(date_seen_at_hf_day_mnth_yr))
          as.Date(as.numeric(date_seen_at_hf_day_mnth_yr), origin = "1899-12-30")  # text serials -> Date
          else as.Date(date_seen_at_hf_day_mnth_yr),
        age_yrs = as.numeric(age_yrs)
      )
  }) %>%
  dplyr::bind_rows(.id = "data_year")

## Trim/collapse stray whitespace in all text columns so exact matches
## (e.g. "Positive " vs "Positive") and state_of_residence-name grouping work reliably.
merged_diph <- merged_diph %>%
  mutate(across(where(is.character), stringr::str_squish))


merged_diph <- merged_diph %>%
  mutate(s_n = row_number()) %>% # add serial number
  select(s_n, everything())      #make serial number first variable 

## Create Epi week and Year
merged_diph <- merged_diph %>%
  mutate(
    onset_missing  = is.na(dateofonset_day_mnth_yr),
    hf_available   = !is.na(date_seen_at_hf_day_mnth_yr),
    used_hf_date   = onset_missing & hf_available,     # rescued: HF date used
    still_excluded = onset_missing & !hf_available,    # no usable date at all
    date_used = coalesce(dateofonset_day_mnth_yr, date_seen_at_hf_day_mnth_yr),  # drives epi week/year
    epi_week  = isoweek(date_used),
    epi_year  = isoyear(date_used)
  )

### Data-quality tallies (printed by 10_print_data_checks.R)
# 2a. Rows rescued by using date_seen_at_hf_day_mnth_yr instead of dateofonset_day_mnth_yr
n_hf_used_by_year <- merged_diph %>%
  filter(used_hf_date) %>%
  count(data_year, name = "n_hf_used")
n_hf_used <- sum(n_hf_used_by_year$n_hf_used)

# 2b. Rows still excluded: neither a valid onset date nor a valid HF date
n_bad_date_by_year <- merged_diph %>%
  filter(still_excluded) %>%
  count(data_year, name = "n_bad_date")
n_bad_date <- sum(n_bad_date_by_year$n_bad_date)

# Data-quality check: records whose onset-date year doesn't match the sheet they came from.
# A row on the 2025 tab with a 2024 or 2026 onset date is almost always a date typo, and it
# would be silently dropped from that year's counts. Grouped so you know which tab to clean.
year_mismatch <- merged_diph %>%
  filter(!is.na(dateofonset_day_mnth_yr),
         as.numeric(data_year) != lubridate::year(dateofonset_day_mnth_yr)) %>%
  count(data_year, onset_year = lubridate::year(dateofonset_day_mnth_yr), name = "n")

# Data-quality check:age_yrs values that aren't a plausible whole number of years.
# The `age = as.numeric(age)` step below silently turns text like "18 months" or "unknown"
# into NA (which then falls intoage_yrs group "Unknown"), and a typo like "199" would land in
# "45+". We check the RAW value here, before that conversion, so the offending text stays
# visible. Blank cells are ignored (missing, not a typo). Grouped by year sheet.
implausible_age <- merged_diph %>%
  mutate(age_raw = as.character(age_yrs),
        age_yrs_num = suppressWarnings(as.numeric(age_raw))) %>%
  filter(!is.na(age_raw),age_raw != "",
         is.na(age_yrs_num) |age_yrs_num < 0 |age_yrs_num > 110) %>%
  count(data_year,age_raw, name = "n")




## Standardize variables
merged_diph <- merged_diph %>%
  mutate(
    state_of_residence = clean_state_names(state_of_residence),
    lga_of_residence = as.character(lga_of_residence),
    sex_m_f = as.character(sex_m_f),
    outcome = as.character(outcome),
    s_n = as.character(s_n),
   age_yrs = as.numeric(age_yrs),
    epi_week = as.numeric(epi_week),
    epi_year = as.numeric(epi_year)
  ) 

#Data-quality check: raw lab-result_fc values that aren't clearly positive/negative.
# Anything not listed here is currently treated as "Negative" (see result_fc block below),
# so typos like "postive" or "pos." silently undercount confirmed cases.
# Grouped by year sheet so you know which tab of cholera.xlsx to clean.

known_result_fcs <- c("5_Pending", "3_Clin compatible", "6_Unknown", "1_Lab confirmed",
                   "4_Discarded", "2_Epid linked", "")

unmatched_rdt <- merged_diph %>%
  filter(!is.na(final_classification),
         !str_to_lower(final_classification) %in% known_result_fcs) %>%
  count(data_year, final_classification, name = "n")

# unmatched_cul <- merged_diph %>%
#   filter(!is.na(final_classification),
#          !str_to_lower(final_classification) %in% known_result_fcs) %>%
#   count(data_year, final_classification, name = "n")


# result_fc variable 
merged_diph <- merged_diph %>%
  mutate(
    result_fc = case_when(
      str_to_lower(final_classification) %in% c("3_clin compatible",
                                                "1_lab confirmed", "2_epid linked" ) ~ "Positive",
      TRUE ~ "Negative"
    ),
    # result_fc_cul = case_when(
    #   str_to_lower(final_classification) %in% c("positive", "pos") ~ "Positive",
    #   TRUE ~ "Negative"
    # ),
    
    # Combined result_fc logic
    # result_fc = case_when(
    #   result_fc_rdt == "Positive" | result_fc_cul == "Positive" ~ "Positive",
    #   TRUE ~ "Negative"
    # ),
    # 
    # result_fc = factor(result_fc, levels = c("Negative", "Positive"))
  )


# sex_m_f variable
merged_diph <- merged_diph %>%
  mutate(
    sex_m_f = case_when(
      str_to_lower(sex_m_f) %in% c("f", "female") ~ "Female",
      str_to_lower(sex_m_f) %in% c("m", "male") ~ "Male",
      TRUE ~ NA_character_
    ), 
    sex_m_f = factor(sex_m_f, levels = c("Male", "Female")))

# Data-quality check: every raw outcome value that is NOT a clean "alive"/"dead"
# (case-insensitive), with how the pipeline actually treats it. Categorization is
# binary because the outputs only ever count deaths (outcome == "Dead"); every
# other value — including blanks/NA is included in the suspected total and shown as "Alive"
###Calculate here to give warning in console - used in 00_run the report data check
unmatched_outcome <- merged_diph %>%
  filter(!str_to_lower(outcome) %in% c("alive", "dead")) %>%
  mutate(
    categorization = case_when(
      str_detect(str_to_lower(outcome), "^dea|^die|died|buried") ~ "Dead",
      TRUE                                                ~ "Alive"   # NA/blank/other -> Alive
    )
  ) %>%
  count(outcome, data_year, categorization, name = "n") %>%
  select(outcome, data_year, n, categorization) %>%   # requested column order
  arrange(desc(n))

# Outcome variable alive or dead
merged_diph <- merged_diph %>%
  mutate(
    outcome = case_when(
      str_detect(str_to_lower(outcome), "^dea|^die|died|buried") ~ "Dead",
      TRUE ~ "Alive"   # anything not matched as a death is treated as alive
    )
  )    
    
 
# 10-Yearage_yrs Categories and Counts ---
merged_diph <- merged_diph %>%
  # Create 10-yearage_yrs categories
  mutate(
   age_yrs_grp = case_when(
     age_yrs >= 0 &age_yrs < 1    ~ "<1",
     age_yrs >= 1 &age_yrs <= 4 ~ "1-4",
     age_yrs >= 5 &age_yrs <= 9 ~ "5-9",
     age_yrs >= 10 &age_yrs <= 14 ~ "10-14",
     age_yrs >= 15 &age_yrs <= 19 ~ "15-19",
     age_yrs >= 20 &age_yrs <= 29 ~ "20-29",
     age_yrs >= 30 &age_yrs <= 39 ~ "30-39",
     age_yrs >= 40             ~ "40+",
      TRUE                  ~ "Unknown"
    ),
   age_yrs_grp = factor(
     age_yrs_grp,
      levels = c("<1", "1-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40+", "Unknown")
    )
  ) %>% 
  mutate(lga_of_residence = str_to_sentence(lga_of_residence)) %>% 
  mutate(lga_of_residence = recode(lga_of_residence, "Tofa" = "Dawakin tofa",
                                   "D/tofa" = "Dawakin tofa",
                                   "Ungoggo" = "Ungogo",
                                   "Kmc" = "Kano municipal",
                                   "Garum mallam" = "Garun mallam"))

# Data-quality check: possible duplicate case records.
# Two or more rows that share ALL of these person-level fields are very likely
# the same case entered twice. We require a real onset date so that mostly-blank
# rows aren't reported as "duplicates". Grouped by year sheet (data_year) so you
# know which tab of cholera.xlsx to inspect.
dup_rows <- merged_diph %>%
  filter(!is.na(dateofonset_day_mnth_yr)) %>%
  group_by(state_of_residence, lga_of_residence, sex_m_f,age_yrs, dateofonset_day_mnth_yr, outcome, date_seen_at_hf_day_mnth_yr) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(data_year, state_of_residence, lga_of_residence, dateofonset_day_mnth_yr)

dup_summary <- dup_rows %>%
  count(data_year, name = "n_duplicate_rows")


## Summary data
epi_summ <- merged_diph %>%
  group_by(epi_year, epi_week, state_of_residence, result_fc) %>%
  summarise(
    suspected = n(),                                          
    confirmed = sum(result_fc == "Positive", na.rm = TRUE),
    deaths = sum(outcome == "Dead" & result_fc == "Positive", na.rm = TRUE),
    number_lga_of_residence = n_distinct(lga_of_residence),
    .groups = "drop"
  )


# WER data  ---------------------------------------------------------------
# 1. Select the required variables for WER
wer_diph <- merged_diph %>%
  select(
    epi_week,
    epi_year,
    result_fc,
    outcome,
    state_of_residence,
    lga_of_residence
  )
# 
# # 2. Ensure `report/Linelists` directory exists
if (!dir.exists(here::here("report", "Linelists"))) {
  dir.create(here::here("report", "Linelists"), recursive = TRUE)
}
# 
# # 4. Build output file path
output_filepath <- here::here(
  "report", "Linelists",
  paste0("wer_chol_", isoyear(report_date), "_W",
         sprintf("%02d", isoweek(report_date)), ".xlsx")
)
# 
# # 5. Write to Excel
writexl::write_xlsx(wer_diph, output_filepath)

