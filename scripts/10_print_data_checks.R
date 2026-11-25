###############################################################################
# Data-quality checks
# ---------------------------------------------------------------------------
# These objects are created during report generation (in 2_data_cleaning.R and
# 5_map_cases.R). This script just prints them to the console. It is sourced by
# 00_Run the report.R AFTER the report has been rendered, so the variables
# already exist in the workspace. The exists() guards make it safe to run even
# if a check produced nothing.
###############################################################################

# 2a. Rows where date_seen_at_hf was used in place of a missing/invalid onset date
if (exists("n_hf_used") && n_hf_used > 0) {
  message(n_hf_used, " row(s) have missing/invalid date_of_onset; date_seen_at_hf was used instead. By source year:")
  print(n_hf_used_by_year)
}

# 2b. Rows still excluded: neither a valid onset date nor a valid date_seen_at_hf
if (exists("n_bad_date") && n_bad_date > 0) {
  message(n_bad_date, " row(s) have missing/invalid date_of_onset AND no valid date_seen_at_hf, and are excluded from counts. By source year:")
  print(n_bad_date_by_year)
}

if (exists("unmatched_outcome") && nrow(unmatched_outcome) > 0) {
  message("Unclean outcome value(s) - consider cleaning the input dataset :")
  print(unmatched_outcome)
}

if (exists("unmatched_states") && length(unmatched_states) > 0) {
  message("State name(s) not matched to the shapefile (their cases are dropped from the map): ",
          paste(unmatched_states, collapse = ", "))
}

if (exists("unmatched_rdt") && nrow(unmatched_rdt) > 0) {
  message("Unrecognized RDT result value(s) — currently counted as Negative (by year sheet):")
  print(unmatched_rdt)
}
if (exists("unmatched_cul") && nrow(unmatched_cul) > 0) {
  message("Unrecognized culture result value(s) — currently counted as Negative (by year sheet):")
  print(unmatched_cul)
}

if (exists("year_mismatch") && nrow(year_mismatch) > 0) {
  message("Onset-date year doesn't match the source sheet — possible date typos (by year sheet):")
  print(year_mismatch)
}

if (exists("implausible_age") && nrow(implausible_age) > 0) {
  message("Age value(s) that aren't a plausible whole number of years — not assigned an age group or categorized 45+ (by year sheet):")
  print(implausible_age)
}

if (exists("dup_summary") && nrow(dup_summary) > 0) {
  message("Possible duplicate case records (same state, LGA, sex, age, onset date, date seen at HF, outcome), by year sheet:")
  print(dup_summary)
  message("  -> inspect the full duplicated rows with: View(dup_rows)")
}

