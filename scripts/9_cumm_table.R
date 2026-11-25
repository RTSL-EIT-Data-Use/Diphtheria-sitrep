
##Helper functions (weekly + cumulative)
get_weekly_data <- function(df, yr, wk) {
  df %>%
    filter(epi_year == yr, epi_week == wk) %>%
    mutate(
      result_fc = stringr::str_to_lower(result_fc),
      # result_cul = stringr::str_to_lower(result_cul)
    ) %>%
    group_by(state_of_residence) %>%
    summarise(
      suspected = dplyr::n(),  # line list: each row = suspected case
      rdt_pos   = sum(result_fc == "positive", na.rm = TRUE),
      # cul_pos   = sum(result_cul == "positive", na.rm = TRUE),
      deaths    = sum(stringr::str_to_lower(outcome) == "dead", na.rm = TRUE),
      .groups = "drop"
    )
}

get_cumulative_data <- function(df, yr, max_wk) {
  df %>%
    filter(epi_year == yr, epi_week <= max_wk) %>%
    mutate(
      result_fc = stringr::str_to_lower(result_fc),
      # result_cul = stringr::str_to_lower(result_cul)
    ) %>%
    group_by(state_of_residence) %>%
    summarise(
      suspected = dplyr::n(),
      rdt_pos   = sum(result_fc == "positive", na.rm = TRUE),
      # cul_pos   = sum(result_cul == "positive", na.rm = TRUE),
      deaths    = sum(stringr::str_to_lower(outcome) == "dead" & result_fc == "positive", na.rm = TRUE),
      .groups = "drop"
    )
}

## Current vs previous week + trend arrow (Suspected)

curr_week_data <- get_weekly_data(merged_diph, report_year, report_week) %>%
  rename_with(~ paste0(., "_curr"), -state_of_residence)

prev_week_data <- get_weekly_data(
  merged_diph,
  isoyear(report_date - lubridate::weeks(1)),
  isoweek(report_date - lubridate::weeks(1))
) %>%
  rename_with(~ paste0(., "_prev"), -state_of_residence)

weekly_data_merged <- curr_week_data %>%
  full_join(prev_week_data, by = "state_of_residence") %>%
  mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
  mutate(
    suspected_change = case_when(
      suspected_curr > suspected_prev ~ " \u25b2",
      suspected_curr < suspected_prev ~ " \u25bc",
      TRUE ~ ""
    )
  ) %>%
  transmute(
    state_of_residence,
    suspected_cw = suspected_curr,
    rdt_cw       = rdt_pos_curr,
    suspected_change,
    deaths_cw    = deaths_curr
  )

## Current vs previous week + trend arrow (Suspected)
cumulative_data <- get_cumulative_data(merged_diph, report_year, report_week) %>%
  transmute(
    state_of_residence,
    suspected_cum = suspected,
    rdt_cum       = rdt_pos,
    deaths_cum    = deaths
  )

## Final merge + Total row + zero suppression
final_data <- weekly_data_merged %>%
  full_join(cumulative_data, by = "state_of_residence") %>%
  mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
  arrange(desc(suspected_cum), state_of_residence)

grand_total <- final_data %>%
  summarise(
    state_of_residence = "Total",
    across(where(is.numeric), sum),
    suspected_change = case_when(
      sum(suspected_cw) > sum(prev_week_data$suspected_prev, na.rm = TRUE) ~ " \u25b2",
      sum(suspected_cw) < sum(prev_week_data$suspected_prev, na.rm = TRUE) ~ " \u25bc",
      TRUE ~ ""
    )
  )

Summary_table_data <- bind_rows(final_data, grand_total) %>%
  select(
    state_of_residence,
    suspected_cw, rdt_cw, suspected_change, deaths_cw,
    suspected_cum, rdt_cum, deaths_cum
  )

numeric_cols_to_suppress <- c(
  "suspected_cw","rdt_cw","deaths_cw",
  "suspected_cum","rdt_cum","deaths_cum"
)

Summary_table_data <- Summary_table_data %>%
  mutate(across(all_of(numeric_cols_to_suppress),
                ~ ifelse(. == 0, "", as.character(round(.)))))

# Green zebra stripe on every other DATA row (1, 3, 5, …), never the Total row.
# The Total row is always the last row, so the data rows are all rows except the last.
n_data_rows <- nrow(Summary_table_data) - 1
stripe_rows <- if (n_data_rows >= 1) seq(1, n_data_rows, by = 2) else integer(0)


## flextable with new headers
cumm_table <- flextable(Summary_table_data) %>%
  theme_vanilla() %>%
  set_header_df(
    mapping = data.frame(
      col_keys = c(
        "state_of_residence",
        "suspected_cw","rdt_cw","suspected_change","deaths_cw",
        "suspected_cum","rdt_cum","deaths_cum"
      ),
      level_0 = c(
        "",
        rep(paste0("Current week (Week ", report_week, ")"), 4),
        rep(paste0("Cumulative (Week 1 - ", report_week, ")"), 3)
      ),
      level_1 = c(
        "State",
        "Suspected", "Confirmed", "Trend", "Deaths",
        "Suspected", "Confirmed", "Deaths"
      ),
      stringsAsFactors = FALSE
    ),
    key = "col_keys"
  ) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  merge_at(i = 1, j = 6:8, part = "header") %>%
  merge_v(j = 1, part = "header") %>%
  color(i = 1, part = "header", color = "#138750") %>%
  color(i = 2, part = "header", color = "black") %>%
  bg(part = "header", bg = "white") %>%
  bold(part = "header") %>%
  flextable::align(align = "center", part = "header") %>%
  flextable::align(j = 2:8, align = "center", part = "body") %>%
  flextable::align(j = 1, align = "left", part = "body") %>%
  bold(j = 1, part = "body") %>%
  bold(i = nrow(Summary_table_data), part = "body") %>%
  bg(i = nrow(Summary_table_data), bg = "#d0d0d0", part = "body") %>%
  color(j = "suspected_change", i = ~ suspected_change == " \u25b2", color = "red", part = "body") %>%
  color(j = "suspected_change", i = ~ suspected_change == " \u25bc", color = "green", part = "body") %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 1.5, part = "all") %>%
  flextable::width(j = 2:8, width = 0.72) %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  bg(i = stripe_rows, bg = "#E6F4E6", part = "body") %>%
  autofit() %>%
  border_remove() %>%
  hline(part = "all", border = fp_border(color = "black", width = 1)) %>%
  vline(part = "all", border = fp_border(color = "black", width = 1)) %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1))

cumm_table
