
cum_data <- merged_diph %>% 
  filter(epi_year %in% c(2023, 2024, 2025, 2026))




# weekly_breaks_all <- seq.Date(
#   from = floor_date(min(cum_data$dateofonset_day_mnth_yr, na.rm=T),   "week", week_start = 1), # Monday before first case
#   to   = ceiling_date(max(cum_data$dateofonset_day_mnth_yr, na.rm=T), "week", week_start = 1), # Monday after last case
#   by   = "week")    # bins are 7-days


year_month_labels <- function(x) {
  month_day <- format(x, "%d %b")
  year      <- format(x, "%Y")
  show_year <- c(TRUE, year[-1] != head(year, -1))
  label     <- ifelse(show_year, paste0(month_day, "\n", year), month_day)
  ifelse(is.na(x), "", label)
}

# explicit breaks starting exactly at 2023-01-01, every 12 weeks
custom_breaks <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = max(cum_data$dateofonset_day_mnth_yr, na.rm = TRUE),
  by   = "12 weeks"
)

cum_epicurve <-  ggplot(
  data = cum_data %>% 
    filter(final_classification != "6_Unknown") %>% 
    mutate(final_classification = factor(
      final_classification,
      levels = c("4_Discarded", "5_Pending", "3_Clin compatible", "2_Epid linked", "1_Lab confirmed")
    ))
)+
  
  geom_histogram(
    mapping = aes(x = dateofonset_day_mnth_yr,
                  group = final_classification,
                  fill = final_classification),
    breaks = weekly_breaks_all,
    closed = "left",
    color = "gray31")+
  
  scale_x_date(
    expand       = c(0,4),
    limits       = c(as.Date("2023-01-01"), NA),
    breaks       = custom_breaks,
    minor_breaks = "4 week",
    labels       = year_month_labels)+
  
  scale_fill_manual(
    values = c("1_Lab confirmed" = "#C00000",
               "2_Epid linked"   = "#ED7D31",
               "3_Clin compatible" = "#FFC000",
               "4_Discarded"     = "#00B050",
               "5_Pending"       = "#4472C4"),
    breaks = c("4_Discarded", "5_Pending", "3_Clin compatible", "2_Epid linked", "1_Lab confirmed"),
    labels = c("1_Lab confirmed" = "Lab confirmed",
               "2_Epid linked" = "Epid linked",
               "3_Clin compatible" = "Clin compatible",
               "4_Discarded" = "Discarded",
               "5_Pending" = "Pending"),
    name = "Final Classification") +
  
  labs( x = "Epiweek/yr",
        y = "# of cases") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(hjust = 0.5, lineheight = 0.9),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(8, 8, 18, 8),
    plot.background = element_rect(color = scales::alpha("gray60", 0.6), fill = NA)
  )
cum_epicurve


