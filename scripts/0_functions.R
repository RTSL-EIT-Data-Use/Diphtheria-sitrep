# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman")

pacman::p_load(ggnewscale,ggplot2, remotes, ggthemr, rio, here, dplyr, janitor, tidyverse, 
               lubridate, magrittr, rmarkdown, knitr, kableExtra, toOrdinal, 
               gridExtra, ggpubr, gridtext, grid, patchwork, viridisLite, png, webshot2, scales, 
               ggforce, ggh4x, gt, commonmark, flextable, officedown, officer, 
               sf, ggspatial, stringr, openxlsx, ggrepel, RColorBrewer, maptiles,
               terra, tidyterra,leaflet, pointdensityP, classInt, magick,
               mapview, english, zoo, cowplot, glue, writexl)

# devtools::install_github("cttobin/ggthemr", force = TRUE) 

# Monday of the week containing report_date
monday <- function(report_date){
  lubridate::floor_date(report_date, unit = "week", week_start = 1)
}

# Sunday of that same week
sunday <- function(report_date){
  monday(report_date) + 6
}

# Cite a whole week period (mon - sun)
mon_to_sun <- function(report_date){
  start_date <- monday(report_date)
  end_date <- sunday(report_date)
  
  ifelse(lubridate::month(start_date)==lubridate::month(end_date),
         sprintf("%s - %s, %s, %.0f", 
                 scales::ordinal(lubridate::day(start_date)),
                 scales::ordinal(lubridate::day(end_date)),
                 lubridate::month(start_date, label = T, abbr = T),
                 lubridate::year(start_date)),
         sprintf("%s, %s - %s, %s, %.0f",
                 scales::ordinal(lubridate::day(start_date)),
                 lubridate::month(start_date, label = T, abbr = T),
                 scales::ordinal(lubridate::day(end_date)),
                 lubridate::month(end_date, label = T, abbr = T),
                 lubridate::year(start_date)))
}

# Convert date to epidemiological year and week
week_period <- mon_to_sun(report_date)

report_year <- isoyear(report_date)
report_week <- isoweek(report_date)
prev_year <- report_year - 1

# Previous week date
prev_date <- report_date - weeks(1)
prev_week <- isoweek(prev_date)

# ISO year the previous week belongs to
prev_week_year <- isoyear(prev_date)   

week_n <- paste0("Week ", lubridate::isoweek(report_date))
prev_week_n <- paste0("Week ", prev_week)

set_flextable_defaults(
  font.face = "Arial",
  align = 'center',
  font.size = 14)

# Normalize Nigerian state names so the surveillance data matches the
# GADM shapefile's `name_1`. Used on BOTH the data side and the shapefile
# side so the join keys are guaranteed identical.
clean_state_names <- function(x) {
  x <- stringr::str_squish(x)                # trim + collapse stray spaces
  x <- stringr::str_to_title(x)              # standardize capitalization
  x <- stringr::str_remove(x, "\\s+State$")  # drop a trailing " State"
  dplyr::case_when(
    x %in% c("Fct", "F.C.T.", "Federal Capital Territory") ~ "FCT",
    x == "Akwa-Ibom"   ~ "Akwa Ibom",
    x == "Cross-River" ~ "Cross River",
    x == "Jos"         ~ "Plateau",
    TRUE ~ x
  )
}

