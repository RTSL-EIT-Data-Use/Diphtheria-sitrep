
###########Import data

# Years to include in the report. THIS IS THE ONLY PLACE TO EDIT to add a year:
# add the new sheet to cholera.xlsx, then add its year string to this vector.
data_years <- c("2022", "2023", "2024", "2025", "2026")

# Read every year's sheet into one named list (list names = the year strings).
diph_by_year <- lapply(data_years, function(yr) {
  import(here::here("data", "DIPHTHERIA_2026_yr.xlsx"), sheet = yr) %>%
    clean_names()
})
names(diph_by_year) <- data_years

lga_pop <- import(here::here("data", "lga_pop.xlsx")) %>% 
  clean_names()


#Load Admin Boundaries
nigeria_adm <- read_sf(here("map/gadm41_NGA_1.shp")) %>% 
  clean_names()

nigeria_lga <- read_sf(here("map/gadm41_NGA_2.shp")) %>% 
  clean_names()

