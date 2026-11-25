# 1) Clean state names
# ----------------------------
epi_clean <- epi_summ   # state already normalized in 2_data_cleaning.R

nigeria_adm <- nigeria_adm %>%
  mutate(name_1 = clean_state_names(name_1))

unmatched_states <- setdiff(unique(epi_clean$state_of_residence), nigeria_adm$name_1)
if (length(unmatched_states) > 0) {
  print("States in epi_clean not matched to shapefile:")
  print(unmatched_states)
} else {
  print("All states matched successfully.")
}

# ----------------------------
# 2) Summarise confirmed/suspected (FIXED filter: epi_week <= report_week)
# ----------------------------
state_cases_latest <- epi_clean %>%
  filter(epi_year == report_year, epi_week <= report_week) %>%   # << fixed
  group_by(state_of_residence) %>%
  summarise(
    suspected = sum(suspected, na.rm = TRUE),
    confirmed = sum(confirmed, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 3) Merge with shapefile + status + confirmed categories
# 1) Prep: categories to match the attached map legend
# ---------------------------------------------------------
# diph_map_sf <- nigeria_adm %>%
#   left_join(state_cases_latest, by = c("name_1" = "state_of_residence")) %>%
#   mutate(
#     suspected = replace_na(suspected, 0),
#     deaths    = replace_na(deaths, 0),
#     
#     suspected_cat = case_when(
#       suspected == 0 ~ "0",
#       suspected >= 1  & suspected < 10  ~ "1-9",
#       suspected >= 10 & suspected <  50  ~ "10-49",
#       suspected >= 50 & suspected <  100 ~ "50-99",
#       suspected >= 100                  ~ "100+",
#       TRUE ~ NA_character_
#     ),
#     suspected_cat = factor(
#       suspected_cat,
#       # Levels must match the labels produced by case_when() above AND the
#       # names in fill_cols below — otherwise polygons fall through to NA
#       # and render as gray.
#       levels = c("0", "1-9", "10-49", "50-99", "100+")
#     )
#   )



diph_map_sf <- nigeria_adm %>%
  left_join(state_cases_latest, by = c("name_1" = "state_of_residence")) %>%
  mutate(
    confirmed = replace_na(confirmed, 0),
    deaths    = replace_na(deaths, 0),
    
    confirmed_cat = case_when(
      confirmed == 0 ~ "0",
      confirmed >= 1  & confirmed < 10  ~ "1-9",
      confirmed >= 10 & confirmed <  50  ~ "10-49",
      confirmed >= 50 & confirmed <  100 ~ "50-99",
      confirmed >= 100                  ~ "100+",
      TRUE ~ NA_character_
    ),
    confirmed_cat = factor(
      confirmed_cat,
      # Levels must match the labels produced by case_when() above AND the
      # names in fill_cols below — otherwise polygons fall through to NA
      # and render as gray.
      levels = c("0", "1-9", "10-49", "50-99", "100+")
    )
  )













# ---------------------------------------------------------
# 2) Label points (safe for complex polygons)
# ---------------------------------------------------------
state_labels <- diph_map_sf %>%
  mutate(
    label = str_wrap(name_1, width = 10),
    label_point = st_point_on_surface(geometry)
  )

# ---------------------------------------------------------
# 3) Death points at state surfaces (with slight jitter)
# ---------------------------------------------------------
set.seed(123)

death_pts <- diph_map_sf %>%
  filter(deaths > 0) %>%
  st_point_on_surface() %>%
  mutate(
    X = st_coordinates(.)[, 1],
    Y = st_coordinates(.)[, 2]
  ) %>%
  mutate(
    X_jit = X + runif(n(), -0.15, 0.15),
    Y_jit = Y + runif(n(), -0.15, 0.15)
  ) %>%
  st_drop_geometry()

# ---------------------------------------------------------
# 4) Colors to match the attached style (white -> dark green)
# ---------------------------------------------------------
fill_cols <- c(
  "0"     = "white",
  "1-9"   = "#A1D99B",   # ColorBrewer Greens 5-class (light)
  "10-49" = "#74C476",   # ColorBrewer Greens 5-class
  "50-99" = "#31A354",   # ColorBrewer Greens 5-class
  "100+"  = "#006D2C"    # ColorBrewer Greens 5-class (dark)
)

# ---------------------------------------------------------
# 4b) Dummy layer data to FORCE legend swatches to use colors
# ---------------------------------------------------------
legend_dummy <- data.frame(
  suspected_cat = factor(names(fill_cols), levels = names(fill_cols))
)

# ---------------------------------------------------------
diph_map <- ggplot() +
  
  # ---- Dummy layer to force ALL case categories to appear in legend ----
geom_point(
  data = legend_dummy,
  aes(x = 0, y = 0, fill = suspected_cat),
  shape = 22, size = 5, color = "grey40", alpha = 0
) +
  
  # ---- Polygons ----
geom_sf(
  data = diph_map_sf,
  aes(fill = confirmed_cat),
  color = "black",
  linewidth = 0.4
) +
  
  # ---- State labels ----
geom_sf_text(
  data = state_labels,
  aes(label = name_1, geometry = label_point),
  size = 3,
  fontface = "bold",
  color = "grey20"
) +
  
  # ---- Death points ----
geom_point(
  data = death_pts,
  aes(x = X_jit, y = Y_jit, color = "Deaths"),
  size = 2.2
) +
  
  # ---- Scales ----
scale_fill_manual(
  name   = "Cases",
  values = fill_cols,
  limits = names(fill_cols),
  drop   = FALSE
) +
  scale_color_manual(
    name   = NULL,
    values = c("Deaths" = "red"),
    limits = "Deaths",
    drop   = FALSE
  ) +
  
  guides(
    fill  = guide_legend(
      order = 1,
      override.aes = list(shape = 22, size = 5, color = "grey40", alpha = 1)
    ),
    color = guide_legend(order = 2, override.aes = list(size = 3))
  ) +
  
  # ---- Scale bar + North arrow ---------------------------------------------
  # Using ggspatial::annotation_scale at "br" again — the extended ylim in
  # coord_sf below pushes the panel bottom into the ocean strip below Nigeria,
  # so the scale bar lands there cleanly without a gap above it.
  # (Previous experiment with annotate("rect", fill = "black") interfered with
  # scale_fill_manual and made the choropleth render as gray, so we keep
  # annotation_scale here.)
annotation_scale(
  location   = "br",
  width_hint = 0.20,
  pad_x      = unit(0.3, "cm"),
  pad_y      = unit(0.3, "cm")
) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.2, "cm"),   # sits in the empty area west of Nigeria
    pad_y = unit(0.2, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # ---- Frame extent --------------------------------------------------------
  # Extend xmin from Nigeria's natural ~2.7 westward to 1.4 to create empty
  # white space on the LEFT for the north arrow to live in (outside Nigeria's
  # western border, in what would be Benin Republic). Likewise extend ymin
  # from ~4.27 down to 3.30 to create an ocean strip below for the scale bar.
coord_sf(xlim = c(1.4, 14.8),
         ylim = c(3.30, 14.0),
         expand = FALSE,
         datum = NA) +
  
  # ---- Theme (Word-friendly) ----
theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey30", linewidth = 0.6),
    legend.key = element_rect(color = "grey40"),
    legend.key.size = unit(0.55, "cm"),
    legend.spacing.y = unit(0.06, "cm"),
    legend.margin = margin(4, 4, 4, 4),
    
    legend.title = element_text(face = "bold", size = 10),
    legend.text  = element_text(size = 9),
    
    panel.border = element_rect(fill = NA, color = "white", linewidth = 1),
    
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    
    # tighter bottom margin so scale sits closer
    plot.margin = margin(5, 8, 3, 8)
  )
diph_map

