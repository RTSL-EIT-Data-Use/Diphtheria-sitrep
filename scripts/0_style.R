center_par <- fp_par(
  text.align = "center", 
  padding = 10, 
  shading.color = "transparent",
)

footer_par <- fp_par(
  text.align = "center", 
  padding = 10, 
  shading.color = "#138750",
)

header_par <- fp_par(
  text.align = "left",
  padding = 10,
  shading.color = "#FFC82D"
)

header_text <- fp_text(color = "black", bold = T, font.size = 15)

title_par <- fp_par(
  text.align = 'center',
  padding.top = 5, padding.left = 10,
  shading.color = "#138750"
)

title_text <- fp_text(color = "white", bold = T, font.size = 14)

subtitle_par <- fp_par(
  text.align = 'left',
  padding = 10,
  shading.color = "#138750"
)

# Shared green heading style: Arial 13, bold, NCDC green (#138750).
# Reused for the Table titles, figure titles,
# and the Challenges / Recommendations headers so they stay identical.
heading_text <- fp_text(
  font.family = "Arial",
  color       = "#138750",
  bold        = TRUE,
  font.size   = 13
)