
# NCDC logo ---------------------------------------------------------------
  ncdc <- data.frame(
    text = c(
      "Nigeria Centre for Disease Control and Prevention",
      "Protecting the health of Nigerians")
  )
  
  # Convert to flextable
  ncdc <- flextable(ncdc) %>%
    delete_part(part = "header") %>%   # remove column header
    flextable::align(align = "right", part = "all") %>%
    bg(bg = "white", part = "all") %>%
    color(i = 1, color = "black", part = "body") %>%  # row 1 black
    color(i = 2, color = "red", part = "body") %>%    # row 2 red
    bold(part = "all", bold = TRUE) %>%
    fontsize(i = 1, size = 14) %>%     # first row 
    fontsize(i = 2, size = 9) %>%     # second row
    border_remove() %>%
    set_table_properties(layout = "autofit", width = 1)

  
# Diphtheria Titles ---------------------------------------------------------------
  # Create two-row data frame
  title <- data.frame(
    text = c(
      "Diphtheria Situation Report",
      paste0(week_n, ": ", week_period)
    )
  )
  
  # Convert to flextable
  title <- flextable(title) %>%
    delete_part(part = "header") %>%   # remove column header
    flextable::align(align = "center", part = "all") %>%
    bg(bg = "white", part = "all") %>%
    color(color = "#135C2C", part = "all") %>%
    bold(part = "all", bold = TRUE) %>%
    fontsize(i = 1, size = 26) %>%     # first row (title) bigger
    fontsize(i = 2, size = 12) %>%     # second row (week + period)
    border_remove() %>%
    set_table_properties(layout = "autofit", width = 1)


# Highlight of the Week ---------------------------------------------------
key_points <- ftext("Key Points",
                   header_text)


## Highlight
    highlight <- ftext(
      text = paste0("Highlights (key summary)"),
      prop = heading_text
      )
      # --- Paragraph formatting (for alignment) ---
    highlight <- fpar(
      highlight,
        fp_p = fp_par(text.align = "left")   # <-- align left here
      )

# Figure titles ----------------------------------------------------------------
    fig1_title <- ftext(
      text = paste0("Figure 1: Epidemiological curve of final classification of cases in week ",report_week," "),
      prop = heading_text
    )
    fig2_title <- ftext(
      text = paste0("Figure 2: Vaccination status of cases across Age-groups ",report_year," "),
      prop = heading_text
    )
    
    fig3_title <- ftext(
      text = paste0("Figure 3. Map showing distribution of diphtheria cases in Nigeria, ", report_year," "),
      prop = heading_text
    )
    
    fig4_title <- ftext(
      text = paste0("Figure 4. Age-Sex pyramid showing the number of confirmed diphtheria cases ", report_year," "),
      prop = heading_text
    )
    
    
    fig5_title <- ftext(
      text = paste0("Figure 5: Number of Diphtheria cases with case fatality rate (CFR) by state, ", report_year," cumulative through ",
                    tolower(week_n)),
      prop = heading_text
    )
    
    fig6_title <- ftext(
      text = paste0("Figure 6: Trend of suspected cases by epidemiological week ",
                    report_year - 4, " - ", report_year),
      prop = heading_text
    )
    
    chall <- ftext(
      text = paste0("Challenges"),
      prop = heading_text
    )
    
    recomm<- ftext(
      text = paste0("Recommendations"),
      prop = heading_text
    )

# Table titles ------------------------------------------------------------
##Table 1
    table1 <- ftext(
      text = paste0("Table 1: Summary of the current ",
                    tolower(week_n), ", cumulative ", tolower(week_n), ", ", report_year,
                    ", and comparison with the previous year ", report_year-1),
      prop = fp_text(
        font.family = "Arial",
        color = "#138750",
        bold = TRUE,
        font.size = 13
      )
      )
      # --- Paragraph formatting (for alignment) ---
      table1_title <- fpar(
        table1,
        fp_p = fp_par(text.align = "left")   # <-- align left here
      )

## Table 2
table2 <- ftext(
  text = paste0("Table 2. Weekly and cumulative suspected and confirmed cases by state for ", report_year),
  prop = heading_text
  )

table2_title <- fpar(
  table2,
  fp_p = fp_par(text.align = "left")   # <-- align left here
)

#Footer
footer <- ftext("Information for this diseases were retrieved from the Technical Working Group and Situation Reports of Nigerian Centre for Disease Control and Prevention",
                fp_text(color = "black", bold = F, font.size = 8)

)

# Footnote text (single source of truth). Used by the Word/Quarto report (as the
# "cases" footnote) AND by the PowerPoint (printed under the summary table), so
# editing it here updates every output.
footnote_confirmed <- "Confirmed cases represent a subset of suspected cases; they are not mutually exclusive."

