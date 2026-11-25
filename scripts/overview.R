library(officer)
library(flextable)
library(magrittr)

## ---------------------------------------------------------------
## 1. Define your data (replace with real values from your pipeline)
## ---------------------------------------------------------------
epi_week_now      <- 35
epi_year_now      <- 2026
cum_start_label   <- "Epi wk 1, 29th Dec 2025"
cum_end_label     <- "Epi wk 35, 24th August 2026"

# Current week
cw_suspected      <- 69
cw_states         <- 3
cw_lgas           <- 13
cw_confirmed      <- 48
cw_lc             <- 0
cw_el             <- 24
cw_cc             <- 24
cw_discarded      <- 15
cw_pending        <- 6

# Cumulative 2026
cum_suspected     <- 16182
cum_states        <- 26
cum_lgas          <- 296
cum_confirmed     <- 13024
cum_lc            <- 60
cum_el            <- 2583
cum_cc            <- 10381
cum_discarded     <- 161
cum_pending       <- 442
cum_deaths        <- 365
cum_cfr           <- "2.8%"

## ---------------------------------------------------------------
## 2. Create a blank PowerPoint slide (16:9)
## ---------------------------------------------------------------
ppt <- read_pptx() %>%
  add_slide(layout = "Blank", master = "Office Theme")

slide_size <- slide_size(ppt)   # width, height in inches

## ---------------------------------------------------------------
## 3. Helper to add a coloured rounded box with text
## ---------------------------------------------------------------
add_box <- function(ppt, left, top, width, height,
                    text,
                    fill = "#138750",
                    border_color = "#138750",
                    font_color = "white",
                    font_size = 16,
                    bold = TRUE,
                    align = "center") {
  
  ft <- flextable(data.frame(x = text)) %>%
    delete_part(part = "header") %>%
    border_remove() %>%
    bg(bg = fill, part = "body") %>%
    color(color = font_color, part = "body") %>%
    bold(bold = bold, part = "body") %>%
    fontsize(size = font_size, part = "body") %>%
    align(align = align, part = "body") %>%
    valign(valign = "center", part = "body") %>%
    padding(padding = 4, part = "body") %>%
    width(width = width) %>%
    height(height = height) %>%
    set_table_properties(layout = "fixed")
  
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- remove_slide(ppt, index = length(ppt))   # discard extra slide
  ppt
}

## ---------------------------------------------------------------
## 4. Manually add all elements to the slide
## ---------------------------------------------------------------
ppt <- read_pptx() %>%
  add_slide(layout = "Blank", master = "Office Theme")

## --- Title ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext("Overview of 2026 Diphtheria Outbreak",
                       prop = fp_text(color = "#138750", font.size = 28, bold = TRUE))
               ),
               location = ph_location(left = 0.4, top = 0.2, width = 12.3, height = 0.7)
)

## --- Left panel: Epi-week 35, 2026 header ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Epi-week ", epi_week_now, ", ", epi_year_now),
                       prop = fp_text(color = "#138750", font.size = 18, bold = TRUE, underlined = TRUE))
               ),
               location = ph_location(left = 0.7, top = 1.1, width = 5.5, height = 0.4)
)

## --- Left: Suspected cases green box ---
ppt <- ph_with(ppt,
  value = fpar(
    ftext(paste0(cw_suspected, " Suspected Cases:"),
          prop = fp_text(color = "white", font.size = 16, bold = TRUE)),
    ftext("\n", prop = fp_text(color = "white", font.size = 16, bold = FALSE)),  # ← fixed
    ftext(paste0(cw_states, " (State(s), ", cw_lgas, " LGA(s))"),
          prop = fp_text(color = "#FFF200", font.size = 14, bold = TRUE))
  ),
  location = ph_location(left = 0.7, top = 1.6, width = 5.5, height = 0.9,
                         bg = "#138750")
)

## --- Left: Confirmed cases grey box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Confirmed cases:", cw_confirmed),
                       prop = fp_text(color = "#138750", font.size = 14, bold = TRUE)),
                 ftext("\n", prop = fp_text(color = "#138750", font.size = 13, bold = FALSE)),
                 ftext(paste0("LC: ", cw_lc, ", EL: ", cw_el, ", CC: ", cw_cc),
                       prop = fp_text(color = "#138750", font.size = 13, bold = TRUE, underlined = TRUE))
               ),
               location = ph_location(left = 1.8, top = 2.8, width = 4.4, height = 0.8,
                                      bg = "#E8E8E8")
)

## --- Left: Discarded box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Discarded case: ", cw_discarded),
                       prop = fp_text(color = "#C00000", font.size = 14, bold = TRUE))
               ),
               location = ph_location(left = 1.8, top = 3.8, width = 4.4, height = 0.7,
                                      bg = "#E8E8E8")
)

## --- Left: Pending box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Pending: ", cw_pending),
                       prop = fp_text(color = "#C00000", font.size = 14, bold = TRUE))
               ),
               location = ph_location(left = 1.8, top = 4.7, width = 4.4, height = 0.7,
                                      bg = "#E8E8E8")
)

## --- Right panel: Cumulative 2026 header ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Cumulative ", epi_year_now, " (", cum_start_label, " – ", cum_end_label, ")"),
                       prop = fp_text(color = "#138750", font.size = 14, bold = TRUE, underlined = TRUE))
               ),
               location = ph_location(left = 6.5, top = 1.05, width = 6.2, height = 0.4)
)

## --- Right: Suspected green box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext("Suspected Cases:",
                       prop = fp_text(color = "white", font.size = 16, bold = TRUE)),
                 ftext("\n",
                       prop = fp_text(color = "white", font.size = 16, bold = FALSE)),
                 ftext(paste0(format(cum_suspected, big.mark = ","), " (", cum_states,
                              " State(s), ", cum_lgas, " LGA(s))"),
                       prop = fp_text(color = "#FFF200", font.size = 13, bold = TRUE))
               ),
               location = ph_location(left = 6.5, top = 1.55, width = 6.2, height = 0.9,
                                      bg = "#138750")
)

## --- Right: Confirmed cases grey box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Confirmed cases: ", format(cum_confirmed, big.mark = ",")),
                       prop = fp_text(color = "#138750", font.size = 13, bold = TRUE)),
                 ftext("\n",
                       prop = fp_text(color = "#138750", font.size = 12, bold = FALSE)),
                 ftext(paste0("LC:", cum_lc, ", EL:", format(cum_el, big.mark = ","),
                              ", CC: ", format(cum_cc, big.mark = ",")),
                       prop = fp_text(color = "#138750", font.size = 12, bold = TRUE, underlined = TRUE))
               ),
               location = ph_location(left = 7.5, top = 2.75, width = 5.2, height = 0.8,
                                      bg = "#E8E8E8")
)

## --- Right: Discarded box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Discarded case: ", cum_discarded),
                       prop = fp_text(color = "#C00000", font.size = 14, bold = TRUE))
               ),
               location = ph_location(left = 7.5, top = 3.75, width = 5.2, height = 0.7,
                                      bg = "#E8E8E8")
)

## --- Right: Pending box ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Pending: ", cum_pending),
                       prop = fp_text(color = "#C00000", font.size = 14, bold = TRUE))
               ),
               location = ph_location(left = 7.5, top = 4.7, width = 5.2, height = 0.7,
                                      bg = "#E8E8E8")
)

## --- Bottom: Deaths + CFR ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext(paste0("Total Confirmed Deaths: ", cum_deaths),
                       prop = fp_text(color = "#C00000", font.size = 15, bold = TRUE)),
                 ftext("\n",
                       prop = fp_text(color = "#C00000", font.size = 15, bold = FALSE)),
                 ftext(paste0("CFR ", cum_cfr),
                       prop = fp_text(color = "#C00000", font.size = 15, bold = TRUE))
               ),
               location = ph_location(left = 6.5, top = 5.6, width = 6.2, height = 1.0,
                                      bg = "#138750")
)

## --- Footer ---
ppt <- ph_with(ppt,
               value = fpar(
                 ftext("Nigeria Centre For Disease Control And Prevention",
                       prop = fp_text(color = "white", font.size = 12))
               ),
               location = ph_location(left = 0, top = 7.1, width = 13.33, height = 0.4,
                                      bg = "#138750")
)

## ---------------------------------------------------------------
## 5. Save the slide
## ---------------------------------------------------------------
print(ppt, target = here::here("report", "diphtheria_overview.pptx"))
