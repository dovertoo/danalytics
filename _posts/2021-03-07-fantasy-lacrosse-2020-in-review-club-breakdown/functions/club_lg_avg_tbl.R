#function to generate club vs lg avg tables

club_lg_avg_tbl <- function(club_selec, ...){
     library(gt)
     club_lg_avg_data <- fp_club_lg_avg %>% 
          filter(Club == club_selec | Club == "League Average")
     
     club_lg_avg_data %>% 
          gt() %>% 
          cols_label(
               Club = ""
          ) %>% 
          tab_style(
               style = cell_text(weight = "bold"),
               locations = cells_body(
                    columns = vars(Club)
               )
          ) %>% 
          tab_options(
               column_labels.background.color = "white",
               column_labels.font.weight = "bold",
               table.border.top.width = px(3),
               table.border.top.color = "transparent",
               table.border.bottom.color = "transparent",
               table.border.bottom.width = px(3),
               column_labels.border.top.width = px(3),
               column_labels.border.top.color = "transparent",
               column_labels.border.bottom.width = px(3),
               column_labels.border.bottom.color = "black",
               data_row.padding = px(3),
               source_notes.font.size = 12,
               table.font.size = 16,
               heading.align = "left",
               ...
          ) %>%
          opt_table_font(
               font = list(
                    google_font("Chivo"),
                    default_fonts()
               )
          ) %>% 
          tab_header(
               title = md("**Fantasy Point Totals By Position vs League Average**"),
               subtitle = "2020 Championship Series Pool Play"
          ) %>% 
          tab_source_note(
               source_note = md("**Data:** @FlowFantasyInc | **Table:** @danovertoom")
          ) %>% 
          fmt_number(., columns = 2:7, drop_trailing_zeros = TRUE)
}