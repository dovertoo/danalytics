#function to generate defensive player tables

defense_plyr_tbl <- function(club_selec, num_defense, ...){
     library(gt)
     #gather top n defenders from selected club
     defense_plyr_data <- summary_2020_plyr %>% 
          filter(Club == club_selec) %>% 
          filter(Position == "Defense") %>% 
          select(1:12, -Club, -Position) %>% 
          select(Player, TotalFantasyPoints, FantasyPointsPG, TotalCausedTurnovers, TotalGroundBalls, everything()) %>% 
          arrange(desc(TotalFantasyPoints)) %>% 
          slice(1:num_defense)
     #making gt() table
     defense_plyr_data %>% 
          gt() %>% 
          cols_width(
                    "Player" ~ px(175),
                    "TotalTurnovers" ~ px(85),
                    "TotalCausedTurnovers" ~ px(100),
                    everything() ~ px(70)
          ) %>%   
          cols_label(
               Player = "",
               TotalFantasyPoints = "Fantasy Points",
               FantasyPointsPG = "FP PG",
               TotalPoints = "Points",
               TotalGoals = "Goals",
               Total2ptGoals = "2pt Goals",
               TotalAssists = "Assists",
               TotalGroundBalls = "Ground Balls",
               TotalTurnovers = "Turnovers",
               TotalCausedTurnovers = "Caused Turnovers"
          ) %>% 
          tab_style(
               style = cell_text(weight = "bold"),
               locations = cells_body(
                    columns = vars(Player)
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
               title = md("**Defensive Player Fantasy Summary**"),
               subtitle = "2020 Championship Series Pool Play"
          ) %>% 
          tab_source_note(
               source_note = md("**Data:** @FlowFantasyInc | **Table:** @danovertoom")
          ) %>% 
          fmt_number(., columns = 2:7, drop_trailing_zeros = TRUE)

}