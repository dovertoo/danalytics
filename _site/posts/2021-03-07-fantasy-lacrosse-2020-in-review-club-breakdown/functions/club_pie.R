#function to create club pie charts pf FP position breakdown

club_pie <- function(club_selec){
     library(tastypie)
     club_pie_data <- fp_club_pie %>% 
          filter(Club == club_selec) %>% 
          select(-Club)
     
     pie_bake(data = club_pie_data, template = "basic4", perc =TRUE,
              title = club_selec, group_name = "Position")
}