#test3

library("maps")
library("dplyr")
library("leaflet")

plotInterActiveMap <- function(vor.polygon,point.centerVorinoi,point.raw){
  m = leaflet() %>% addTiles()
  m %>% addPolygons(data = vor.polygon,fill = TRUE, color = c('red'),
                    popup = paste0("Area: ", as.character(vor.polygon@data$Group.1)," square km")) %>% 
        addCircles(data = point.raw,color = c('Green'))  %>%
        addCircles(data = point.centerVorinoi,color = c('blue'))
}

