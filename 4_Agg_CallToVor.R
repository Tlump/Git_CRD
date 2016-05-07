# count number of call per site
call.agg <- plyr::count(call.all,c("Hour","Longitude","Latitude"))


# Assignment Area Number that cell insite
call.sp <- call.agg
coordinates(call.sp) <- ~ Longitude + Latitude
proj4string(call.sp) <- proj4string(vor.bkk)
call.agg$inside  <-!is.na(over(call.sp,as(vor.bkk, "SpatialPolygons")))
call.agg$vorID <- over(call.sp, vor.bkk)$id
call.agg <- call.agg[call.agg$inside==TRUE,]

call.aggArea <- plyr::count(call.agg,c("vorID","Hour"))


# countWithinArea <- function(id,data){
#   ret <- sum (data[data$vorid %in% id,]$freq)
#   return(ret)
# }
# 
# vor.bkk@data$count <- do.call(rbind,lapply(vor.bkk@data$id, function(x) countWithinArea(x,call.agg)))
write.csv(call.aggArea, file = "/Users/TLump_MAC/Desktop/CallaggArea.csv")


library(rgdal)
library(leaflet)
map <- leaflet(vor.bkk) %>% addTiles()
map %>% addPolygons(stroke = FALSE, smoothFactor = 0.8, fillOpacity = 1,color = "RED", popup = ~as.character(id),group = "fill_1") #%>%
  #addPolygons(stroke = FALSE, smoothFactor = 0.8, fillOpacity = 0.5,color = ~bpal(count_boundary), popup = ~as.character(count),group = "fill_0.5") %>%
  #addLegend(pal = bpal, values = ~count_boundary, opacity = 1,title = "Total Calls")  %>%
  #addLayersControl(
  #  baseGroups = c("fill_1","fill_0.5"),
  #  options = layersControlOptions(collapsed = FALSE)
  #)
map
