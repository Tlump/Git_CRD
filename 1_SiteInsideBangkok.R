library("rgdal")
# read map
map <- readOGR(dsn="/Users/TLump_MAC/Thesis/Data/tam_geo/",layer = "tam_geo");
map.bkk <- map[map$PROV_NAME=="BANGKOK",]
rm(map)

# # read CRD
# ais10.lv1 <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/CU/cu_location_10_lv1_Social.csv",header = TRUE,sep ='|')
# ais10.lv2 <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/CU/cu_location_10_lv2_Social.txt",header = TRUE,sep ='|')
# ais11.lv1 <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/CU/cu_location_11_lv1_Social.txt",header = TRUE,sep ='|')
# ais11.lv2 <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/CU/cu_location_11_lv2_Social.txt",header = TRUE,sep ='|')
# 
# site10.lv1 <- subset(ais10.lv1,select=c("longitude","latitude"))
# site10.lv1 <- site10.lv1[!duplicated(site10.lv1[,c("longitude","latitude")] ),]
# rm(ais10.lv1)
# 
# site10.lv2 <- subset(ais10.lv2,select=c("longitude","latitude"))
# site10.lv2 <- site10.lv2[!duplicated(site10.lv2[,c("longitude","latitude")] ),]
# rm(ais10.lv2)
# 
# site11.lv1 <- subset(ais11.lv1,select=c("longitude","latitude"))
# site11.lv1 <- site11.lv1[!duplicated(site11.lv1[,c("longitude","latitude")] ),]
# rm(ais11.lv1)
# 
# site11.lv2 <- subset(ais11.lv2,select=c("longitude","latitude"))
# site11.lv2 <- site11.lv2[!duplicated(site11.lv2[,c("longitude","latitude")] ),]
# rm(ais11.lv2)
# 
# site <- rbind(site10.lv1,site10.lv2,site11.lv1,site11.lv2)
# rm(site10.lv1)
# rm(site10.lv2)
# rm(site11.lv1)
# rm(site11.lv2)
# 
# site <- site[complete.cases(site),]
# 
# #filter bbox site in bangkok
# site <- site[site$longitude>100.32905 & site$longitude<100.93857&site$latitude>13.49254 & site$latitude<13.95527,]
# 
# names(site)[1] <- paste("Longitude")
# names(site)[2] <- paste("Latitude")

site <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/site_BKK-bbox_AllSocial.csv",header = TRUE,sep =',')

site <- site[!duplicated(site$Longitude,site$Latitude),]


site.sp <- site
# Assignment modified according
coordinates(site.sp) <- ~ Longitude + Latitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(site.sp) <- proj4string(map.bkk)

site$inside  <-!is.na(over(site.sp,as(map.bkk, "SpatialPolygons")))
site$Prov <- over(site.sp, map.bkk)$PROV_NAME
site$AMP <- over(site.sp, map.bkk)$AMP_NAME
site$TAM <- over(site.sp, map.bkk)$TAM_NAME

site.bkk <- site[site$inside==TRUE,]
#write.csv(site.bkk, file = "/Users/TLump_MAC/Thesis/Data/AIS/siteBKK.csv")


plot(map.bkk)
points(site,col="blue")
points(site[site$inside==TRUE,],col="red")






