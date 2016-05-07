library("rgdal")
library("rgdal")
library("maptools")
library("gridExtra")

readMap <- function(){
  map <- readOGR(dsn="/Users/TLump_MAC/Thesis/Data/tam_geo",layer = "tam_geo");
  return (map[map$PROV_NAME=="BANGKOK",])
}

createBKKPolygons <- function(map.bkk){
  map.coords <- coordinates(map.bkk)
  oregon.id <- cut(map.coords[,1],quantile(map.coords[,1], c(0,1)), include.lowest=TRUE)
  map.unionBKK <- unionSpatialPolygons(map.bkk,oregon.id)
  proj4string(map.unionBKK) <- proj4string(map.bkk)
  return(map.unionBKK)
}

toSpatialPoints <- function(site,proj,inBKKOnly){
   
  coordinates(site) <- ~ Longitude + Latitude
  # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
  proj4string(site) <- proj
  
  if (inBKKOnly == TRUE){
    site.sp <- site
    site.sp$inside  <-!is.na(over(site.sp,as(map.bkk, "SpatialPolygons")))
    site <- site.sp[site.sp$inside==TRUE,]  
  }
  
  return (site)
}
  
voronoiBbox <- function(site){
  library('dismo')
  library('deldir')
  return(dismo::voronoi(site))
}

vorinoiInBKK <- function(vor.bbox,map.bkk){  
  
  map.unionBKK <- createBKKPolygons(map.bkk)
  
  #create all polygon
  vor.polygon <- map.unionBKK + vor.bbox
  
  library("rgeos")
  vor.polygon.Centroids = gCentroid(vor.polygon,byid=TRUE)
  vor.polygon$inside  <-!is.na(over(vor.polygon.Centroids,as(map.bkk, "SpatialPolygons")))
  vor.bkk <-vor.polygon[vor.polygon@data$inside==TRUE,] 
  return(vor.bkk)
}


# main
# read map
map.bkk <- readMap()
proj <- proj4string(map.bkk)

# calculate agent of cluster
site.agent <- aggregate(site,by =  list(site$cluster),FUN = mean)
#site.agent <- aggregate(site.BKK.kmean,by =  list(site.BKK.kmean$cluster),FUN = mean)
#site.agent <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/sitecluster_KMEAN_mean.csv",header = TRUE,sep =',')

site.agent <- toSpatialPoints(site = site.agent,proj = proj,inBKKOnly = F)

#create voronoi
vor.bbox <- voronoiBbox(site.agent)

# select voronoi in BKK
vor.bkk <- vorinoiInBKK(vor.bbox,map.bkk)
rm(vor.bbox)

# show
plotInterActiveMap(vor.bkk,site.agent,site)

