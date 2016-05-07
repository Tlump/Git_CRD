#Clustering Large Applications
clusLargeAppSite <- function(coors){
  site <-  subset(coors,select=c(Longitude,Latitude))
  library (cluster)
  # Medoids
  #y <- pam(site,1500)
  clara.out <- cluster::clara(site,1500)
  site$cluster <- clara.out$cluster
  
  site <- subset(site,select=c("clusters","Longitude","Latitude"))
  site <- site[order(site$clusters,site$Longitude,site$Latitude),]
  return (site)
}

# Hierarchical Clustering
hClusSite <- function(coors){
  library("fields")
  dist <- rdist.earth(coors,miles = False,R=6371)

  fit <- hclust(as.dist(dist), method = "single")
  threshold.in.km <- 0.3
  site  <- coors
  site$cluster <- cutree(fit,h = threshold.in.km)
  
  site <- subset(site,select=c("cluster","Longitude","Latitude"))
  site <- site[order(site$clusters,site$Longitude,site$Latitude),]
  return (site)
}

#Kmean Clustering
kmeansSite <- function(coors,k){
  site <-  subset(coors,select=c(Longitude,Latitude))
  kmeans.out <- kmeans(site, k)
  site$cluster <- kmeans.out$cluster
  
  site <- subset(site,select=c("cluster","Longitude","Latitude"))
  site <- site[order(site$cluster,site$Longitude,site$Latitude),]
  return (site)
}

kmeansWithin <- function(coors,min,max){
  data <- subset(site.bkk,select=c(Longitude,Latitude))
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in min:max) wss[i] <- sum(kmeans(data,centers=i,iter.max = 10000)$withinss)
  plot(1:max, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

#############################################################################################
site.BKK.bbox <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/AIS/site_BKK-bbox_AllSocial.csv",header = TRUE,sep =',')

site.BKK.kmean <- kmeansSite(site,50)
rm(site.BKK.bbox)


kmeansWithin(site.bkk,2,50)


#write.csv(x, file = "/Users/TLump_MAC/Thesis/Data/AIS/sitesitecluster_clara.csv")