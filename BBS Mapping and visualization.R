## This uploads & combines the data from the fifty-stop, route-level dataset


library(maps)
library(mapdata)
library(RCurl)

## Upload route file
routes <- read.csv("Princeton/Socolar - Washburne/BBS and Land Use Changes/Data/BBS Data and Documents/routes.csv")
# Need to replace this with the URL on our github file

## Demo for how to plot on maps. map() needs to be done first to create the map object, on which we can attach points and, eventually, polygons for land-use change
map("worldHires",c("USA","Canada"), xlim=c(-170,-50), ylim=c(25,80), col="gray90", fill=TRUE)
points(routes$Longi[routes$Active==1], routes$Lati[routes$Active==1],pch=19, col="blue",cex=0.5)
points(routes$Longi[routes$Active==0], routes$Lati[routes$Active==0],pch=19, col="red",cex=0.5)
points(routes$Longi[routes$countrynum==840 & routes$statenum==60], routes$Lati[routes$countrynum==840 & routes$statenum==60],pch=19, col="yellow",cex=0.5)


## Function to obtain the route location given its route, country, state combo
RouteLoc <- function(routes,route,country,state){
  # Function to obtain a matrix of [longitude,latitude] for a given set of routes with labels route, country=countrynum, state=statenum
  Lat=routes$Lati[is.element(routes$countrynum,country) & is.element(routes$statenum,state) & is.element(routes$Route,route)]
  Long=routes$Longi[is.element(routes$countrynum,country) & is.element(routes$statenum,state) & is.element(routes$Route,route)]
  return(matrix(c(Long,Lat),length(Long),2))
}



## e.g. a demo - find the routes that contained Baltimore orioles, AOU = 5070, in 1978.

# Extract route data for the species + year
spp=5070
country=good_data$countrynum.x[good_data$year==1978 & good_data$AOU==spp & good_data$Abundance>0]
state=good_data$statenum.x[good_data$year==1978 & good_data$AOU==spp & good_data$Abundance > 0]
route=good_data$Route.x[good_data$year==1978 & good_data$AOU==spp & good_data$Abundance > 0]

# Use our RouteLoc function to obtain coordinates and plot those coordinates in green
Loc=RouteLoc(routes,route,country,state)
map("worldHires",c("USA","Canada"), xlim=c(-170,-50), ylim=c(25,80), col="gray90", fill=TRUE)
points(routes$Longi[routes$Active==1], routes$Lati[routes$Active==1],pch=19, col="blue",cex=0.5)
points(Loc[,1],Loc[,2], pch=19, col='green', cex=0.8)




## Now we need a function to determine if a given point (or polygon) 
## is within a distance of L meters from a route.
library(fields)

## The distance matrix (in meters) between all points in the vector, Loc, is

DLoc=rdist.earth(Loc,Loc,miles=F,R=NULL)/1000

# However, the rdist.earth function is picky - it requires matrices for Loc. Thus, if comparing only one point to many, we will have to use rbind(Loc,Loc)
# i.e. the distance between point 5 and points 20 and 21 is

rdist.earth(rbind(Loc[5,],Loc[5,]),rbind(Loc[20,],Loc[21,]),miles=F,R=NULL)[1,]/1000

## Now, let's check out the land-use change datasets to see what form they're in!!
