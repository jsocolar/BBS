library(dismo)
library(rgdal)
library(maps)
library(mapdata)
library(maptools)
gpclibPermit()

basemap <- gmap("United States", type="satellite")
plot(basemap)

BBSroutes <- readOGR(dsn="/Users/Jacob/Downloads/bbsrtes_2012_alb", 
                     layer="bbsrte_2012_alb")
