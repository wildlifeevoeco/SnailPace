#### TREATMENT 1 ####

library(readxl)
library(tidyverse)
UTMTreat1 <- read_excel("~/Honours stuff/Snails/UTMTreat1.xlsx")

##Drop rows with NA values
Treat1 <- drop_na(UTMTreat1)

##Create spatial points 
xy1 <-Treat1[,c(10,11)]
p1 <- SpatialPointsDataFrame(coords = xy1, data = Treat1,
                             proj4string = CRS("+proj=utm +zone=22 ellps=WGS84"))  
##Calculate distance from points   
r.d1 <- distanceFromPoints(r1, p1) 

plot(r.d1)
spplot(r.d1)

writeRaster(r.d1, "proxraster_treat1")
write.csv(p1, "pointsdata_treat1.csv")
write.csv(r.d@data@values, "proxraster1.csv")


#### TREATMENT 2 ####

UTMTreat2 <- read_excel("Honours stuff/Snails/UTMTreat2.xlsx")

##Drop rows with NA values
Treat2 <- drop_na(UTMTreat2)

##Create spatial points 
xy2 <-Treat2[,c(10,11)]
p2 <- SpatialPointsDataFrame(coords = xy2, data = Treat2,
                             proj4string = CRS("+proj=utm +zone=22 ellps=WGS84"))  
##Calculate distance from points   
r.d2 <- distanceFromPoints(r2, p2) 

plot(r.d2)
spplot(r.d2)

writeRaster(r.d2, "proxraster_treat2")
write.csv(p2, "pointsdata_treat2.csv")
write.csv(r.d2@data@values, "proxraster2.csv")

#### TREATMENT 3 ####

UTMTreat3 <- read_excel("Honours stuff/Snails/UTMTreat3.xlsx")

##Drop rows with NA values
Treat3 <- drop_na(UTMTreat3)

##Create spatial points 
xy3 <-Treat3[,c(10,11)]
p3 <- SpatialPointsDataFrame(coords = xy3, data = Treat3,
                             proj4string = CRS("+proj=utm +zone=22 ellps=WGS84"))  
##Calculate distance from points   
r.d3 <- distanceFromPoints(r3, p3) 

plot(r.d3)
spplot(r.d3)

writeRaster(r.d3, "proxraster_treat3")
write.csv(p3, "pointsdata_treat3.csv")
write.csv(r.d3@data@values, "proxraster3.csv")

#### CONTROL ####

UTMControl <- read_excel("~/Honours stuff/Snails/UTMControl.xlsx")

##Drop rows with NA values
Control <- drop_na(UTMControl)

##Create spatial points 
xy4 <-Control[,c(10,11)]
p4 <- SpatialPointsDataFrame(coords = xy4, data = Control,
                             proj4string = CRS("+proj=utm +zone=22 ellps=WGS84"))  
##Calculate distance from points   
r.d4 <- distanceFromPoints(r4, p4) 

plot(r.d4)
spplot(r.d4)

writeRaster(r.d4, "proxraster_control")
write.csv(p4, "pointsdata_control.csv")
write.csv(r.d4@data@values, "proxrastercontrol.csv")

### Maybe????
#Try gridDistance
gdist1 <- gridDistance(r1, origin=1, omit=NULL)
plot(gdist1)
writeRaster(gdist1, "gdist1")


#Try rasterToPolygon
r1poly <- rasterToPolygons(r1, dissolve=T)
plot(r1poly)

dist <- st_distance(r1, p1)
