##ENCLOSURE EDGE

# Load raster package
library(raster)

# Set projection
proj <- '+proj=utm +zone=22 ellps=WGS84'

# Generate empty raster
xmin <- 370194
ymin <- 5268492
edgelength <- 65 + 1
r1 <- raster(
  ncol = edgelength,
  nrow = edgelength,
  ymn = ymin,
  ymx = ymin + edgelength,
  xmn = xmin,
  xmx = xmin + edgelength,
  crs = CRS(proj),
  resolution = 1
)

# Fill all cells with NA (grass)
r1[] <- NA


# Edges to 1
rowcells <- cellFromRow(r1, row = c(1, edgelength))
colcells <- cellFromCol(r1, col = c(1, edgelength))
r1[rowcells] <- 1
r1[colcells] <- 1


## Plotting options
# base r
plot(r1)

# raster package
spplot(r1)
plot(r1)
## Distance to
gdist1 <- distance(r1)
plot(gdist1)
writeRaster(gdist1, "edgedist.tif", overwrite=TRUE)
## BRICK EDGES ##

## Treatment 1 ##
gdist1 <- gridDistance(r1, origin=1, omit=NULL)
plot(gdist1)
writeRaster(gdist1, "brickedge1.tif")

## Treatment 2 ##
gdist2 <- gridDistance(r2, origin=1, omit=NULL)
plot(gdist2)
writeRaster(gdist2, "brickedge2.tif")

##Treatment 3 ##
gdist3 <- gridDistance(r3, origin=1, omit=NULL)
plot(gdist3)
writeRaster(gdist3, "brickedge3.tif")


