##TREATMENT 1

# Load raster package
library(raster)

# Set projection
# TODO: switch this to your proj4string
proj <- '+proj=utm +zone=22 ellps=WGS84'

# Generate empty raster
extent <-
  r1 <- raster(ncol = 65, nrow = 65,
              ymn = 5268492, ymx = 5268557,
              xmn = 370194, xmx =  370259,
              crs = CRS(proj), resolution = 1)

# Fill all cells with 0 (grass)
r1[] <- 0

# Brick cells
xbrick1 <- seq(370239, 370259 , by = 1)
ybrick1 <- seq(5268547, 5268557, by = 1)

# Expand the two sequence of x, y to all combinations and cast as matrix
xybrick1 <- as.matrix(expand.grid(xbrick1, ybrick1))


cells <- cellFromXY(r1, xybrick1)
r1[cells] <- 1


## Plotting options
# base r
plot(r1)

# raster package
spplot(r1)


#########

##TREATMENT 2

# Load raster package
library(raster)

# Set projection
# TODO: switch this to your proj4string
proj <- '+proj=utm +zone=22 ellps=WGS84'

# Generate empty raster
extent <-
  r2 <- raster(ncol = 65, nrow = 65,
              ymn = 5268492, ymx = 5268557,
              xmn = 370194, xmx =  370259,
              crs = CRS(proj), resolution = 1)
#writeRaster(r2, "raster2")
# Fill all cells with 0 (grass)
r2[] <- 0

# Brick cells
xbrick2 <- seq(370239, 370259 , by = 1)
ybrick2 <- seq(5268537, 5268557, by = 1)

# Expand the two sequence of x, y to all combinations and cast as matrix
xybrick2 <- as.matrix(expand.grid(xbrick2, ybrick2))


cells <- cellFromXY(r2, xybrick2)
r2[cells] <- 1


## Plotting options
# base r
plot(r2)

# raster package
spplot(r2)


#########

##TREATMENT 3

# Load raster package
library(raster)

# Set projection
# TODO: switch this to your proj4string
proj <- '+proj=utm +zone=22 ellps=WGS84'

# Generate empty raster
extent <-
  r3 <- raster(ncol = 65, nrow = 65,
              ymn = 5268492, ymx = 5268557,
              xmn = 370194, xmx =  370259,
              crs = CRS(proj), resolution = 1)
#writeRaster(r3, "raster3")
# Fill all cells with 0 (grass)
r3[] <- 0

# Brick cells
xbrick3 <- seq(370219, 370259 , by = 1)
ybrick3 <- seq(5268537, 5268557, by = 1)

# Expand the two sequence of x, y to all combinations and cast as matrix
xybrick3 <- as.matrix(expand.grid(xbrick3, ybrick3))


cells <- cellFromXY(r3, xybrick3)
r3[cells] <- 1


## Plotting options
# base r
plot(r3)

# raster package
spplot(r3)

#########

##CONTROL

# Load raster package
library(raster)

# Set projection
# TODO: switch this to your proj4string
proj <- '+proj=utm +zone=22 ellps=WGS84'

# Generate empty raster
extent <-
  r4 <- raster(ncol = 65, nrow = 65,
              ymn = 5268492, ymx = 5268557,
              xmn = 370194, xmx =  370259,
              crs = CRS(proj), resolution = 1)
#writeRaster(r4, "raster4")
# Fill all cells with 0 (grass)
r4[] <- 0


## Plotting options
# base r
plot(r4)

# raster package
spplot(r4)


