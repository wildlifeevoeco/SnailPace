### SNAIL iSSA PREP ###

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'raster', 'tidyr', 'ggplot2') # you might need lubridate
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/' # folder I store raw data here
derived <- 'Data/derived/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

### data ----
dat <- fread(paste0(raw, 'SnailDataUTM.csv')) #my data
dat$Time <- paste(dat$Time, "00", sep=":")
#'amt' needs a date time in this format with the date and time all in one
dat$datetime <- paste(dat$Date, dat$Time)
# making sure my newly created datetime is the right str
dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M:%S") 
dat[,'x'] <- round(dat$x_cm + 370194, 2)
dat[,'y'] <- round(dat$y_cm + 5268492, 2)

set.seed(57)
### setting my crs ----
utm22T <- "+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs22 <- sp::CRS("+init=epsg:32621")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat %>% 
  dplyr::select(x = "x", y = "y", t = 'datetime', snail = 'Snail', temp = "Temperature",
                precip = "Precipitation", treatment = "Treatment", stage = "Stage") 
saveRDS(DT.prep, paste0(derived, 'baseSnail.RDS'))