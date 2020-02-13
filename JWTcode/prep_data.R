### Barebones iSSA prep code ====
# Julie Turner
# Started: July 1 2019


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'raster', 'tidyr', 'ggplot2') # you might need lubridate? pretty sure the code that requires it isn't here, but if something doesn't work, you might need it
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/' # folder I store raw data here
derived <- 'data/derived-data/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here


### example wolf data ----
dat <- fread(paste0(raw, 'RMNPwolf_rarified.csv')) #my data
#'amt' needs a date time in this format with the date and time all in one
dat$datetime <- paste(dat$gmtDate, dat$gmtTime)
# making sure my newly created datetime is the right str
dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M:%S") 

### setting my crs ----
utm14N <- "+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs14 <- sp::CRS("+init=epsg:32614")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat %>% dplyr::select(x = "X", y = "Y", t = 'datetime', id = "WolfID") 


# prepping structure for amt (they really like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_all <- DT.prep %>% group_by(id) %>% nest()

#making the track
dat_all <- dat_all %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32614")) 
  }))  

#summary of track sampling rate for each individual
dat_all %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))


#### layers ####
# loading my landcover dist to roads rasters
land <- raster(paste0(raw, 'RMNPlandcover_wgs84.tif'), )
roads <- raster(paste0(raw, 'roadall_LinFeat_dist.tif'))


#### making steps from track and extracting covariates ####
ssa <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(120), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% amt::random_steps(n=10) %>%
      amt::extract_covariates(land, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
      amt::extract_covariates(roads, where = "both") %>%
      amt::time_of_day(include.crepuscule = F, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate(land_start = factor(RMNPlandcover_wgs84_start, levels = 1:7, labels = c("coniferous", 'deciduous', "mixed", 'shrub', "open", 'wet', 'urban')), # renaming the extracted landcover types so I know what they are instead of the numbers that are in the raster
             land_end = factor(RMNPlandcover_wgs84_end, levels = 1:7, labels = c("coniferous", 'deciduous', "mixed", 'shrub', "open", 'wet', 'urban')), # you have to do this for the start and end point since we extracted both
             log_sl = log(sl_), # adding log transformed SL *DO THIS*
             cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))


# unnest the data
ssa.all <- ssa %>% dplyr::select(id, steps) %>% unnest(cols = c(steps))

# Save extracted data
# saveRDS(ssa.all, 'data/derived-data/ssaAll.Rds')



