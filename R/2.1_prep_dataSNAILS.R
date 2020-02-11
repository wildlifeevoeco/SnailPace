### SNAIL iSSA PREP ###

#### CONTROL ####

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'raster', 'tidyr', 'ggplot2') # you might need lubridate
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/Honours stuff/Snails/Data/raw/' # folder I store raw data here
derived <- '~/Honours stuff/Snails/Data/derived/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

### data ----
dat <- fread(paste0(raw, 'UTMControlcsv.csv')) #my data
#'amt' needs a date time in this format with the date and time all in one
dat$datetime <- paste(dat$Date, dat$Time)
# making sure my newly created datetime is the right str
dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 

### setting my crs ----
utm22T <- "+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs22 <- sp::CRS("+init=epsg:32621")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature", precip = "Precip") 

# prepping structure for amt (they really like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_all <- DT.prep %>% group_by(snail) %>% nest()

#making the track
dat_all <- dat_all %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  

#summary of track sampling rate for each individual
dat_all %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))


#### layers ####
# load proximity rasters
edge <- raster(paste0(raw, 'edgedist.tif'), )
#plot(edge)
#### making steps from track and extracting covariates ####
ssa <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% amt::random_steps(n=10) %>%
      amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
      amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
        cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

# unnest the data
ssa.all <- ssa %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))


# Save extracted data
saveRDS(ssa.all, '~/Honours stuff/Snails/Data/derived/ssaAll.Rds')#, overwrite=T)

#### TREATMENT 1 ####

### data ----
dat <- fread(paste0(raw, 'SnailDataUTM.csv')) #my data
dat.treat1 <- dat[Treatment=="1"]

dat.treat1$Time.s <- paste(dat.treat1$Time, "00", sep = ":")
#'amt' needs a date time in this format with the date and time all in one
dat.treat1$datetime <- paste(dat.treat1$Date, dat.treat1$Time.s)
# making sure my newly created datetime is the right str
dat.treat1$datetime <- as.POSIXct(dat.treat1$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 

### setting my crs ----
utm22T <- "+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs22 <- sp::CRS("+init=epsg:32621")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat.treat1 %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature", precip = "Precipitation") 

# prepping structure for amt (they really like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_all <- DT.prep %>% group_by(snail) %>% nest()

#making the track
dat_all <- dat_all %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  

#summary of track sampling rate for each individual
dat_all %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))


#### layers ####
# load proximity rasters
edge <- raster(paste0(raw, 'edgedist.tif'), )
brick <- raster(paste0(raw, 'brickedge1.tif'), )
#### making steps from track and extracting covariates ####


track <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% 
      mutate(sl_ = sl_ + 1)
  }))


lapply(track$steps, function(x){
  x %>% amt::random_steps(n=10)
  amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
  amt::extract_covariates(brickedge1, where = "both")
})




ssa <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% amt::random_steps(n=10) %>%
      amt::extract_covariates(edge, where = "end")  %>%
      amt::extract_covariates(brick, where = "end") %>% # both indicates you want the covariate at the start and end of the step
      amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
              cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

# unnest the data
ssa.all <- ssa %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))


# Save extracted data
#saveRDS(ssa.all, '~/Honours stuff/Snails/Data/derived/ssaAll.Rds')#, overwrite=T)


#### TREATMENT 2 ####


### data ----
dat.treat2 <- dat[Treatment=="2"]

dat.treat2$Time.s <- paste(dat.treat2$Time, "00", sep = ":")
#'amt' needs a date time in this format with the date and time 2 in one
dat.treat2$datetime <- paste(dat.treat2$Date, dat.treat2$Time.s)
# making sure my newly created datetime is the right str
dat.treat2$datetime <- as.POSIXct(dat.treat2$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 


# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep2 <- dat.treat2 %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature", precip = "Precipitation") 

# prepping structure for amt (they re2y like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_2 <- DT.prep2 %>% group_by(snail) %>% nest()

#making the track2
dat_2 <- dat_2 %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  

#summary of track2 sampling rate for each individual
dat_2 %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))


#### layers ####
# load proximity rasters
brick2 <- raster(paste0(raw, 'brickedge2.tif'), )
#### making steps from track2 and extracting covariates ####


track2 <- dat_2 %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% 
      mutate(sl_ = sl_ + 1)
  }))



lapply(track2$steps, function(x){
  x %>% amt::random_steps(n=10) %>% 
  amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
  amt::extract_covariates(brick2, where = "both")
})




ssa <- dat_2 %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% amt::random_steps(n=10) %>%
      amt::extract_covariates(edge, where = "end")  %>%
      amt::extract_covariates(brick, where = "end") %>% # both indicates you want the covariate at the start and end of the step
      amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
              cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

# unnest the data
ssa.2 <- ssa %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))

#### TREATMENT 3 ####

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'raster', 'tidyr', 'ggplot2') # you might need lubridate
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/Honours stuff/Snails/Data/raw/' # folder I store raw data here
derived <- '~/Honours stuff/Snails/Data/derived/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

### data ----
dat <- fread(paste0(raw, 'SnailDataUTM.csv')) #my data
dat.treat1 <- dat[Treatment=="4"]

dat.treat1$Time.s <- paste(dat.treat1$Time, "00", sep = ":")
#'amt' needs a date time in this format with the date and time all in one
dat.treat1$datetime <- paste(dat.treat1$Date, dat.treat1$Time.s)
# making sure my newly created datetime is the right str
dat.treat1$datetime <- as.POSIXct(dat.treat1$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 

### setting my crs ----
utm22T <- "+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs22 <- sp::CRS("+init=epsg:32621")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat.treat1 %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature", precip = "Precipitation") 

# prepping structure for amt (they really like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_all <- DT.prep %>% group_by(snail) %>% nest()

#making the track
dat_all <- dat_all %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  

#summary of track sampling rate for each individual
dat_all %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))


#### layers ####
# load proximity rasters
edge <- raster(paste0(raw, 'edgedist.tif'), )
brick <- raster(paste0(raw, 'brickedge2.tif'), )
#### making steps from track and extracting covariates ####


track <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% 
      mutate(sl_ = sl_ + 1)
  }))



amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
  amt::extract_covariates(brick, where = "both")
})




ssa <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% amt::random_steps(n=10) %>%
      amt::extract_covariates(edge, where = "end")  %>%
      amt::extract_covariates(brick, where = "end") %>% # both indicates you want the covariate at the start and end of the step
      amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
              cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

# unnest the data
ssa.all <- ssa %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))
