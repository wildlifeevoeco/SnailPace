### SNAIL iSSA PREP ###

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'raster', 'tidyr', 'ggplot2') # you might need lubridate
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/snails/Data/raw/' # folder I store raw data here
derived <- '~/snails/Data/derived/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

### data ----
dat <- fread(paste0(raw, 'SnailDataUTM.csv')) #my data
dat$Time <- paste(dat$Time, "00", sep=":")
#'amt' needs a date time in this format with the date and time all in one
dat$datetime <- paste(dat$Date, dat$Time)
# making sure my newly created datetime is the right str
dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M:%S") 

### setting my crs ----
utm22T <- "+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs22 <- sp::CRS("+init=epsg:32621")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature",
                                 precip = "Precipitation", treatment = "Treatment", stage = "Stage") 

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
brickedge1 <- raster(paste0(raw, 'brickedge1.tif'), )
brickedge2 <- raster(paste0(raw, 'brickedge2.tif'), )
brickedge3 <- raster(paste0(raw, 'brickedge3.tif'), )

track <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% 
      mutate(sl_ = sl_ + 1)
  }))


### Find snails with less than 30 steps missing ###


track_unnest <- track %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))
sum.sl<-setDT(track_unnest)[,.(stepn=uniqueN(t1_), nas=sum(is.na(sl_)), mean=mean(sl_, na.rm=T), min = min(sl_, na.rm=T), max=max(sl_, na.rm=T), median = median(sl_, na.rm=T)), by= .(snail)]
sum.sl[,'diff'] <- sum.sl$stepn-sum.sl$nas

snail.30 <- sum.sl[diff>=30]
dat.30 <- dat[Snail %chin% snail.30$snail]

# dat.30$datetime <- paste(dat.30$Date, dat.30$Time)
# dat.30$datetime <- as.POSIXct(dat.30$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 


# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep.30 <- dat.30 %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature",
                                       precip = "Precipitation", treatment = "Treatment", stage = "Stage") 

# nesting data by id
dat_all.30 <- DT.prep.30 %>% group_by(snail) %>% nest()

#making the track
dat_all.30 <- dat_all.30 %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  


#summary of track sampling rate for each individual
dat_all.30 %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))


track.30 <- dat_all.30 %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% 
      mutate(sl_ = sl_ + 1)
  }))

ssa.30 <- track.30 %>%
  mutate(randsteps = map(steps, function(x) {   
    x %>% amt::random_steps(n=10) %>%
      amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
      amt::extract_covariates(brickedge1, where = "both") %>%
      amt::extract_covariates(brickedge2, where = "both") %>%
      amt::extract_covariates(brickedge3, where = "both") %>%
      # amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
              cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

ssa.30.unnest <- ssa.30 %>% dplyr::select(snail, randsteps) %>% unnest(cols = c(randsteps))

merged.snails <-merge(ssa.30.unnest, DT.prep.30,
                      by.x=c('snail','t1_', 'x1_', 'y1_'), by.y= c('snail', 't', 'x','y'))


#saveRDS(ssa.30.unnest, '~/snails/Data/derived/ssa30.Rds')

### finding and removing duplicates
DT.prep.30[,.(len=length(t), unique=uniqueN(t)), by=.(snail)]

duplicated(dat[Trial==2, .(Time), by=.(Snail)])

t2snails <- dat[Trial==2, unique(Snail)]

t2<-DT.prep.30[snail %in% t2snails]

t2[(duplicated(t)),.(t), by=.(snail)]

badsnails <- t2[, .N, by = .(snail, t)][N > 1]

DT.prep.good <- DT.prep.30[-c(4379, 4811, 5243, 5675, 6107, 6539, 6971, 7403, 7835, 8267, 8699, 9131),]
           