### SNAIL iSSA PREP ###

#### CONTROL ####

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'raster', 'tidyr', 'ggplot2') # you might need lubridate
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/Honours stuff/Snails/Data/raw/' # folder I store raw data here
derived <- '~/Honours stuff/Snails/Data/derived/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

### data ----
dat <- fread(paste0(raw, 'SnailDataUTM.csv')) #my data
#'amt' needs a date time in this format with the date and time all in one
dat$datetime <- paste(dat$Date, dat$Time)
# making sure my newly created datetime is the right str
dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 

### setting my crs ----
utm22T <- "+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs22 <- sp::CRS("+init=epsg:32621")

# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep <- dat %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature",
                                 precip = "Precipitation", treatment = "Treatment", stage = "Stage") 

# prepping structure for amt (they really like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_all <- DT.prep %>% group_by(snail) %>% nest()
#dat.p14a <-DT.prep [snail=="P14a"]
#making the track
dat_all <- dat_all %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  
#dat.p14a <- amt::make_track(dat.p14a, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  
#summary of track sampling rate for each individual
dat_all %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))

# dat_all <- dat_all %>% mutate(steps = map(trk, function(x) {
#   x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
#     amt::filter_min_n_burst(min_n = 3) %>%
#     amt::steps_by_burst() 
#   }))

#summarize_sampling_rate(dat.p14a)
#### layers ####
# load proximity rasters
edge <- raster(paste0(raw, 'edgedist.tif'), )
brickedge1 <- raster(paste0(raw, 'brickedge1.tif'), )
brickedge2 <- raster(paste0(raw, 'brickedge2.tif'), )
brickedge3 <- raster(paste0(raw, 'brickedge3.tif'), )


# dat_all$steps
#### making steps from track and extracting covariates ####
# ssa <- mutate(steps = map(steps, function(x){
#   x %>%  amt::random_steps(n=10)# %>%
# }))
#      
    # amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
    #   amt::extract_covariates(brickedge1, where = "both") %>%
    #   amt::extract_covariates(brickedge2, where = "both") %>%
    #   amt::extract_covariates(brickedge3, where = "both") %>%
    #   # amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
    #   #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
    #   mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
    #           cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
    # 

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
    amt::extract_covariates(brickedge1, where = "both") %>%
    amt::extract_covariates(brickedge2, where = "both") %>%
    amt::extract_covariates(brickedge3, where = "both") 
  })




ssa <- dat_all %>%
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


#DT <- amt::make_track(DT.prep, x, y, t, crs = sp::CRS("+init=epsg:32614")) # %>%
# amt::transform_coords(sp::CRS("+init=epsg:26914")) # may not need to transform
#summarize_sampling_rate(DT)
# 
# # making steps from track
# stps <- amt::track_resample(dat.p14a, rate = minutes(30), tolerance = minutes(10)) %>%
#   filter_min_n_burst(min_n = 3) %>% steps_by_burst() #%>%
#   #time_of_day(include.crepuscule = T) # need to fix this because missing dusk (KK?)
# 
# quantile(stps$sl_)
# 
# #### exploratory checks ####
# 
# stps.parkdist <- stps %>% extract_covariates(parkbound_dist, where = "both") %>%
#   mutate(lnparkdist_start = log(boundary_dist_start + 1)) %>%
#   mutate(lnparkdist_end = log(boundary_dist_end + 1))
# 
# 
# stps.park <- stps %>% extract_covariates(parkYN, where = "both") %>%
#   mutate(parkYN_start = factor(parkYN_start, levels = c(0, 1), labels = c("park", "outside park"))) %>%
#   mutate(parkYN_end = factor(parkYN_end, levels = c(0, 1), labels = c("park", "outside park")))
# 
# p1.park <- stps.park %>% dplyr::select(parkYN_end, tod = tod_end_, sl_, ta_) %>%
#   
#   tidyr::gather(key, val, -parkYN_end, -tod) %>%
#   dplyr::filter(key == "sl_") %>%
#   ggplot(., aes(val, group = tod, fill = tod)) + geom_density(alpha = 0.5) +
#   facet_wrap(~ parkYN_end, nrow = 2) +
#   xlab("Step length [m]") + theme_light() +
#   ylab("Density") +
#   theme(legend.title = element_blank())
# 
# stps.wet <- stps %>% extract_covariates(wet, where = "both") %>%
#   mutate(wet_start = factor(wet_start, levels = c(0,1), labels = c('other', 'wet'))) %>%
#   mutate(wet_end = factor(wet_end, levels = c(0,1), labels = c('other', 'wet')))
# 
# 
# stps.closed <- stps %>% extract_covariates(closed, where = "both") %>%
#   mutate(closed_start = factor(closed_start, levels = c(0,1), labels = c('other', 'closed'))) %>%
#   mutate(closed_end = factor(closed_end, levels = c(0,1), labels = c('other', 'closed')))
# 
# stps.land <- stps %>% extract_covariates(land, where = "both") %>%
#   mutate(land_start = factor(RMNPlandcover_start, levels = 1:3, labels = c("closed", "open", 'wet'))) %>%
#   mutate(land_end = factor(RMNPlandcover_end, levels = 1:3, labels = c("closed", "open", 'wet')))
# 
# p1.land <- stps.land %>% dplyr::select(land_end, tod = tod_end_, sl_, ta_) %>%
#   
#   tidyr::gather(key, val, -land_end, -tod) %>%
#   filter(key == "sl_") %>%
#   ggplot(., aes(val, group = tod, fill = tod)) + geom_density(alpha = 0.5) +
#   facet_wrap(~ land_end, nrow = 2) +
#   xlab("Step length [m]") + theme_light() +
#   ylab("Density") +
#   theme(legend.title = element_blank())
# 
# 
# 
# 
# 
# #### extract covariate for SSF ####
# ssf1 <- stps %>% random_steps(n = 10) %>%
#   amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
#   amt::extract_covariates(brickedge1, where = "both") %>%
#   amt::extract_covariates(brickedge2, where = "both") %>%
#   amt::extract_covariates(brickedge3, where = "both") %>%
#   amt::time_of_day(include.crepuscule = T, where = 'start') %>%
#   mutate(log_sl = log(sl_),
#          cos_ta = cos(ta_))


# unnest the data
ssa.all <- ssa %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))

track_unnest <- track %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))

# Save extracted data
#saveRDS(ssa.all, '~/Honours stuff/Snails/Data/derived/ssaAllTreats.Rds')#, overwrite=T)

#XXX<-merge(ssa.all, DT.prep[,.(snail, t, temp, precip)], by.x=c('snail','t2_'), by.y= c('snail', 't'), all.x=T)


sum.sl<-setDT(track_unnest)[,.(stepn=uniqueN(t1_), nas=sum(is.na(sl_)), mean=mean(sl_, na.rm=T), min = min(sl_, na.rm=T), max=max(sl_, na.rm=T), median = median(sl_, na.rm=T)), by= .(snail)]
sum.sl[,'diff'] <- sum.sl$stepn-sum.sl$nas



### 30 steps ###

snail.30 <- sum.sl[diff>=30]
dat.30 <- dat[Snail %chin% snail.30$snail]

dat.30$datetime <- paste(dat.30$Date, dat.30$Time)
# making sure my newly created datetime is the right str
dat.30$datetime <- as.POSIXct(dat.30$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 


# selecting just the parts of the data that I need, you'll probably need to include your other things like temp and precip
DT.prep.30 <- dat.30 %>% dplyr::select(x = "xUTM", y = "yUTM", t = 'datetime', snail = 'Snail', temp = "Temperature",
                                 precip = "Precipitation", treatment = "Treatment", stage = "Stage") 

# prepping structure for amt (they really like tidyvers, and it's kind of annoyingly dependent on it)
# nesting data by id
dat_all.30 <- DT.prep.30 %>% group_by(snail) %>% nest()
#dat.p14a <-DT.prep [snail=="P14a"]
#making the track
dat_all.30 <- dat_all.30 %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:32621")) 
  }))  
#dat.p14a <- amt::make_track(dat.p14a, x, y, t, crs = sp::CRS("+init=epsg:32621")) 

#summary of track sampling rate for each individual
dat_all.30 %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(snail, sr) %>% unnest(cols = c(sr))

# dat_all <- dat_all %>% mutate(steps = map(trk, function(x) {
#   x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
#     amt::filter_min_n_burst(min_n = 3) %>%
#     amt::steps_by_burst() 
#   }))

#summarize_sampling_rate(dat.p14a)
#### layers ####
# load proximity rasters
edge <- raster(paste0(raw, 'edgedist.tif'), )
brickedge1 <- raster(paste0(raw, 'brickedge1.tif'), )
brickedge2 <- raster(paste0(raw, 'brickedge2.tif'), )
brickedge3 <- raster(paste0(raw, 'brickedge3.tif'), )


# dat_all$steps
#### making steps from track and extracting covariates ####
# ssa <- mutate(steps = map(steps, function(x){
#   x %>%  amt::random_steps(n=10)# %>%
# }))
#      
# amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
#   amt::extract_covariates(brickedge1, where = "both") %>%
#   amt::extract_covariates(brickedge2, where = "both") %>%
#   amt::extract_covariates(brickedge3, where = "both") %>%
#   # amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
#   #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
#   mutate( log_sl = log(sl_), # adding log transformed SL *DO THIS*
#           cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
# 

track.30 <- dat_all.30 %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() %>% 
      mutate(sl_ = sl_ + 1)
  }))


ssa <-lapply(track.30$steps, function(x){
  x %>% amt::random_steps(n=10)%>% 
  amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
    amt::extract_covariates(brickedge1, where = "both") %>%
    amt::extract_covariates(brickedge2, where = "both") %>%
    amt::extract_covariates(brickedge3, where = "both") 
})


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
saveRDS(ssa.30.unnest, '~/Honours stuff/Snails/Data/derived/ssa30.Rds')
