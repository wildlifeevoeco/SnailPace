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
 ##################33



DT.sum.hr <- DT.prep[t %like% ':30:' & stage != 'Acc', .(disturbance = unique(ifelse(treatment %like% 'C', 'undisturbed', 'disturbed')), 
                                                         treatment = unique(treatment), nTotal = .N, nSteps = .SD[!(is.na(x)), .N]), by=.(snail)]
#saveRDS(DT.sum.hr, paste0(derived, 'summary_steps.RDS'))

DT.prep.hr <- DT.prep[t %like% ':30:' & !(is.na(x))]
stp.snails <- DT.prep.hr[!(is.na(x)),.(nSteps =uniqueN(t)), by=.(snail)]
stp.snails.30 <- stp.snails[nSteps >=30, snail]

DT.prep.2hr <- DT.prep.hr[seq(1, length(t), 2)]
stp.snails.2hr <- DT.prep.2hr[!(is.na(x)),.(nSteps =uniqueN(t)), by=.(snail)]
stp.snails.30.2hr <- stp.snails.2hr[nSteps >=10, snail]

DT.prep.hr <- DT.prep.hr[snail %chin% stp.snails.30]
DT.prep.2hr <- DT.prep.2hr[snail %chin% stp.snails.30.2hr & !(is.na(x))]

# nesting data by id
dat_all <- DT.prep.hr %>% group_by(snail) %>% nest()

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
    x %>% amt::track_resample(rate = hours(1), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst() 
  }))

track <- track %>%
  mutate(randsteps = map(steps, function(x) {
    x %>% amt::random_steps(n = 10, sl_distr = fit_distr(.$sl_, 'exp'))
  }))
### Find snails with less than 30 steps missing ###
#amt::random_steps(x=track_unnest$sl_, sl_distr = fit_distr(.$sl_, sl_distr))

track_unnest <- track %>% dplyr::select(snail, steps) %>% unnest(cols = c(steps))
sum.sl<-setDT(track_unnest)[,.(stepn=uniqueN(t1_), nas=sum(is.na(sl_)), mean=mean(sl_, na.rm=T), min = min(sl_, na.rm=T), max=max(sl_, na.rm=T), median = median(sl_, na.rm=T)), by= .(snail)]
sum.sl[,'diff'] <- sum.sl$stepn-sum.sl$nas

snail.30 <- sum.sl[diff>=30]
snail.20 <- sum.sl[diff>=20]
DT.prep.30 <- DT.prep.hr[snail %chin% snail.30$snail]
DT.prep.30 <- DT.prep.2hr[snail %chin% snail.20$snail]




### finding and removing duplicates
DT.prep.30[,.(len=length(t), unique=uniqueN(t)), by=.(snail)]



# getting SL and TA distributions
SLdistr <- function(x.col, y.col, date.col, crs, ID, sl_distr, ta_distr) {
  #print(ID)
  #create track from dataset
  trk <- track(x.col, y.col, date.col, ID, crs) %>%
    #function turns locs into steps
    steps()
  #remove any steps that span more than 2hr
  trk$dt_ <- difftime(trk$t2_, trk$t1_, unit='hours')
  trk <- subset(trk, trk$dt_ > 0.9 & trk$dt_ < 1.1, drop = T) ### make sure time is right
  #generate random steps
  trk %>%
    random_steps(sl_distr = fit_distr(.$sl_, sl_distr)) %>%
    sl_distr_params()
}

TAdistr <- function(x.col, y.col, date.col, crs, ID, sl_distr, ta_distr) {
  #print(ID)
  #create track from dataset
  trk <- track(x.col, y.col, date.col, ID, crs) %>%
    #function turns locs into steps
    steps()
  #remove any steps that span more than 2hr
  trk$dt_ <- difftime(trk$t2_, trk$t1_, unit='hours')
  trk <- subset(trk, trk$dt_ > 0.9 & trk$dt_ < 1.1, drop = T)
  #generate random steps
  trk %>%
    random_steps(sl_distr = fit_distr(.$sl_, sl_distr)) %>%
    ta_distr_params()
}

#run function by ID
DT.prep.30[,unique(snail)]
bad <-c('P13a', 'O13a', 'P24a','O24a', 'P31a', 'O31a') # good 1hr
# bad <- c('P12a', 'P13a', 'P14a', 'O14a', 'O24b', 'O31a') # good 2hr
slParams.gamma <- DT.prep.30[!(snail %in% bad), {
  print(.BY[[1]])
  SLdistr(x.col = x, y.col = y, date.col = t, crs = utm22T, ID = snail, 
          sl_distr = "gamma", ta_distr = "vonmises")},
  by = snail]

taParams.gamma <- DT.prep.30[!(snail %in% bad), {
  print(.BY[[1]])
  TAdistr(x.col = x, y.col = y, date.col = t, crs = utm22T, ID = snail, 
          sl_distr = "gamma", ta_distr = "vonmises")},
  by = snail]

Params.gamma <- merge(slParams.gamma, taParams.gamma[,.(snail,kappa)], by = 'snail')

slParams.exp <- DT.prep.30[!(snail %in% bad), {
  print(.BY[[1]])
  SLdistr(x.col = x, y.col = y, date.col = t, crs = utm22T, ID = snail, 
                                sl_distr = "exp", ta_distr = "vonmises")},
                  by = snail]

taParams.exp <- DT.prep.30[!(snail %in% bad), {
  print(.BY[[1]])
  TAdistr(x.col = x, y.col = y, date.col = t, crs = utm22T, ID = snail, 
          sl_distr = "exp", ta_distr = "vonmises")},
                         by = snail]

Params.exp <- merge(slParams.exp, taParams.exp[,.(snail,kappa)], by = 'snail')

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
    x %>% amt::track_resample(rate = hours(1), tolerance = minutes(10)) %>% # make sure time frame is right
      amt::filter_min_n_burst(min_n = 3) %>%
      amt::steps_by_burst()
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
      mutate( log_sl = log(sl_ + 1 ), # adding log transformed SL *DO THIS*
              cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

ssa.exp <- track.30 %>%
  mutate(randsteps = map(steps, function(x) {   
    x %>% amt::random_steps(n=10, sl_distr = fit_distr(.$sl_, "exp")) %>%
      amt::extract_covariates(edge, where = "both")  %>% # both indicates you want the covariate at the start and end of the step
      amt::extract_covariates(brickedge1, where = "both") %>%
      amt::extract_covariates(brickedge2, where = "both") %>%
      amt::extract_covariates(brickedge3, where = "both") %>%
      # amt::time_of_day(t, where = 'start') %>%  # calc day/night ToD, only for start of the step
      #you propbably won't need to do the land_start/_end renaming since you don't have any categorical rasters, but just in case some go on the brick, and you want to know if they're on the brick or not?
      mutate( log_sl = log(sl_ + 1), # adding log transformed SL *DO THIS*
              cos_ta = cos(ta_)) # adding cos transformed TA *DO THIS*
  }))

ssa.30.unnest <- ssa.30 %>% dplyr::select(snail, randsteps) %>% unnest(cols = c(randsteps))

ssa.exp.unnest <- ssa.exp %>% dplyr::select(snail, randsteps) %>% unnest(cols = c(randsteps))

merged.snails <-merge(ssa.30.unnest, DT.prep.30,
                      by.x=c('snail','t2_'), by.y= c('snail', 't'))

merged.snails.exp <-merge(ssa.exp.unnest, DT.prep.30,
                      by.x=c('snail','t2_'), by.y= c('snail', 't'))



saveRDS(merged.snails, 'Data/derived/ssa-gam-goods.Rds')

saveRDS(Params.gamma, 'Data/derived/moveParams-gam-goods.Rds')

saveRDS(merged.snails.exp, 'Data/derived/ssa-exp-goods.Rds')

saveRDS(Params.exp, 'Data/derived/moveParams-exp-goods.Rds')

### 2 hr
saveRDS(merged.snails, 'Data/derived/ssa-gam2hr-goods.Rds')

saveRDS(Params.gamma, 'Data/derived/moveParams-gam2hr-goods.Rds')

saveRDS(merged.snails.exp, 'Data/derived/ssa-exp2hr-goods.Rds')

saveRDS(Params.exp, 'Data/derived/moveParams-exp2hr-goods.Rds')

