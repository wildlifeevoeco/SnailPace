################################################################
################################################################
######                                                  ########
######           2 - iSSA MODELS BY WOLF                ########
######                                                  ########
################################################################
################################################################

gc()

############################
##### 1 - Load packages ----

libs <- c('lubridate', 'amt', 'data.table', 'tidyr', 'raster')
lapply(libs, require, character.only=TRUE)

###############################
#### 2 - Load spatial data ----

# landcover layers

ag_cov <- raster('data/layers/habitat/Agriculture.tif')
names(ag_cov) <- 'agriculture'

conbog_cov <- raster('data/layers/habitat/Con_Bog.tif')
names(conbog_cov) <- 'coniferous'

decid_cov <- raster('data/layers/habitat/Dec_OpenDec.tif')
names(decid_cov) <- 'deciduous'

open_cov <- raster('data/layers/habitat/Marsh_Grass.tif')
names(open_cov) <- 'open'

mw_cov <- raster('data/layers/habitat/Mixedwood.tif')
names(mw_cov) <- 'mixedwood'

#water_cov <- raster('data/layers/habitat/Water.tif')
#names(water_cov) <- 'water'

# combine landcover into raster brick
# habitat_cov <- brick(ag_cov, conbog_cov, decid_cov, open_cov, mw_cov)
# summary(habitat_cov)

  ###make a buffer and calculate proportion
buff90 <- focalWeight(mw_cov, d=90, type = 'circle')

ag_buff90 <- focal(ag_cov, buff90, na.rm = TRUE, pad = TRUE, padValue = 0)
names(ag_buff90) <- 'agriculture'
con_buff90 <- focal(conbog_cov, buff90, na.rm = TRUE, pad = TRUE, padValue = 0)
names(con_buff90) <- 'coniferous'
dec_buff90 <- focal(decid_cov, buff90, na.rm = TRUE, pad = TRUE, padValue = 0)
names(dec_buff90) <- 'deciduous'
opn_buff90 <- focal(open_cov, buff90, na.rm = TRUE, pad = TRUE, padValue = 0)
names(opn_buff90) <- 'open'
mix_buff90 <- focal(mw_cov, buff90, na.rm = TRUE, pad = TRUE, padValue = 0)
names(mix_buff90) <- 'mixedwood'

# combine buffers
hab_buff90 <- brick(ag_buff90, con_buff90, dec_buff90, opn_buff90, mix_buff90)

#habitat edge
hab_edge<- raster('data/layers/habitat/Edge_D.tif')
names(hab_edge) <- 'hab_dist'

###distance to  water
water<- raster('data/layers/habitat/Water_Dist.tif')
names(water) <- 'wat_dist'

# #selection
# elk_rsf<- raster('data/layers/PreyRSFs/Elk_habitat.tif')
# names(elk_rsf) <- 'elk_hab'
# moose_rsf<- raster('data/layers/PreyRSFs/Moose_habitat.tif')
# names(moose_rsf) <- 'moose_hab'

#vulnerability
elk_ks<- raster('data/layers/PreyRSFs/Elk_vulnerability.tif')
names(elk_ks) <- 'elk_kill'
moose_ks<- raster('data/layers/PreyRSFs/Moose_vulnerability.tif')
names(moose_ks) <- 'moose_kill'

###distance to trails
dist_trail = raster('data/layers/distHuman/All_trails_Dist.tif')
names(dist_trail) <- 'trail_dist'

# distance to park boundary
dist_rmnp <-raster('data/layers/distHuman/park_dist.tif')

# distance to road raster
dist_road <-raster('data/layers/distHuman/road_dist.tif')

#distance to range edge
# (make a vector with all the pack abbreviations)
packs <- c('AD','BD','BL','BT','GL','RC','SL', 'WW')
for(i in packs){
  j <- raster(paste0('data/layers/distWR/', paste('distance_raster_', '.tif', sep=i)))
  assign(paste0('distance_raster_', i, sep=''), j)
}

################################
#### 2 - Load location data ----

#### - Load location data and subset ----

# load the data
wolves <- fread("data/wolf_locs/mergedKloc-locdataAll-CCv31 - R300H96 - Test-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx.csv")
wolves$gmtDateTime <- as.POSIXct(paste(wolves$gmtDate, wolves$gmtTime), tz = "GMT", format="%Y-%m-%d %H:%M:%S")
wolves <- wolves[Month == 1 |  Month == 2 | Month ==  3 |  Month == 11 | Month == 12]
wolves$WolfID = as.factor(wolves$WolfID )
wolves$PackID = as.factor(wolves$PackID)


# Make a vector of all unique wolf IDs
wolf_IDs <- c('W01', 'W02','W03','WO4','W05','W06','W07','W08','W09','W10','W11','W12','W13','W14','W16','W19','W21','W22','W24','W25','W26', 'W27')

#########################################
#### 3 - Clean NAs and add ID column ----

dat <- wolves %>% 
  # remove NAs
  filter(!is.na('X')) %>%
  dplyr::select(x='X', y='Y', t = 'gmtDateTime', id = 'WolfID', pack = 'PackID', tfkill = 'tfkill')


######################################################
#### 4 - Subset dates for either winter or summer ----

# make datetime column POSIX
dat$t <- as.POSIXct(dat$t, tz = 'America/Winnipeg')

##########################################################
#### 4 - Loop through to save track for each wolf-year ----


for(i in wolf_IDs){
  tryCatch({
    print(i)
  
      dat_1 <- dat %>% filter(id==i)
        
    
    # A) Make track with x, y, time
   # dat_1 <- mk_track(dat_1, .x=x, .y=y, .t=t, tfkill=tfkill, pack=pack, crs = sp::CRS("+init=epsg:26914"))
    
   
    # B) Specify which pack density raster to use
    
    # choose raster:

    if(dat_1$pack=='AD'){
      wolf_dist <- distance_raster_AD
    }
    if(dat_1$pack=='BD'){
      wolf_dist <- distance_raster_BD
    }
    if(dat_1$pack=='BT'){
      wolf_dist <- distance_raster_BT
    }
    if(dat_1$pack=='BL'){
      wolf_dist <- distance_raster_BL
    }
    if(dat_1$pack=='GL'){
      wolf_dist <- distance_raster_GL
    }
    if(dat_1$pack=='RC'){
      wolf_dist <- distance_raster_RC
    }
    if(dat_1$pack=='SL'){
      wolf_dist <- distance_raster_SL
    }
    if(dat_1$pack=='WW'){
      wolf_dist <- distance_raster_WW
    }
    
    # rename raster values to common value
    names(wolf_dist) <- 'wolf_dist'
    
    # dat_1 <- dat_1 %>% 
    #   # remove NAs
    #   filter(!is.na('X')) %>%
    #   select(x='X', y='Y', t = 'gmtDateTime', id = 'WolfID', pack = "PackID", tfkill = 'tfkill')
    # # make datetime column POSIX
    # dat_1$t <- as.POSIXct(dat_1$t)
    # make track
    dat_1 <- mk_track(dat_1, .x=x, .y=y, .t=t, pack = pack, tfkill = tfkill, crs = sp::CRS("+init=epsg:26914"))
    
 #### 4 - Resample track ----
    
    # check sampling rate to look for spread of resampling rates
    summarize_sampling_rate(dat_1, time_unit = 'hour')
    # resample track to constant fix rate with a tolerance around the fix rate
    # to accommodate differences in timing of fix rate
    # fix rate of 2 hours with a tolerance of 10 minutes
    stps <- track_resample(dat_1, rate=hours(2), tolerance=minutes(10)) %>%
      # filter to bursts with at least 3 consecutive points (required to calculate TA)
      filter_min_n_burst(min_n=3) %>% steps_by_burst(keep_cols = 'start') %>%
      # calculates whether the location was taken at day, night, dawn, dusk
      time_of_day(include.crepuscule=F, where='both')
    
    
    ###########################################################
    ###########################################################
    ####                 F) RUN MODELS                      ###
    ###########################################################
    ###########################################################
    
    
  ################################
  ######## CORE  ########
  
        

     m0 <-  stps %>% random_steps(n=10) %>%
      extract_covariates(hab_buff90, where='both') %>%
      extract_covariates(hab_edge, where='both') %>%
      extract_covariates(water, where='both') %>%
      extract_covariates(wolf_dist, where='both') %>%
      extract_covariates(elk_ks, where='both') %>%
      extract_covariates(moose_ks, where='both') %>%
      extract_covariates(dist_trail, where='both') %>%
      extract_covariates(dist_rmnp, where='both') %>%
      extract_covariates(dist_road, where='both') %>%
      time_of_day(include.crepuscule=FALSE) %>%
      mutate(log_sl_=log(sl_+1)) %>%
      mutate(cos_ta_=cos(ta_)) %>%
      mutate(log_hab_dist_end=log(hab_dist_end+1)) %>%
      mutate(log_wat_dist_end=log(wat_dist_end+1)) %>%
      mutate(log_trail_dist_end=log(trail_dist_end+1)) %>%
      mutate(log_park_dist_end=log(park_dist_end+1)) %>%
      mutate(log_road_dist_end=log(road_dist_end+1)) %>%
      mutate(in_park_dist_end=(1/(park_dist_end+1))) %>%
      mutate(log_wolf_dist_end=(log(wolf_dist_end+1))) %>%
     # na.omit() %>%
      fit_issf(case_ ~
                 log_sl_  +
                 cos_ta_ +
                 #agriculture_end +
                 coniferous_end +
                 deciduous_end +
                 open_end +
                 mixedwood_end +
                 log_wat_dist_end +
                 log_hab_dist_end +
                 log_trail_dist_end +
                 # log_wolf_dist_end +
                 # elk_kill_end +
                 # moose_kill_end +
                 # log_park_dist_end +
                 strata(step_id_))


    # Tidy output
    tidycore <- broom::tidy(m0$model, exponentiate=FALSE, conf.int=TRUE)

    # Save the output

    pack <- dat_1$pack[1]

    fwrite(tidycore, file = paste0("output/models/core/", paste(i, pack, sep = "m0")))
    
    ################################
    ######## 1 - Conspecific ########

    m1 <- stps %>% random_steps(n=10) %>%
      extract_covariates(hab_buff90, where='both') %>%
      extract_covariates(hab_edge, where='both') %>%
      extract_covariates(water, where='both') %>%
      extract_covariates(wolf_dist, where='both') %>%
      extract_covariates(elk_ks, where='both') %>%
      extract_covariates(moose_ks, where='both') %>%
      extract_covariates(dist_trail, where='both') %>%
      extract_covariates(dist_rmnp, where='both') %>%
      extract_covariates(dist_road, where='both') %>%
      time_of_day(include.crepuscule=FALSE) %>%
      mutate(log_sl_=log(sl_+1)) %>%
      mutate(cos_ta_=cos(ta_)) %>%
      mutate(log_hab_dist_end=log(hab_dist_end+1)) %>%
      mutate(log_wat_dist_end=log(wat_dist_end+1)) %>%
      mutate(log_trail_dist_end=log(trail_dist_end+1)) %>%
      mutate(log_park_dist_end=log(park_dist_end+1)) %>%
      mutate(log_road_dist_end=log(road_dist_end+1)) %>%
      mutate(in_park_dist_end=(1/(park_dist_end+1))) %>%
      mutate(log_wolf_dist_end=(log(wolf_dist_end+1))) %>%
      fit_issf(case_ ~
                 log_sl_  +
                 cos_ta_ +
                # agriculture_end +
                 coniferous_end +
                 deciduous_end +
                 open_end +
                 mixedwood_end +
                 log_wat_dist_end +
                 log_hab_dist_end +
                 log_trail_dist_end +
                 log_wolf_dist_end +
                 log_wolf_dist_end:tfkill +
                 strata(step_id_))

    # Tidy output
    tidycore <- broom::tidy(m1$model, exponentiate=FALSE, conf.int=TRUE)

    # Save the output

    pack <- dat_1$pack[1]

    fwrite(tidycore, file = paste0("output/models/cons/", paste(i, pack, sep = "m1")))
    ###each model in its own folder

    ################################
    ###### 2 - PREY #######


    m2 <- stps %>% random_steps(n=10) %>%
      extract_covariates(hab_buff90, where='both') %>%
      extract_covariates(hab_edge, where='both') %>%
      extract_covariates(water, where='both') %>%
      extract_covariates(wolf_dist, where='both') %>%
      extract_covariates(elk_ks, where='both') %>%
      extract_covariates(moose_ks, where='both') %>%
      extract_covariates(dist_trail, where='both') %>%
      extract_covariates(dist_rmnp, where='both') %>%
      extract_covariates(dist_road, where='both') %>%
      time_of_day(include.crepuscule=FALSE) %>%
      mutate(log_sl_=log(sl_+1)) %>%
      mutate(cos_ta_=cos(ta_)) %>%
      mutate(log_hab_dist_end=log(hab_dist_end+1)) %>%
      mutate(log_wat_dist_end=log(wat_dist_end+1)) %>%
      mutate(log_trail_dist_end=log(trail_dist_end+1)) %>%
      mutate(log_park_dist_end=log(park_dist_end+1)) %>%
      mutate(log_road_dist_end=log(road_dist_end+1)) %>%
      mutate(in_park_dist_end=(1/(park_dist_end+1))) %>%
      mutate(log_wolf_dist_end=(log(wolf_dist_end+1))) %>%
      mutate(norm_elk_kill_end = ((elk_kill_end-0.0000001)/(0.3294261-0.0000001))) %>%
      mutate(norm_moose_kill_end = ((moose_kill_end-0.0004475)/(0.1951060-0.0004475))) %>%
      fit_issf(case_ ~
                 log_sl_  +
                 cos_ta_ +
                # agriculture_end +
                 coniferous_end +
                 deciduous_end +
                 open_end +
                 mixedwood_end +
                 log_wat_dist_end +
                 log_hab_dist_end +
                 log_trail_dist_end +
                 norm_elk_kill_end + 
                 norm_elk_kill_end:tfkill +
                 norm_moose_kill_end + 
                 norm_moose_kill_end:tfkill + 
                 strata(step_id_))


    # Tidy output
    tidycore <- broom::tidy(m2$model, exponentiate=FALSE, conf.int=TRUE)

    # Save the output

    pack <- dat_1$pack[1]

    fwrite(tidycore, file = paste0("output/models/prey/", paste(i, pack, sep = "m2")))
    ###each model in its own folder

    # #########################################
    # ###### 3 - Human #######


    m3 <- stps %>% random_steps(n=10) %>%
      extract_covariates(hab_buff90, where='both') %>%
      extract_covariates(hab_edge, where='both') %>%
      extract_covariates(water, where='both') %>%
      extract_covariates(wolf_dist, where='both') %>%
      extract_covariates(elk_ks, where='both') %>%
      extract_covariates(moose_ks, where='both') %>%
      extract_covariates(dist_trail, where='both') %>%
      extract_covariates(dist_rmnp, where='both') %>%
      extract_covariates(dist_road, where='both') %>%
      time_of_day(include.crepuscule=FALSE) %>%
      mutate(log_sl_=log(sl_+1)) %>%
      mutate(cos_ta_=cos(ta_)) %>%
      mutate(log_hab_dist_end=log(hab_dist_end+1)) %>%
      mutate(log_wat_dist_end=log(wat_dist_end+1)) %>%
      mutate(log_trail_dist_end=log(trail_dist_end+1)) %>%
      mutate(log_park_dist_end=log(park_dist_end+1)) %>%
      mutate(log_road_dist_end=log(road_dist_end+1)) %>%
      mutate(log_wolf_dist_end=(log(wolf_dist_end+1))) %>%
      fit_issf(case_ ~
                 log_sl_  +
                 cos_ta_ +
                 #agriculture_end +
                 coniferous_end +
                 deciduous_end +
                 open_end +
                 mixedwood_end +
                 log_wat_dist_end +
                 log_hab_dist_end +
                 log_trail_dist_end +
                 log_park_dist_end +
                 log_park_dist_end:tfkill +
                 strata(step_id_))


    # Tidy output
    tidycore <- broom::tidy(m3$model, exponentiate=FALSE, conf.int=TRUE)

    # Save the output

    pack <- dat_1$pack[1]

    fwrite(tidycore, file = paste0("output/models/human/", paste(i, pack, sep = "m3")))

    #########################################
    ###### 4 - Move #######

    m4 <- stps %>% random_steps(n=10) %>%
      extract_covariates(hab_buff90, where='both') %>%
      extract_covariates(hab_edge, where='both') %>%
      extract_covariates(water, where='both') %>%
      extract_covariates(wolf_dist, where='both') %>%
      extract_covariates(elk_ks, where='both') %>%
      extract_covariates(moose_ks, where='both') %>%
      extract_covariates(dist_trail, where='both') %>%
      extract_covariates(dist_rmnp, where='both') %>%
      extract_covariates(dist_road, where='both') %>%
      time_of_day(include.crepuscule=FALSE) %>%
      mutate(log_sl_=log(sl_+1)) %>%
      mutate(cos_ta_=cos(ta_)) %>%
      mutate(log_hab_dist_end=log(hab_dist_end+1)) %>%
      mutate(log_wat_dist_end=log(wat_dist_end+1)) %>%
      mutate(log_trail_dist_end=log(trail_dist_end+1)) %>%
      mutate(log_park_dist_end=log(park_dist_end+1)) %>%
      mutate(log_road_dist_end=log(road_dist_end+1)) %>%
      mutate(in_park_dist_end=(1/(park_dist_end+1))) %>%
      mutate(log_wolf_dist_end=(log(wolf_dist_end+1))) %>%
      fit_issf(case_ ~
                 log_sl_  +
                 cos_ta_ +
                 #agriculture_end +
                 coniferous_end +
                 deciduous_end +
                 open_end +
                 mixedwood_end +
                 log_wat_dist_end +
                 log_hab_dist_end +
                 log_trail_dist_end +
                 log_sl_:tfkill +
                 cos_ta_:tfkill +
                 strata(step_id_))


    # Tidy output
    tidycore <- broom::tidy(m4$model, exponentiate=FALSE, conf.int=TRUE)

    # Save the output

    pack <- dat_1$pack[1]

    fwrite(tidycore, file = paste0("output/models/move/", paste(i, pack, sep = "m4")))
    
    ##extract movement parameters
    SL = sl_distr_params(m4)
    shape =  SL$shape
    scale =  SL$scale
    
    TA = ta_distr_params(m4)
    kappa =TA$kappa
    mu = TA$mu
    
    move_par  = data.table(shape,scale,kappa,mu)
    
    fwrite(move_par, file = paste0("output/models/distr_params/", paste(i, pack, sep = "m4params")))


     #############################
     ###### 5 - FULL MODEL #######

    m5 <- stps %>% random_steps(n=10) %>%
      extract_covariates(hab_buff90, where='both') %>%
      extract_covariates(hab_edge, where='both') %>%
      extract_covariates(water, where='both') %>%
      extract_covariates(wolf_dist, where='both') %>%
      extract_covariates(elk_ks, where='both') %>%
      extract_covariates(moose_ks, where='both') %>%
      extract_covariates(dist_trail, where='both') %>%
      extract_covariates(dist_rmnp, where='both') %>%
      extract_covariates(dist_road, where='both') %>%
      time_of_day(include.crepuscule=FALSE) %>%
      mutate(log_sl_=log(sl_+1)) %>%
      mutate(cos_ta_=cos(ta_)) %>%
      mutate(log_hab_dist_end=log(hab_dist_end+1)) %>%
      mutate(log_wat_dist_end=log(wat_dist_end+1)) %>%
      mutate(log_trail_dist_end=log(trail_dist_end+1)) %>%
      mutate(log_park_dist_end=log(park_dist_end+1)) %>%
      mutate(log_road_dist_end=log(road_dist_end+1)) %>%
      mutate(in_park_dist_end=(1/(park_dist_end+1))) %>%
      mutate(log_wolf_dist_end=(log(wolf_dist_end+1))) %>%
      mutate(norm_elk_kill_end = ((elk_kill_end-0.0000001)/(0.3294261-0.0000001))) %>%
      mutate(norm_moose_kill_end = ((moose_kill_end-0.0004475)/(0.1951060-0.0004475))) %>%
      fit_issf(case_ ~
                 log_sl_  +
                 log_sl_:tfkill +
                 cos_ta_ +
                 cos_ta_:tfkill +
                 #agriculture_end +
                 coniferous_end +
                 deciduous_end +
                 open_end +
                 mixedwood_end +
                 log_wat_dist_end +
                 log_hab_dist_end +
                 log_trail_dist_end +
                 norm_elk_kill_end + 
                 norm_elk_kill_end:tfkill +
                 norm_moose_kill_end + 
                 norm_moose_kill_end:tfkill + 
                 log_wolf_dist_end +
                 log_wolf_dist_end:tfkill +
                 log_park_dist_end +
                 log_park_dist_end:tfkill +
                 strata(step_id_))

     # Tidy output
      tidycore <- broom::tidy(m5$model, exponentiate=FALSE, conf.int=TRUE)

    # Save the output

     pack <- dat_1$pack[1]

     fwrite(tidycore, file = paste0("output/models/full/", paste(i, pack, sep = "m5")))
     
     

  }, error=function(e){cat("ERROR:", conditionMessage(e), "\n")})}

