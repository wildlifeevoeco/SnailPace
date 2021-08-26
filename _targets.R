# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code by Alec L. Robitaille


# Packages ----------------------------------------------------------------
library(targets)

library(amt)
library(data.table)
library(sf)
library(sp)
library(ggplot2)
library(glmmTMB)
library(raster)
library(broom.mixed)

# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'workspace')


# Variables ---------------------------------------------------------------
path <- file.path('Data', 'raw', 'SnailDataUTM.csv')
# load proximity rasters
raw <- file.path('Data', 'raw') # folder I store raw data here
derived <- file.path('Data', 'derived') #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

edge <- raster(file.path(raw, 'edgedist.tif'))
brickedge1 <- raster(file.path(raw, 'brickedge1.tif'))
brickedge2 <- raster(file.path(raw, 'brickedge2.tif'))
brickedge3 <- raster(file.path(raw, 'brickedge3.tif'))


id <- 'snail'
datetime <- 't'
longlat = FALSE
#not actually longitude and latitude, just don't want to change code
long <- 'x'
lat <- 'y'
crs <- CRS(st_crs(32621)$wkt)


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id


# Resampling rate 
rate <- hours(1)

# Tolerance
tolerance <- minutes(2)

# columns to rename
# oldname <- c('becomes')
# newname <- c('lc_end')

# build iSSA 
# response <- 'case_'
# explanatory <- "I(log(sl_)) + I(log(sl_)):tod_start_ + 
#   lc_end +lc_end:I(log(sl_)) +
#   (1|indiv_step_id) +
#   (0|I(log(sl_)):id) + (0:I(log(sl_)):tod_start_:id) +
#   (0|lc_end:id) + (0|lc_end:I(log(sl_)):id)"



# Targets: prep -----------------------------------------------------------
targets_prep <- c(
  # Read input data
  tar_target(
    input,
    fread(path)
  ),
  
  # Prep columns
  tar_target(
    prepared,
    prep_cols(input)
  ),
  
  # Remove duplicated and incomplete observations
  tar_target(
    mkunique,
    make_unique_complete(prepared, id, datetime, long, lat)
  )
)



# Targets: splits ---------------------------------------------------------
targets_splits <- c(
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    mkunique[, tar_group := .GRP, by = splitBy],
    iteration = 'group'
  ),
  
  tar_target(
    splitsnames,
    unique(mkunique[, .(path = path), by = splitBy])
  )
)

# Targets: iSSA -----------------------------------------------------------
targets_issa <- c(
  # Make tracks. Note from here on, when we want to iterate use pattern = map(x)
  #  where x is the upstream target name
  tar_target(
    tracks,
    make_track(splits, x, y, t, crs = crs, id = snail),
    pattern = map(splits)
  ),
  
  # Resample sampling rate
  tar_target(
    resamples,
    resample_tracks(tracks, rate, tolerance),
    pattern = map(tracks)
  ),
  
  # create random steps and extract covariates
  tar_target(
    randsteps,
    make_random_steps(resamples, brickedge1, brickedge2, brickedge3, edge),
    pattern = map(resamples)
  ),
  
  # create step ID across individuals
  tar_target(
    stepID,
    setDT(randsteps)[,indiv_step_id := paste(id, step_id_, sep = '_')],
    pattern = map(resamples)
  ),
  
  # Merge prep data back
  tar_target(
    mergeprep,
    merge_steps(stepID, splits),
    pattern = map(stepID, splits)
  )
)



# Targets: treatments -----------------------------------------------------
targets_treatments <- c(
  # tar_target(
  #   filterNAs,
  #   mergeprep[!is.na(ghostbrick)]
  # ),
  
  tar_target(
    bricktreats,
    brick_treatments(
      mergeprep
    )
  ),
  tar_target(
    controltreats,
    control_treatments(
      mergeprep
    )
  ),
  tar_target(
    combtreats,
    # TODO: move ghostbricks into combine treatments?
    combine_treatments(bricktreats, controltreats)
  )
)



# Targets: model ----------------------------------------------------------
targets_models <- c(
  tar_target(
    modelp1,
    model_p1(combtreats),
    iteration = 'list'
  ),
  tar_target(
    tidymodelp1,
    tidy_model(modelp1, effect = 'ran_vals')
  ),
  
  tar_target(
    modelp3,
    model_p3(combtreats),
    iteration = 'list'
  ),
  
  tar_target(
    tidymodelp3,
    tidy_model(modelp3, effect = 'ran_vals')
  )
)



# Targets: speed ----------------------------------------------------------
targets_speed <- c(
  tar_target(
    cleaned_names,
    clean_model_names(tidymodelp3),
    pattern = map(tidymodelp3)
  ),
  
  tar_target(
    coefs,
    tidy_coefs(cleaned_names, distparams),
    pattern = map(cleaned_names, distparams)
  ),
  
  tar_target(
    predict_seq,
    make_predict_seq(combtreats, tidy(modelp3)),
    pattern = map(combtreats, modelp3)
  ),
  
  tar_target(
    predicted_speed,
    predict_speed(coefs, predict_seq),
    pattern = map(coefs, predict_seq)
  ),
  
  tar_target(
    plotted_speed_brick,
    plot_speed_brick(predicted_speed),
    pattern = map(predicted_speed)
  ),
  
  tar_target(
    plotted_speed_edge,
    plot_speed_edge(predicted_speed),
    pattern = map(predicted_speed)
  )
)



# Targets: RSS ------------------------------------------------------------
targets_rss <- c(
  tar_target(
    setfactors,
    set_factors(combtreats)
  ),
  
  tar_target(
    predictmeans,
    predict_means(combtreats, modelp1)
    # predict_means(combtreats[subset with i?], modelp1)
  ),
  
  # TODO: need to fix error of mismatching lengths
  #       id_treat seems to have length of 11
  #       this is possibly fixed by adjusting what the by= is
  #       within these functions
  tar_target(
    predictbricks,
    predict_brickdist(combtreats, modelp1)
    # predict_brickdist(combtreats[subset with i?], modelp1)
  ),
  tar_target(
    predictedges,
    predict_edgedist(combtreats, modelp1)
    # predict_edgedist(combtreats[subset with i?], modelp1)
  ),
  tar_target(
    rss,
    calc_rss(predictedges, predictbricks, predictmeans)
  ) 
)





# Targets: distributions --------------------------------------------------
targets_distributions <- c(
  # Check step distributions
  #  iteration = 'list' used for returning a list of ggplots,
  #  instead of the usual combination with vctrs::vec_c()
  tar_target(
    distributions,
    ggplot(resamples, aes(sl_)) + geom_density(alpha = 0.4),
    pattern = map(resamples),
    iteration = 'list'
  ),
  # Distribution parameters
  tar_target(
    distparams,
    calc_distribution_parameters(randsteps),
    pattern = map(randsteps)
  )
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)

# This is equivalent to 
# c(targets_distribution, targets_issa, ...)
# Because remember - the _targets.R file always needs to end 
# in a list of targets tar_target objects
