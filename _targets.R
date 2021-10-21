# === Snails --------------------------------------------------------------



# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               workspace_on_error = TRUE)



# Data --------------------------------------------------------------------
# Path to raw, derived directories
raw <- file.path('Data', 'raw')
derived <- file.path('Data', 'derived')


# Snail movement
path <- file.path(raw, 'SnailDataUTM.csv')


# Proximity rasters
edge <- raster(file.path(raw, 'edgedist.tif'))
brickedge1 <- raster(file.path(raw, 'brickedge1.tif'))
brickedge2 <- raster(file.path(raw, 'brickedge2.tif'))
brickedge3 <- raster(file.path(raw, 'brickedge3.tif'))



# Variables ---------------------------------------------------------------
# Columns
id <- 'snail'
datetime <- 't'
x <- 'x'
y <- 'y'

# CRS
crs <- CRS(st_crs(32621)$wkt)

# Column to split analysis by
splitBy <- id

## resample_tracks(): 
# resampling rate 
rate <- hours(1)
# tolerance rate 
tolerance <- minutes(2)

# Levels
treatment_levels <- c('C', '1', '2', '4')
stage_levels <- c('B', 'A')
ghostbrick_levels <- c('g1', 'g2', 'g3', '1', '2', '3')

# TODO: not needed?
# longlat <- FALSE
# columns to rename
# oldname <- c('becomes')
# newname <- c('lc_end')
# build iSSA 
# response <- 'case_'
# explanatory <- 'I(log(sl_)) + I(log(sl_)):tod_start_ + 
#   lc_end +lc_end:I(log(sl_)) +
#   (1|indiv_step_id) +
#   (0|I(log(sl_)):id) + (0:I(log(sl_)):tod_start_:id) +
#   (0|lc_end:id) + (0|lc_end:I(log(sl_)):id)'



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
    make_unique_complete(prepared, id, datetime, x, y)
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
  ## Make tracks 
  # Note from here on, when we want to iterate use pattern = map(x)
  # where x is the upstream target name
  tar_target(
    tracks,
    make_track(splits, x, y, t, crs = crs, id = snail),
    pattern = map(splits)
  ),
  
  ## Resample sampling rate
  # Regular
  tar_target(
    resamples,
    resample_tracks(tracks, rate, tolerance, binomial = FALSE),
    pattern = map(tracks)
  ),
  # Binomial move/not
  tar_target(
    binomial_resamples,
    resample_tracks(tracks, rate, tolerance, binomial = TRUE),
    pattern = map(tracks)
  ),
  
  ## Create random steps and extract covariates
  # Regular
  tar_target(
    randsteps,
    make_random_steps(resamples, 45, brickedge1, brickedge2, brickedge3, edge),
    pattern = map(resamples)
  ),

  
  ## Create step ID across individuals
  # Regular
  tar_target(
    step_id,
    make_step_id(randsteps),
    pattern = map(randsteps)
  ),

  
  ## Merge prep data back
  # Regular
  tar_target(
    merge_prep,
    merge_steps(step_id, splits, limit_edge = TRUE),
    pattern = map(step_id, splits)
  ),
  # Binomial
  tar_target(
    binomial_merge_prep,
    merge_steps(binomial_resamples, splits, limit_edge = FALSE),
    pattern = map(binomial_resamples, splits)
  ),
  
  # Prep binomial for model
  tar_target(
    prep_binomial,
    prep_model_binomial(binomial_merge_prep)
  )
)



# Targets: treatments -----------------------------------------------------
targets_treatments <- c(
  tar_target(
    brick_treats,
    brick_treatments(
      merge_prep
    )
  ),
  tar_target(
    control_treats,
    control_treatments(
      merge_prep
    )
  ),
  tar_target(
    combined_treatments,
    bind_treatments(brick_treats, control_treats)
  )
)



# Targets: model ----------------------------------------------------------
targets_models <- c(
  tar_target(
    model_select,
    model_selection(combined_treatments),
    iteration = 'list'
  ),
  tar_target(
    tidy_model_select,
    tidy_model(model_select, effect = 'ran_vals')
  ),
  
  tar_target(
    model_move,
    model_movement(combined_treatments),
    iteration = 'list'
  ),
  tar_target(
    tidy_model_move,
    tidy_model(model_move, effect = 'ran_vals')
  ),
  
  tar_target(
    model_binom,
    model_binomial(prep_binomial),
    iteration = 'list'
  )
)



# Targets: speed ----------------------------------------------------------
targets_speed <- c(
  tar_target(
    cleaned_names,
    clean_model_names(tidymodelp3)
  ),
  
  tar_target(
    tidied_coefs,
    tidy_coefs(cleaned_names, distparams),
    pattern = map(distparams)
  ),
  
  tar_target(
    predict_seq,
    make_predict_seq(combined_treatments, tidy(modelp3))
  ),
  
  tar_target(
    predicted_speed,
    predict_speed(tidied_coefs, predict_seq),
    pattern = map(tidied_coefs)
  ),
  
  tar_target(
    plotted_speed_brick,
    plot_speed_brick(predicted_speed)
  ),
  
  tar_target(
    plotted_speed_edge,
    plot_speed_edge(predicted_speed)
  )
)



# Targets: RSS ------------------------------------------------------------
targets_rss <- c(
  tar_target(
    predictmeans,
    predict_means(combined_treatments, modelp1)
    # predict_means(combined_treatments[subset with i?], modelp1)
  ),
  
  # TODO: need to fix error of mismatching lengths
  #       id_treat seems to have length of 11
  #       this is possibly fixed by adjusting what the by= is
  #       within these functions
  tar_target(
    predictbricks,
    predict_brickdist(combined_treatments, modelp1)
    # predict_brickdist(combined_treatments[subset with i?], modelp1)
  ),
  tar_target(
    predictedges,
    predict_edgedist(combined_treatments, modelp1)
    # predict_edgedist(combined_treatments[subset with i?], modelp1)
  ),
  tar_target(
    rss,
    calc_rss(predictedges, predictbricks, predictmeans)
  ) 
)




# Targets: plots ----------------------------------------------------------
targets_plots <- c(
  tar_target(
    fig_rss_edge,
    plot_rss_edge(rss)
  ),
  tar_target(
    fig_rss_brick,
    plot_rss_brick(rss)
  ),
  tar_target(
    fig_binomial,
    plot_binomial(model_binom)
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
# Automatically grab all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)

# This is equivalent to 
# c(targets_distribution, targets_issa, ...)
# Because remember - the _targets.R file always needs to end 
# in a list of targets tar_target objects