# === iSSA ----------------------------------------------------------------



# Check resamples ---------------------------------------------------------
resample_tracks <- function(tracks, rate, tolerance, binomial = FALSE) {
  t <- track_resample(tracks, rate = rate, tolerance = tolerance) %>%
    filter_min_n_burst()
  
  # Cancel if there are not at least 20 observed steps after resample
  # this is semi-arbitrary, but this should be enough for robust estimates in the model 
  # (Street et al preprint 2021)
  
  # But keep > 3 for binomial
  if (binomial) {
    if (nrow(t) <= 3) return()
  } else {
    if (nrow(t) < 20) return()
  }

  t %>% steps_by_burst(., keep_cols = 'start')
}



# Make random steps ------------------------------------------------------
make_random_steps <- function(DT, n_random_steps, brick1, brick2, brick3, edgeDist) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  DT %>% random_steps(n = n_random_steps, sl_distr = fit_distr(.$sl_, 'exp')) %>%
    extract_covariates(brick1, where = "both") %>%
    extract_covariates(brick2, where = "both") %>%
    extract_covariates(brick3, where = "both") %>%
    extract_covariates(edgeDist, where = "both") %>%
    time_of_day(where = 'start')
}


# Make unique step ID across individuals -----------------------------------
make_step_id <- function(DT) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  DT$indiv_step_id <- paste(DT$id, DT$step_id_, sep = '_')
  DT
}

# Calculate distribution parameters ---------------------------------------
calc_distribution_parameters <- function(steps) {
  if (is.null(steps)) return()
    
  data.table(
    id = unique(steps$id),
    kappa = ta_distr_params(steps)$kappa,
    rate = sl_distr_params(steps, sl_distr = fit_distr(.$sl_, 'exp'))$rate
  )
}



# Merge steps onto prep data ----------------------------------------------
merge_steps <- function(DT, prepDT, limit_edge = TRUE) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  if (limit_edge) {
    # To handle extra locs, ensuring sufficient random locs, 
    # checking within the bounds by not na bridge edge
    subDT <- setDT(DT)[(case_) | !is.na(brickedge1_end)]
    subDT[, iter := seq.int(.N), by = .(id, step_id_)]
    
    DT <- subDT[iter <= 11]
  }

  merge(DT, prepDT, by.x = c('id', 't1_'), by.y = c('snail', 't'))
}

