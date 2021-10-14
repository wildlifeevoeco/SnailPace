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
make_random_steps <- function(DT, brick1, brick2, brick3, edgeDist) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  DT %>% random_steps(n = 40, sl_distr = fit_distr(.$sl_, 'exp')) %>%
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
  
  setDT(DT)[, indiv_step_id := paste(id, step_id_, sep = '_')]
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

# rename mergelc column
make_good_names <- function(DT, old, new){
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  setnames(DT, old, new)
  
  return(DT)
}

# iSSA ------------------------------------------------------
make_iSSA <- function(DT, resp, expl) {
  if (is.null(DT))
    return()
  if (nrow(DT) == 0)
    return()
  
  mod.tmp <- glmmTMB(
    formula = reformulate(expl, resp),
    data = DT,
    family = Poisson(),
    doFit = FALSE
  )
  
  mod.tmp$parameters$theta[1] <- log(1e3)
  nvarparm <- length(mod.tmp$parameters$theta)
  mod.tmp$mapArg <- list(theta = factor(c(NA, 1:(nvarparm - 1))))
  fitTMB(mod.tmp)
}



# Merge steps onto prep data ----------------------------------------------
merge_steps <- function(DT, prepDT) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  subDT <- setDT(DT)[(case_) | !is.na(brickedge1_end)]
  subDT[, iter := seq.int(.N), by = .(id, step_id_)]
  
  # TODO: check this
  subsubDT <- subDT[iter <= 11]
  # TODO: check these
  # setnames(subsubDT, c('snail'), c('id'))
  
  merge(subsubDT, prepDT, by.x = c('id', 't1_'), by.y = c('snail', 't'))
}

