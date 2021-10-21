# === Helpers -------------------------------------------------------------



# Tidy coefs --------------------------------------------------------------
tidy_coefs <- function(model, distparams) {
  if (is.null(distparams)) return(NULL)
  
  coefs <- dcast(model, level ~ term, value.var = 'estimate')
  coefs <- coefs %>% tidyr::separate((level), c('id', 'brick'), sep = '_')
  coefs <- merge(coefs, distparams, by = 'id')
  
  return(coefs)
}



# Tidy model --------------------------------------------------------------
tidy_model <- function(model, effect) {
  # Get random effects for id * treatment
  ran_vals <- tidy(model, effect = effect)
  indiv.se <- setDT(ran_vals)[group == 'id_treat']
  
  return(indiv.se)
}



# Set factors -------------------------------------------------------------
set_factors <- function(DT, 
                        treatment_levels = c('C', '1', '2', '4'),
                        stage_levels = c('B', 'A'), 
                        ghostbrick_levels = c('g1', 'g2', 'g3', '1', '2', '3')
                        ) {
  
  if ('stage' %in% colnames(DT)) {
    DT[, stage := factor(stage, levels = stage_levels)]
  }
  
  if ('ghostbricks' %in% colnames(DT)) {
    DT[, ghostbricks := factor(ghostbricks, levels = ghostbrick_levels)]
  }
  
  if ('treatment' %in% colnames(DT)) {
    DT[, treatment:= factor(treatment, levels = treatment_levels)]
  }
  
  return(DT)
}



# Clean model names -------------------------------------------------------
clean_model_names <- function(model) {
  # making names easier to deal with
  model$term <- gsub(':', '-', model$term)
  model$term <- gsub(' ', '', model$term)
  model$term <- gsub('[[:punct:]]', '', model$term)
  model$term <- gsub('stageA', 'after', model$term)
  model$term <- gsub('stageB', 'before', model$term)
  model$term <- gsub('Ilogsl1', 'logsl_', model$term)
  model$term <- gsub('Ilogbrickdiststart1', 'brickdist_', model$term)
  model$term <- gsub('Ilogedgediststart1', 'edgedist_', model$term)
  
  return(model)
}