# === Models --------------------------------------------------------------


model_p1 <- function(DT) {
  p1 <- glmmTMB(
    case_ ~
      I(log(sl_ + 1)):temp +
      #I(log(sl_+1)):stage:ghostbricks +
      I(log(brickdist_end + 1)):stage:ghostbricks +
      I(log(edgedist_end + 1)):stage:ghostbricks +
      ## random effects
      (1 | indiv_treat_step_id) +
      (0 + I(log(sl_ + 1)) | id_treat) +
      (0 + I(log(sl_ + 1)):stage | id_treat) +
      (0 + I(log(brickdist_end + 1)):stage | id_treat) +
      (0 + I(log(edgedist_end + 1)):stage | id_treat),
    family = poisson(),
    data = DT,
    map = list(theta = factor(c(NA, 1:10))),
    start = list(theta = c(log(1000), seq(0, 0, length.out = 10)))
  )
}




tidy_model <- function(model, effect) {
  ran_vals <- tidy(model, effect = effect)
  indiv.se <- setDT(ran_vals)[group == 'id_treat']
}



model_p3 <- function(DT) {
  p3 <- glmmTMB(
    case_ ~
      I(log(sl_ + 1)):temp +
      I(log(sl_ + 1)):stage:ghostbricks +
      I(log(brickdist_start + 1)):I(log(sl_ + 1)):stage:ghostbricks +
      I(log(edgedist_start + 1)):I(log(sl_ + 1)):stage:ghostbricks +
      ## random effects
      (1 | indiv_treat_step_id) +
      (0 + I(log(sl_ + 1)) | id_treat) +
      (0 + I(log(sl_ + 1)):stage | id_treat) +
      (0 + I(log(
        brickdist_start + 1
      )):I(log(sl_ + 1)):stage | id_treat) +
      (0 + I(log(edgedist_start + 1)):I(log(sl_ + 1)):stage |
         id_treat),
    family = poisson(),
    data = DT,
    map = list(theta = factor(c(NA, 1:10))),
    start = list(theta = c(log(1000), seq(0, 0, length.out = 10)))
  )
}



set_factors <- function(DT) {
  stagelevels <- c("B","A")
  ghostbricklevels <- c("g1", "g2","g3", 
                        '1','2', '3')
  DT[, stage := factor(stage, levels = stagelevels)]
  DT[, ghostbricks := factor(ghostbricks, levels = ghostbricklevels)]
  
}

predict_means <- function(DT, model) {
  # Take DT, return means for all variables
  means <- DT[, .(
    sl_ = mean(sl_, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    
    brickdist_end = mean(brickdist_end, na.rm = TRUE),
    edgedist_end = mean(edgedist_end, na.rm = TRUE),
    
    indiv_treat_step_id = NA,
    id_treat = id_treat
  ), by = .(ghostbricks, stage)]
  
  # Predict hab for each ghostbricks*stage combination
  # Uses cbind with .BY[[N]] to retain ghostbricks and stage in the model
  # Be careful with .BY[[N]], depends on the order specified in by = 
  means[, hab := predict(model,
                         newdata = cbind(.SD, 
                                         ghostbricks = .BY[[1]],
                                         stage = .BY[[2]]),
                         type = "link",
                         re.form = NULL),
        by = .(ghostbricks, stage)]
  means
}


predict_brickdist <- function(DT, model) {
  # Take DT, return means for all variables except for seq 0:max brickdist 
  bdist <- DT[, .(
    sl_ = mean(sl_, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    
    brickdist_end = seq(0, max(brickdist_end), length.out = 100),
    edgedist_end = mean(edgedist_end, na.rm = TRUE),
    
    indiv_treat_step_id = NA,
    id_treat = id_treat
  ), by = .(ghostbricks, stage, id)]
  
  # Predict hab for each ghostbricks*stage combination
  # Uses cbind with .BY[[N]] to retain ghostbricks and stage in the model
  # Be careful with .BY[[N]], depends on the order specified in by = 
  bdist[, hab := predict(model,
                         newdata = cbind(.SD, 
                                         ghostbricks = .BY[[1]],
                                         stage = .BY[[2]]),
                         type = "link",
                         re.form = NULL),
        by = .(ghostbricks, stage, id)]
  bdist
}


predict_edgedist <- function(DT, model) {
  # Take DT, return means for all variables except for seq 0:max edgedist 
  edist <- DT[, .(
    sl_ = mean(sl_, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    
    brickdist_end = mean(brickdist_end, na.rm = TRUE),
    edgedist_end = seq(0, max(edgedist_end), length.out = 100),
    
    indiv_treat_step_id = NA,
    id_treat = id_treat
  ), by = .(ghostbricks, stage, id)]
  
  # Predict hab for each ghostbricks*stage combination
  # Uses cbind with .BY[[N]] to retain ghostbricks and stage in the model
  # Be careful with .BY[[N]], depends on the order specified in by = 
  edist[, hab := predict(model,
                         newdata = cbind(.SD, 
                                         ghostbricks = .BY[[1]],
                                         stage = .BY[[2]]),
                         type = "link",
                         re.form = NULL),
        by = .(ghostbricks, stage, id)]
  edist
}