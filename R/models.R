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
    
    brickdist_end = 0,
    edgedist_end = 0,
    
    indiv_treat_step_id = NA
  ), by = .(id_treat, ghostbricks, stage)]
  
  # Predict hab for each ghostbricks*stage combination
  # Uses cbind with .BY[[N]] to retain ghostbricks and stage in the model
  # Be careful with .BY[[N]], depends on the order specified in by = 
  means[, hab := predict(model,
                         newdata = cbind(.SD, 
                                         id_treat = .BY[[1]],
                                         ghostbricks = .BY[[2]],
                                         stage = .BY[[3]]),
                         type = "link",
                         re.form = NULL),
        by = .(id_treat, ghostbricks, stage)]
  means
}


predict_brickdist <- function(DT, model) {
  # Take DT, return means for all variables except for seq 0:max brickdist 
  bdist <- DT[, .(
    sl_ = mean(sl_, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    
    brickdist_end = seq(0, max(brickdist_end), length.out = 100),
    edgedist_end = mean(edgedist_end, na.rm = TRUE),
    
    indiv_treat_step_id = NA
  ), by = .(ghostbricks, stage, id_treat)]
  
  # Predict hab for each ghostbricks*stage combination
  # Uses cbind with .BY[[N]] to retain ghostbricks and stage in the model
  # Be careful with .BY[[N]], depends on the order specified in by = 
  bdist[, hab := predict(model,
                         newdata = cbind(.SD, 
                                         id_treat = .BY[[1]],
                                         ghostbricks = .BY[[2]],
                                         stage = .BY[[3]]),
                         type = "link",
                         re.form = NULL),
        by = .(id_treat, ghostbricks, stage)]
  bdist
}


predict_edgedist <- function(DT, model) {
  # Take DT, return means for all variables except for seq 0:max edgedist 
  edist <- DT[, .(
    sl_ = mean(sl_, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    
    brickdist_end = mean(brickdist_end, na.rm = TRUE),
    edgedist_end = seq(0, max(edgedist_end), length.out = 100),
    
    indiv_treat_step_id = NA 
  ), by = .(id_treat, ghostbricks, stage)]
  
  # Predict hab for each ghostbricks*stage combination
  # Uses cbind with .BY[[N]] to retain ghostbricks and stage in the model
  # Be careful with .BY[[N]], depends on the order specified in by = 
  edist[, hab := predict(model,
                         newdata = cbind(.SD, 
                                         id_treat = .BY[[1]],
                                         ghostbricks = .BY[[2]],
                                         stage = .BY[[3]]),
                         type = "link",
                         re.form = NULL),
        by = .(id_treat, ghostbricks, stage)]
  edist
}

calc_rss <- function(prededge, predbrick, means){
  preds <- rbind(prededge[,.(id_treat, ghostbricks, stage, var = 'edge', x = edgedist_end, h1 = hab)],
                    predbrick[,.(id_treat, ghostbricks, stage, var = 'brick', x = brickdist_end, h1 = hab)])
  rss <- merge(preds, 
               means[,.(id_treat, ghostbricks, stage, h2 = hab)], 
               by = c('id_treat', 'ghostbricks', 'stage'))
  rss[,rss := h1 - h2]
}

calc_speed <- function(){
  
}
