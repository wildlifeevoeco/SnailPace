# === Predictions ---------------------------------------------------------



# Predict: means ----------------------------------------------------------
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
  
  return(means)
}



# Predict: brick dist ------------------------------------------------------
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
  
  return(bdist)
}



# Predict: edge dist ------------------------------------------------------
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
  
  return(edist)
}



# Predict: speed ----------------------------------------------------------
predict_speed <- function(coefs, seqs) {
  if (is.null(coefs)) return(NULL)
  
  bdist <- seqs$bdist
  edist <- seqs$edist
  logsltemp <- seqs$logsltemp
  logsltod <- seqs$logsltod
  meanedge <- seqs$meanedge
  maxedge <- seqs$maxedge
  meanbrick <- seqs$meanbrick
  maxbrick <- seqs$maxbrick
  meantemp <- seqs$meantemp
  
  pred_length <- 100
  repcoef <- coefs[rep(.N, pred_length)]
  
  repcoef[, bd.spd.before := 
            (1 + logsl_ + logsl_before + (logsltemp * meantemp) + logsltod +
               (brickdist_logsl_before * bdist) + 
               (edgedist_logsl_before * meanedge)) * (1 / rate),
          by = .(id, brick)]
  repcoef[, bd.spd.after := 
            (1 + logsl_ + logsl_after + (logsltemp * meantemp) + logsltod +
               (brickdist_logsl_after * bdist) + 
               (edgedist_logsl_after * meanedge)) * (1 / rate), 
          by = .(id, brick)]
  repcoef[, bdist := seq(0, maxbrick, length.out = 100), 
          by = .(id, brick)]
  repcoef[, ed.spd.before := 
            (1 + logsl_ + logsl_before + (logsltemp * meantemp) + logsltod +
               (edgedist_logsl_before * edist) + 
               (brickdist_logsl_before * meanbrick)) * (1 / rate), 
          by = .(id, brick)]
  repcoef[, ed.spd.after := 
            (1 + logsl_ + logsl_after + (logsltemp * meantemp) + logsltod +
               (edgedist_logsl_after * edist) + 
               (brickdist_logsl_after * meanbrick)) * (1 / rate),
          by = .(id, brick)]
  repcoef[, edist := seq(0, maxedge, length.out = 100), 
          by = .(id, brick)]
  
  repcoef[, disturbance := ifelse(brick %like% 'g', 'undisturbed', 'disturbed'), 
          by = .(id, brick)]
  
  return(repcoef)
}



# Make prediction sequence ------------------------------------------------
make_predict_seq <- function(combtreats, model) {
  maxedge <- max(combtreats$edgedist_end, na.rm = TRUE)
  maxbrick <- 65
  
  list(
    bdist = seq(0, maxbrick, length.out = 100),
    edist = seq(0, maxedge, length.out = 100),
    logsltemp = model$estimate[[2]],
    logsltod = model$estimate[[3]],
    meanedge = mean(combtreats$edgedist_end, na.rm = TRUE),
    maxedge = maxedge,
    meanbrick = mean(combtreats$brickdist_end, na.rm = TRUE),
    maxbrick = maxbrick,
    meantemp = mean(combtreats$temp, na.rm = TRUE)
  )
}