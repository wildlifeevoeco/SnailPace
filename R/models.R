# === Models --------------------------------------------------------------


model_p1 <- function(DT) {
  p1 <- glmmTMB(case_ ~ 
                  I(log(sl_+1)):temp +
                  #I(log(sl_+1)):stage:ghostbricks + 
                  I(log(brickdist_end + 1)):stage:ghostbricks +
                  I(log(edgedist_end + 1)):stage:ghostbricks +
                  ## random effects  
                  (1|indiv_treat_step_id) + 
                  (0 + I(log(sl_+1))|id_treat) +
                  (0 + I(log(sl_+1)):stage|id_treat) +
                  (0 + I(log(brickdist_end + 1)):stage|id_treat) +
                  (0 + I(log(edgedist_end + 1)):stage|id_treat),
                family=poisson(), 
                data = DT,  
                map = list(theta=factor(c(NA,1:10))), 
                start = list(theta=c(log(1000), seq(0,0, length.out = 10))))

}




tidy_model <- function(model, effect) {
  ran_vals <- tidy(model, effect = effect)
  indiv.se <- setDT(ran_vals)[group == 'id_treat']
}



model_p3 <- function(DT) {
  p3 <- glmmTMB(case_ ~ 
                  I(log(sl_+1)):temp +
                  I(log(sl_+1)):stage:ghostbricks + 
                  I(log(brickdist_start + 1)):I(log(sl_+1)):stage:ghostbricks +
                  I(log(edgedist_start + 1)):I(log(sl_+1)):stage:ghostbricks +
                  ## random effects  
                  (1|indiv_treat_step_id) + 
                  (0 + I(log(sl_+1))|id_treat) +
                  (0 + I(log(sl_+1)):stage|id_treat) +
                  (0 + I(log(brickdist_start + 1)):I(log(sl_+1)):stage|id_treat) +
                  (0 + I(log(edgedist_start + 1)):I(log(sl_+1)):stage|id_treat),
                family=poisson(), 
                data = DT,  
                map = list(theta=factor(c(NA,1:10))), 
                start = list(theta=c(log(1000), seq(0,0, length.out = 10))))
}



predict_model_setup <- function(DT) {
  DT[, .(
    brickdist_end = mean(brickdist_end, na.rm = TRUE),
    edgedist_end = mean(edgedist_end, na.rm = TRUE),
    
    brickdist_end_seq = seq(0, max(brickdist_end), length.out = 100),
    edgedist_end_seq = seq(0, max(edgedist_end), length.out = 100)
  )
  , by = .(id, ghostbricks, stage)]
}


set_factors <- function(DT) {
  stagelevels <- c("B","A")
  ghostbricklevels <- c("g1", "g2","g3", 
                        '1','2', '3')
  DT[, stage := factor(stage, levels = stagelevels)]
  DT[, ghostbricks := factor(ghostbricks, levels = ghostbricklevels)]
  
}
