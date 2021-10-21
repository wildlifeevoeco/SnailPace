# === Models --------------------------------------------------------------



# Model selection ---------------------------------------------------------
model_selection <- function(DT) {
  glmmTMB(
    case_ ~
      I(log(sl_ + 1)):temp +
      I(log(brickdist_end + 1)):stage:ghostbricks +
      I(log(edgedist_end + 1)):stage:ghostbricks +
      
      #Random effects
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



# Model movement ----------------------------------------------------------
model_movement <- function(DT) {
  glmmTMB(
    case_ ~
      I(log(sl_ + 1)):temp +
      I(log(sl_ + 1)):tod_start_ +
      I(log(sl_ + 1)):stage:ghostbricks +
      I(log(brickdist_start + 1)):I(log(sl_ + 1)):stage:ghostbricks +
      I(log(edgedist_start + 1)):I(log(sl_ + 1)):stage:ghostbricks +
      
      # Random effects
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



# Model binomial ----------------------------------------------------------
model_binomial <- function(DT) {
  
}

