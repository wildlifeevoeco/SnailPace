# === Treatments ----------------------------------------------------------



# Brick treatments --------------------------------------------------------
brick_treatments <- function(DT) {
  # Treatment: C is control
  # Treatment: 3 is four bricks
  subDT <- DT[treatment != 'C']
  subDT[, treatment := ifelse(treatment == "4", "3", treatment)]
  
  
  m1 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_start'),
    variable.name = 'treatment_start',
    value.name = 'brickdist_start'
  )
  
  m2 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_end'),
    variable.name = 'treatment_end',
    value.name = 'brickdist_end'
  )
  
  zz <- cbind(m1, m2[, .(brickdist_end)])
  
  zz[, ghostbricks := gsub('brickedge|_start', '', treatment_start)]
  zz <- zz[treatment == ghostbricks]
  
  return(zz)
}


# Control treatments ------------------------------------------------------
control_treatments <- function(DT) {
  # Treatment: C is control
  subDT <- DT[treatment == 'C']
  
  m1 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_start'),
    variable.name = 'treatment_start',
    value.name = 'brickdist_start'
  )
  
  m2 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_end'),
    variable.name = 'treatment_end',
    value.name = 'brickdist_end'
  )
  
  zz <- cbind(m1, m2[, .(brickdist_end)])
  
  zz[, ghostbricks := paste0('g', gsub('brickedge|_start', '', treatment_start))]
  
  return(zz)
}




# Bind treatments ---------------------------------------------------------
bind_treatments <- function(bricktreats, controltreats) {
  DT <- rbindlist(list(bricktreats, controltreats))
  
  DT[, indiv_treat_step_id := paste(indiv_step_id, ghostbricks, sep = '_')]
  DT[, id_treat := paste(id, ghostbricks, sep = '_')]
  
  # Stage: Acc is acclimation
  subDT <- DT[stage != 'Acc']
  set_factors(subDT)
  
  return(subDT)  
}