
# === Treatments ----------------------------------------------------------
brick_treatments <- function(DT) {
  subDT <- DT[treatment != 'C']
  
  m1 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_start'),
    # variable.factor = FALSE,
    variable.name = 'treatment_start',
    value.name = 'brickdist_start'
  )
  
  m2 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_end'),
    # variable.factor = FALSE,
    variable.name = 'treatment_end',
    value.name = 'brickdist_end'
  )
  
  zz <- cbind(m1, m2[, .(treatment_end, brickdist_end)])
  
  zz[, treatment_start := gsub('brickedge|_start', '', treatment_start)]
  zz[, treatment_end := gsub('brickedge|_end', '', treatment_end)]
  zz
}

control_treatments <- function(DT) {
  subDT <- DT[treatment == 'C']
  
  m1 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_start'),
    # variable.factor = FALSE,
    variable.name = 'treatment_start',
    value.name = 'brickdist_start'
  )
  
  m2 <- melt(
    subDT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_end'),
    # variable.factor = FALSE,
    variable.name = 'treatment_end',
    value.name = 'brickdist_end'
  )
  
  zz <- cbind(m1, m2[, .(treatment_end, brickdist_end)])
  
  zz[, treatment_start := paste0('c', gsub('brickedge|_start', '', treatment_start))]
  zz[, treatment_end := paste0('c', gsub('brickedge|_end', '', treatment_end))]
  zz
}





combine_treatments <- function(bricktreats, controltreats) {
  DT <- rbindlist(list(bricktreats, controltreats))
  
  DT[, indiv_treat_step_id := paste(indiv_step_id, ghostbricks, sep = '_')]
  DT[, id_treat := paste(id, ghostbricks, sep = '_')]
  
  subDT <- DT[stage != 'Acc']
  subDT[, stage := factor(stage, levels = c("B","A"))]
  subDT[, ghostbricks := factor(ghostbricks, levels = c("g1", "g2","g3", 
                                                        '1','2', '3'))]
  
  # TODO: drop where ghostbricks are NA?
  return(subDT)  
}