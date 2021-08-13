
# === Treatments ----------------------------------------------------------
brick_treatments <- function(DT) {
  m1 <- melt(
    DT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_start'),
    # variable.factor = FALSE,
    variable.name = 'treatment_start',
    value.name = 'brickdist_start'
  )
  
  m2 <- melt(
    DT,
    measure.vars = paste0('brickedge', c(1, 2, 3), '_end'),
    # variable.factor = FALSE,
    variable.name = 'treatment_end',
    value.name = 'brickdist_end'
  )
  
  zz <- cbind(m1, m2[, .(treatment_end, brickdist_end)])
  
  zz[, treatment_start := gsub('brickedge|_start', '', treatment_start)]
  zz[, treatment_end := gsub('brickedge|_end', '', treatment_end)]
}



control_treatments <- function(DT){
  treatments <- c(1, 2, 3)
  dist_names <- c('brickdist_start', 'brickdist_end')
  drop_names <- c(paste0('brickedge', treatments, rep(c('_start', '_end'), each = 3)))
  
  subDT <- DT[treatment == 'C']
  
  l <- lapply(treatments, function(treat) {
    zz <- copy(DT)
    zz[, ghostbricks := paste0('g', treat)]
    setnames(
      zz, 
      c(paste0('brickedge', treat, '_start'), paste0('brickedge', treat, '_end')), 
      dist_names
    )
    zz[, (drop_names) := NULL]
    zz
  })
  
  subDT <- DT[treatment == 'C']
  subDT[, (dist_names) := NA]
  subDT[, ghostbricks := 'C']
  subDT[, (drop_names) := NULL]
  
  rbindlist(c(l, list(subDT)), use.names = TRUE)
}
  


combine_treatments <- function(bricktreats, controltreats) {
  DT <- rbindlist(list(bricktreats, controltreats))
  
  DT[, indiv_treat_step_id := paste(indiv_step_id, ghostbricks, sep = '_')]
  DT[, id_treat := paste(id, ghostbricks, sep = '_')]
  
  subDT <- DT[stage!='Acc']
  subDT[, stage := factor(stage, levels = c("B","A"))]
  subDT[, ghostbricks := factor(ghostbricks, levels = c("g1", "g2","g3", 
                                                        '1','2', '3'))]
  
  # TODO: drop where ghostbricks are NA?
  return(subDT)  
}