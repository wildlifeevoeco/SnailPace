
# === Treatments ----------------------------------------------------------
brick_treatments <- function(DT) {
  treatments <- c(1, 2, 3)
  new_names <- c('brickdist_start', 'brickdist_end')
  drop_names <- c(paste0('brickedge', treatments, rep(c('_start', '_end'), each = 3)))
  
  # For each treatment, lapply over the DT and rename the columns 
  # of specific brickdist to new_names
  # and drop old named columns
  # Then rbindlist together
  l <- lapply(treatments, function(treat) {
    zz <- copy(DT)[treatment == treat]
    zz[, ghostbricks := treat]
    setnames(
      zz, 
      c(paste0('brickedge', treat, '_start'), paste0('brickedge', treat, '_end')), 
      new_names
    )
    zz[, (drop_names) := NULL]
    zz
  })
  
  rbindlist(l)
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