
# === Treatments ----------------------------------------------------------

brick_treatments <- function(DT) {
  treatments <- c(1, 2, 3)
  new_names <- c('brickdist_start', 'brickdist_end')
  drop_names <- c(paste0('brickedge', treatments, rep(c('_start', '_end'), each = 3)))
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
  