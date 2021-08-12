
# === Treatments ----------------------------------------------------------

brick_treatments <- function(DT, treatments) {
  new_names <- c('brickdist_start', 'brickdist_end')
  
  l <- lapply(treatments, function(treat) {
    setnames(
      copy(DT), 
      c(paste0('brickedge', treat, '_start'), paste0('brickedge', treat, '_end')), 
      new_names
    )[, ghostbricks := treat]
  })
  
  rbindlist(l)
}
