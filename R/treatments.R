
# === Treatments ----------------------------------------------------------

brick_treatments <- function(DT, treatments) {
  new_names <- c('brickdist_start', 'brickdist_end')
  drop_names <- c(paste0('brickedge', c(1, 2, 3), rep(c('_start', '_end'), each = 3)))
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
