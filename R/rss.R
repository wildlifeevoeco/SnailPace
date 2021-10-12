calc_rss <- function(prededge, predbrick, means){
  preds <- rbind(prededge[,.(id_treat, ghostbricks, stage, var = 'edge', x = edgedist_end, h1 = hab)],
                 predbrick[,.(id_treat, ghostbricks, stage, var = 'brick', x = brickdist_end, h1 = hab)])
  rss <- merge(preds, 
               means[,.(id_treat, ghostbricks, stage, h2 = hab)], 
               by = c('id_treat', 'ghostbricks', 'stage'))
  rss[,rss := h1 - h2]
}