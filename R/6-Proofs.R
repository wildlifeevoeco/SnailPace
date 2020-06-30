### Proofs ###
# Julie Turner
# started 30 June 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'tidyr', 'ggplot2', 'patchwork')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'
p1.rss.all<-readRDS('Data/derived/p1rss_1-2hr.Rds')

set.seed(53)
iterations <- 100
#### proof 1hr vs 2hr ####
p1.rss.all[,.(sample(samp)), by=.(snail)]
rand.samp <- lapply(seq(0, iterations), function(iter){
  sub<-p1.rss.all
  if (iter == 0) {
    sub[, randSamp := samp]
  } else {
    sub[, randSamp := sample(samp), by= .(snail)]
  }
  subls <- rbindlist(sub)
  subls[,iteration:=iter]
})

#### proof before disturbance ####

#### proof treatment diffs ####