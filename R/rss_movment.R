### RSS and movement calculations ###
# Julie Turner
# revised 11 August 2021

### Packages ----
libs <- c('data.table', 'dplyr', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed','ggthemes')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'

dat<- readRDS(paste0(derived,'snailRandSteps.RDS'))
dat[,indiv_treat_step_id:=paste(indiv_step_id, ghostbricks, sep ='_')]
dat[,id_treat:=paste(id, ghostbricks, sep ='_')]
dat <- dat[stage!='Acc']

dat$stage <- factor(dat$stage, levels = c("B","A"))
dat$ghostbricks <- factor(dat$ghostbricks, levels = c("g1", "g2","g3", 
                                                      '1','2', '3'))

### model output
p1<- readRDS(paste0(derived,'modP1.RDS'))
p3<- readRDS(paste0(derived,'modP3.RDS'))




test <- function(DT, mod, pd, brick, habvar){
  #unique(
  DT[, .(
        sl_ = mean(sl_, na.rm = T),
        temp = mean(temp, na.rm = T),
        stage = factor(pd, levels = levels(stage)),
        ghostbricks = factor(brick, levels = levels(ghostbricks)),
        brickdist_end = if(habvar == 'brickdist') seq(0,40, length.out = 100)
        else mean(brickdist_end, na.rm = T),
        edgedist_end = if(habvar == 'edgedist') seq(0,20, length.out = 100)
        else mean(edgedist_end, na.rm = T),
        indiv_treat_step_id = NA,
        id_treat = NA
      )]
  # )
}

test(dat, p1, 'B', '1', 'brickdist')

### RSS ----
#### RSS functions ####
### POP ###
p.pop <- function(DT, mod, pd, brick, habvar, habvalue){
  #unique(
  DT[
    ,.(hab = predict(
      mod,
      newdata = .SD[, .(
        sl_ = mean(sl_, na.rm = T),
        temp = mean(temp, na.rm = T),
        stage = factor(pd, levels = levels(stage)),
        ghostbricks = factor(brick, levels = levels(ghostbricks)),
        brickdist_end = if(habvar == 'brickdist') seq(0,40, length.out = 100)
        else mean(brickdist_end, na.rm = T),
        edgedist_end = if(habvar == 'edgedist') seq(0,20, length.out = 100)
        else mean(edgedist_end, na.rm = T),
        indiv_treat_step_id = NA,
        id_treat = NA
      )],
      type = "link",
      re.form = NA
    ), 
    pd, brick, habvar, habvalue)]
  # )
}

### INDIVS ###
DT = dat
i = snails1[[1]][[1]]
habvar = 'brickdist'
pd = 'B'
brick = '1'
mod = p1
p.indiv <- function(ids, DT, mod, pd, brick, habvar){
  lapply(ids, function(i) {
    if(habvar == 'brickdist')
    xseq <- seq(0, DT[id_treat ==i, max(brickdist_end)], length.out = 100)
    if(habvar == 'edgedist')
    xseq <- seq(0, DT[id_treat ==i, max(edgedist_end)], length.out = 100)
    pred <- DT[
      ,.(hab = predict(
        mod,
        newdata = .SD[,.(
          sl_ = mean(sl_, na.rm = T),
          temp = mean(temp, na.rm = T),
          stage = factor(pd, levels = levels(stage)),
          ghostbricks = factor(brick, levels = levels(ghostbricks)),
          brickdist_end = if(habvar == 'brickdist') xseq
          else mean(brickdist_end, na.rm = T),
          edgedist_end = if(habvar == 'edgedist') xseq
          else mean(edgedist_end, na.rm = T),
          indiv_treat_step_id = NA,
          id_treat = i
        )],
        type = "link",
        re.form = NULL
      ), 
      id = i, pd, brick, habvar, x = xseq)]
    # )
    return(pred)
  })
  
}

snails <- unique(dat$id_treat)
snails1 <- dat[ghostbricks == '1', .(id_treat = unique(id_treat))]
snails2 <- dat[ghostbricks == '2', .(id_treat = unique(id_treat))]
snails3 <- dat[ghostbricks == '3', .(id_treat = unique(id_treat))]
snailsG1 <- dat[ghostbricks == 'g1', .(id_treat = unique(id_treat))]
snailsG2 <- dat[ghostbricks == 'g2', .(id_treat = unique(id_treat))]
snailsG3 <- dat[ghostbricks == 'g3', .(id_treat = unique(id_treat))]


### h2 ----

before.1.h2.indiv <- rbindlist(p.indiv(snails1, dat, p1, pd = 'B', brick = '1', habvar = 'none', habvalue = NA))
before.2.h2.indiv <- rbindlist(p.indiv(snails2, dat, p1, pd = 'B', brick = '2', habvar = 'none', habvalue = NA))
before.3.h2.indiv <- rbindlist(p.indiv(snails3, dat, p1, pd = 'B', brick = '3', habvar = 'none', habvalue = NA))
before.g1.h2.indiv <- rbindlist(p.indiv(snailsG1, dat, p1, pd = 'B', brick = 'g1', habvar = 'none', habvalue = NA))
before.g2.h2.indiv <- rbindlist(p.indiv(snailsG2, dat, p1, pd = 'B', brick = 'g2', habvar = 'none', habvalue = NA))
before.g3.h2.indiv <- rbindlist(p.indiv(snailsG3, dat, p1, pd = 'B', brick = 'g3', habvar = 'none', habvalue = NA))

after.1.h2.indiv <- rbindlist(p.indiv(snails1, dat, p1, pd = 'A', brick = '1', habvar = 'none', habvalue = NA))
after.2.h2.indiv <- rbindlist(p.indiv(snails2, dat, p1, pd = 'A', brick = '2', habvar = 'none', habvalue = NA))
after.3.h2.indiv <- rbindlist(p.indiv(snails3, dat, p1, pd = 'A', brick = '3', habvar = 'none', habvalue = NA))
after.g1.h2.indiv <- rbindlist(p.indiv(snailsG1, dat, p1, pd = 'A', brick = 'g1', habvar = 'none', habvalue = NA))
after.g2.h2.indiv <- rbindlist(p.indiv(snailsG2, dat, p1, pd = 'A', brick = 'g2', habvar = 'none', habvalue = NA))
after.g3.h2.indiv <- rbindlist(p.indiv(snailsG3, dat, p1, pd = 'A', brick = 'g3', habvar = 'none', habvalue = NA))


#### h1 brickdist ####
before.1.h1.indiv.brick <- rbindlist(p.indiv(snails1, dat, p1, pd = 'B', brick = '1', habvar = 'brickdist'))
before.2.h1.indiv.brick <- rbindlist(p.indiv(snails2, dat, p1, pd = 'B', brick = '2', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
before.3.h1.indiv.brick <- rbindlist(p.indiv(snails3, dat, p1, pd = 'B', brick = '3', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
before.g1.h1.indiv.brick <- rbindlist(p.indiv(snailsG1, dat, p1, pd = 'B', brick = 'g1', habvar = 'brickdist'), habvalue = seq(0,40, length.out = 100))
before.g2.h1.indiv.brick <- p.indiv(snailsG2, dat, p1, pd = 'B', brick = 'g2', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100))

before.g3.h1.indiv.brick <- rbindlist(p.indiv(snailsG3, dat, p1, pd = 'B', brick = 'g3', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))

after.1.h1.indiv.brick <- rbindlist(p.indiv(snails1, dat, p1, pd = 'A', brick = '1', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
after.2.h1.indiv.brick <- rbindlist(p.indiv(snails2, dat, p1, pd = 'A', brick = '2', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
after.3.h1.indiv.brick <- rbindlist(p.indiv(snails3, dat, p1, pd = 'A', brick = '3', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
after.g1.h1.indiv.brick <- rbindlist(p.indiv(snailsG1, dat, p1, pd = 'A', brick = 'g1', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
after.g2.h1.indiv.brick <- rbindlist(p.indiv(snailsG2, dat, p1, pd = 'A', brick = 'g2', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))
after.g3.h1.indiv.brick <- rbindlist(p.indiv(snailsG3, dat, p1, pd = 'A', brick = 'g3', habvar = 'brickdist', habvalue = seq(0,40, length.out = 100)))


#### h1 edgedist ####
before.1.h2.indiv.edge <- rbindlist(p.indiv(snails1, dat, p1, pd = 'B', brick = '1', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
before.2.h2.indiv.edge <- rbindlist(p.indiv(snails2, dat, p1, pd = 'B', brick = '2', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
before.3.h2.indiv.edge <- rbindlist(p.indiv(snails3, dat, p1, pd = 'B', brick = '3', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
before.g1.h2.indiv.edge <- rbindlist(p.indiv(snailsG1, dat, p1, pd = 'B', brick = 'g1', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
before.g2.h2.indiv.edge <- rbindlist(p.indiv(snailsG2, dat, p1, pd = 'B', brick = 'g2', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
before.g3.h2.indiv.edge <- rbindlist(p.indiv(snailsG3, dat, p1, pd = 'B', brick = 'g3', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))

after.1.h2.indiv.edge <- rbindlist(p.indiv(snails1, dat, p1, pd = 'A', brick = '1', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
after.2.h2.indiv.edge <- rbindlist(p.indiv(snails2, dat, p1, pd = 'A', brick = '2', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
after.3.h2.indiv.edge <- rbindlist(p.indiv(snails3, dat, p1, pd = 'A', brick = '3', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
after.g1.h2.indiv.edge <- rbindlist(p.indiv(snailsG1, dat, p1, pd = 'A', brick = 'g1', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
after.g2.h2.indiv.edge <- rbindlist(p.indiv(snailsG2, dat, p1, pd = 'A', brick = 'g2', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))
after.g3.h2.indiv.edge <- rbindlist(p.indiv(snailsG3, dat, p1, pd = 'A', brick = 'g3', habvar = 'edgedist', habvalue = seq(0,35, length.out = 100)))

