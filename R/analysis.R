### iSSA analyses ###
# Julie Turner
# revised 11 August 2021

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
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

p1 <- glmmTMB(case_ ~ 
                I(log(sl_+1)):temp +
                #I(log(sl_+1)):stage:ghostbricks + 
                I(log(brickdist_end + 1)):stage:ghostbricks +
                I(log(edgedist_end + 1)):stage:ghostbricks +
              ## random effects  
              (1|indiv_treat_step_id) + 
                (0 + I(log(sl_+1))|id_treat) +
                (0 + I(log(sl_+1)):stage|id_treat) +
                (0 + I(log(brickdist_end + 1)):stage|id_treat) +
                (0 + I(log(edgedist_end + 1)):stage|id_treat),
                family=poisson(), 
              data = dat,  
              map = list(theta=factor(c(NA,1:10))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 10))))
summary(p1)
ran_vals <-tidy(p1, effect= 'ran_vals')
indiv.se <-setDT(ran_vals)[group=='id_treat']

saveRDS(p1, paste0(derived, 'modP1.RDS'))

p3 <- glmmTMB(case_ ~ 
                I(log(sl_+1)):temp +
                I(log(sl_+1)):stage:ghostbricks + 
                I(log(brickdist_start + 1)):I(log(sl_+1)):stage:ghostbricks +
                I(log(edgedist_start + 1)):I(log(sl_+1)):stage:ghostbricks +
                ## random effects  
                (1|indiv_treat_step_id) + 
                (0 + I(log(sl_+1))|id_treat) +
                (0 + I(log(sl_+1)):stage|id_treat) +
                (0 + I(log(brickdist_start + 1)):I(log(sl_+1)):stage|id_treat) +
                (0 + I(log(edgedist_start + 1)):I(log(sl_+1)):stage|id_treat),
              family=poisson(), 
              data = dat,  
              map = list(theta=factor(c(NA,1:10))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 10))))
summary(p3)
saveRDS(p3, paste0(derived,'modP3.RDS'))


