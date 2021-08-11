libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'

dat<- readRDS(paste0(derived,'snailRandSteps.RDS'))
dat[,indiv_treat_step_id:=paste(indiv_step_id, ghostbricks, by ='_')]
dat[,id_treat:=paste(id, ghostbricks, by ='_')]

dat$stage <- factor(dat$stage, levels = c("Acc", "B","A"))
dat$ghostbricks <- factor(dat$ghostbricks, levels = c("g1", "g2","g3", 
                                                      '1','2', '3'))

p1 <- glmmTMB(case_ ~ 
                I(log(sl_+1)):temp +
                I(log(sl_+1)):stage:ghostbricks + 
                I(log(brickdist_end + 1)):stage:ghostbricks +
              ## random effects  
              (1|indiv_treat_step_id) + 
                (0 + I(log(sl_+1))|id_treat) +
                (0 + I(log(sl_+1)):stage|id_treat) +
                (0 + I(log(brickdist_end + 1)):stage|id_treat),
                family=poisson(), 
              data = dat,  
              map = list(theta=factor(c(NA,1:13))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 13))))
summary(p1)


