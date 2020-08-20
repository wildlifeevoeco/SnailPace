#### Figures ====

### Packages ----
libs <- c('ggplot2','ggpubr','data.table', 'dplyr', 'tidyr', 'tidyverse', 'patchwork')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/snails/Data/raw/'
derived <- '~/snails/Data/derived/'
CoreModel <- readRDS('~/snails/Data/derived/CoreModel.Rds')
P1Model <- readRDS('~/snails/Data/derived/P1Model.Rds')
P2Model <- readRDS('~/snails/Data/derived/P2Model.Rds')
P3Model <- readRDS('~/snails/Data/derived/P3Model.Rds')
P4Model <- readRDS('~/snails/Data/derived/P4Model.Rds')

# pulling AICs
p1.aic <- unique(P1Model[,.(AIC_p1=AIC, nbricks, Disturbance), by=.(snail)])
p2.aic <- unique(P2Model[,.(AIC_p2=AIC, nbricks, Disturbance), by=.(snail)])
p3.aic <- unique(P3Model[,.(AIC_p3=AIC, nbricks, Disturbance), by=.(snail)])
p4.aic <- unique(P4Model[,.(AIC_p4=AIC, nbricks, Disturbance), by=.(snail)])
core.aic <- unique(CoreModel[,.(AIC_core=AIC, nbricks, Disturbance), by=.(snail)])

# merge models together
p12all.aic <- merge(as.data.frame(p1.aic), as.data.frame(p2.aic), by=c('snail', 'nbricks', 'Disturbance'), all=T)
p123all.aic <- merge(as.data.frame(p12all.aic), as.data.frame(p3.aic), by=c('snail', 'nbricks', 'Disturbance'), all=T)
p1234all.aic <- merge(as.data.frame(p123all.aic), as.data.frame(p4.aic), by=c('snail', 'nbricks', 'Disturbance'), all=T)
pall.aic <- merge(as.data.frame(p1234all.aic), as.data.frame(core.aic), by=c('snail', 'nbricks', 'Disturbance'), all=T)

# evidence ratios
pall.aic <-melt(setDT(pall.aic)[,.(snail, nbricks, Disturbance, p1=AIC_p1, p2=AIC_p2, p3=AIC_p3, p4=AIC_p4, 
                          core=AIC_core)])

c2 <- pall.aic[nbricks!="2" & Disturbance=="Control", .(snail)]

pall.aic.c2 <- pall.aic[Disturbance=="Disturbed" | nbricks== c("2", "0")]

m.aic.wts <- pall.aic.c2[!is.na(value) & !is.infinite(value),.(model=variable, AIC=value, nbricks, Disturbance, weight=MuMIn::Weights(value)), by=.(snail)]

m.aic.wts[, model_rank := frank(AIC, ties.method = 'dense', na.last = 'keep'), by = snail]

m.aic.wts.t<-dcast(m.aic.wts, snail + nbricks + Disturbance + model~ model_rank, value.var = c( 'weight'))
m.aic.t<-dcast(m.aic.wts, snail + nbricks + Disturbance + model~ model_rank, value.var = c( 'AIC'))

m.er <- merge(m.aic.wts[model_rank==1, .(snail, nbricks, Disturbance, model.1=model, AIC.1=AIC)], m.aic.wts[model_rank==2, .(snail, nbricks, Disturbance, model.2=model, AIC.2=AIC)], all =T)
m.er <- merge(m.er, m.aic.wts[model_rank==3, .(snail, nbricks, Disturbance, model.3=model, AIC.3=AIC)], all =T)
m.er <- merge(m.er, m.aic.wts[model_rank==4, .(snail, nbricks, Disturbance, model.4=model, AIC.4=AIC)], all =T)
m.er[,'er1.2']<- exp((m.er$AIC.2-m.er$AIC.1)/2)
m.er[,'er1.3']<- ifelse(!is.na(m.er$AIC.3),exp((m.er$AIC.3-m.er$AIC.1)/2),NA)
m.er[,'er1.4']<- ifelse(!is.na(m.er$AIC.4),exp((m.er$AIC.4-m.er$AIC.1)/2),NA)
m.er$er1.2<-ifelse(is.infinite(m.er$er1.2), 10^300, m.er$er1.2)

mer.sum <- m.er[,. (uniqueN(snail)), by=.(model.1, Disturbance)]

er.mean <-m.er[AIC.2!="NA",.(best=uniqueN(snail),mean=mean(er1.2, na.rm=T)), by=.(nbricks, Disturbance, model.1)]








