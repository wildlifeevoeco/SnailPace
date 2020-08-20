### RSS ###
# Julie Turner
# started 13 May 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 
          'tidyr', 'ggplot2','survival','forcats', 'patchwork', 'AICcmodavg', 'ggthemes')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'

dat.hr <- readRDS('Data/derived/ssa-exp-good-ghosts.Rds')
moveParams <- readRDS('Data/derived/moveParams-exp-goods.Rds')
dat.snails <- fread('Data/derived/summary_movement.csv')

dat.hr <- merge(dat.hr, dat.snails[,.(snail, id)], by = 'snail')

dat.hr$Stage <- factor(dat.hr$Stage, levels = c("Acc", "B","A"))
dat.hr$ToD_start <- as.factor(dat.hr$ToD_start)
dat.hr$Precipitation <- as.factor(dat.hr$Precipitation)
dat.obs <- dat.hr[case_==TRUE & ghostbricks != 'g1' & ghostbricks != 'g3' & ghostbricks != 'C' & !(Stage %like% 'Acc')]
dat.obs[,group:=ifelse(Stage == 'B', 'before', 
                       ifelse(Stage=='A' & ghostbricks =='g2', 'undisturbed', 'disturbed'))]

goodsnails <- dat.obs[,.N, .(id,group)]
goodsnails[,dup:=duplicated(id)]
goodsnails[N >=20 ,nobs:=.N, by=.(id)]
goods <- unique(goodsnails[nobs==2, id])



### FUNCTIONS ----
se <- function(x){
  sd(x, na.rm = T)/ sqrt(length(na.omit(x)))
}

# function for model list
list_models <- function(resp, expl, DT) {
  list(list(clogit(reformulate(expl, resp),
                   model = T, data = DT)))
}

list_predict <- function(mod, ND) {
  mapply(function(m, n) predict(m, newdata = n, type = 'lp'),
         m = mod, n = ND)
}

list_issa <- function(resp, expl, DT) {
  list(list(fit_clogit(reformulate(expl, resp),
                       model = T, data = DT)))
}

#' @param ls list column
#' @param val constant value
dif_list <- function(ls, val) {
  list(lapply(ls, function(l) l - val))
}

calc_coef <- function(model) {
  list(lapply(model, coef))
}

calc_coef_names <- function(model) {
  list(lapply(model, function(m) names(coef(m))))
}

mod_list <- function(mod, treat) {
  mapply(function(m, t) paste(m,t, sep = '.'),
         m = mod, t = treat)
}


calc_aic <- function(model) {
  lapply(model, AIC)
}

calc_aictab <- function(models, names) {
  aictab(models, names)
}

calc_evidence <- function(aictab) {
  list(lapply(aictab, evidence))
}


calc_loglik <- function(model) {
  lapply(model, logLik)
}


dat <- dat.hr[ToD_start == 'night' & id %in% goods]

#### CORE ====

# Setup model name, explanatory and response variables
setup <- data.table(
  model = 'core',
  unique(dat[,.(snail, id)]),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  response = 'case_',
  explanatory = 'log_sl + cos_ta + Precipitation:log_sl + Temperature:log_sl + strata(step_id_)'
)


# Which bricks do we want to keep?
setup[ model == 'core', lsbricks := list(c("C", "1", "2", "3"))]

# Run only on *good* individual
setup[, mod :=
        list_models(response, explanatory,
                    dat[ghostbricks %in% unlist(lsbricks) &
                          snail == .BY[[1]]]),
      by = snail]

setup[, issa :=
        list_issa(response, explanatory,
                  dat[ghostbricks %in% unlist(lsbricks) &
                        snail == .BY[[1]]]),
      by = snail]

# coefs 

setup[, coef := calc_coef(mod),
      by = .(snail)]


setup[, var := calc_coef_names(mod),
      by = .(snail)]


move <- setup[,.(coef = unlist(coef), var = unlist(var), model), by = .(snail, id)]
move <- merge(move, moveParams, by = 'snail', all.x = T)

core.move.1hr <- copy(move)
core.issa.1hr <- setup[,.(snail, id, issa, model = model)]
core.mods.1hr <- setup[,.(snail, id, mod, model = model)]


### P1 ----
### Brick Dist ----
# Setup model name, explanatory and response variables
# Cross join individuals and bricks of interest
setup <- CJ(
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  model = 'p1',
  response = 'case_',
  explanatory = 'log_sl + log_sl:Temperature + log(brickdist_end + 1):Stage + strata(step_id_)'
)

setup <- merge(setup, dat.snails[,.(snail,id)], by ='snail')

p1bad <- c('DoorKeeper') 

setup[model == 'p1', bad := id %in% p1bad]


# Run only on *good* individuals and those with > 0 rows
setup[, n := 
        dat[ghostbricks == .BY[[2]] & snail == .BY[[1]], .N],
      by = .(snail, brick)]

setup[!(bad) &  n != 0, mod := 
        list_models(response, explanatory,
                    dat[ghostbricks == .BY[[2]] & snail == .BY[[1]]]),
      by = .(snail, brick)]

setup[!(bad) & n != 0, issa := 
        list_issa(response, explanatory,
                  dat[ghostbricks == .BY[[2]] & snail == .BY[[1]]]),
      by = .(snail, brick)]

# coefs 

setup[!(bad) & n != 0, coef := calc_coef(mod),
      by = .(snail, brick)]


setup[!(bad) & n != 0, var := calc_coef_names(mod),
      by = .(snail, brick)]



# Setup new data
meansl <- mean(dat$log_sl, na.rm = T)
meanta <- mean(dat$cos_ta, na.rm = T)
meantemp = mean(dat$Temperature, na.rm = T)
meanedge <- 0 #mean(dat$edgedist_end, na.rm = T)
maxedge <- max(dat$edgedist_end, na.rm = T)
meanbrick <- 0 #mean(dat$brickdist_end, na.rm = T)
maxbrick <- 65

# h2 before
setup[!(bad) & n != 0, h2Bdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = meanedge,
                        brickdist_end = meanbrick,
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h2 After
setup[!(bad) & n != 0, h2Adat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_end = meanedge,
                        brickdist_end = meanbrick,
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 before edge
# setup[!(bad) & n != 0, h1Bedgedat :=
#         list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
#                       .(log_sl = meansl,
#                         cos_ta = meanta,
#                         ToD_start = factor('day', levels = levels(ToD_start)),
#                         Temperature = meantemp,
#                         Stage = factor('B', levels = levels(Stage)),
#                         edgedist_end = seq(0, maxedge, length.out = 100),
#                         brickdist_end = meanbrick,
#                         step_id_ = step_id_[1])])),
#       by = .(snail, brick)]
# 
# # h1 after edge
# setup[!(bad) & n != 0, h1Aedgedat :=
#         list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
#                       .(log_sl = meansl,
#                         cos_ta = meanta,
#                         ToD_start = factor('day', levels = levels(ToD_start)),
#                         Temperature = meantemp,
#                         Stage = factor('A', levels = levels(Stage)),
#                         edgedist_end = seq(0, maxedge, length.out = 100),
#                         brickdist_end = meanbrick,
#                         step_id_ = step_id_[1])])),
#       by = .(snail, brick)]

# h1 before brick
setup[!(bad) & n != 0, h1Bbrickdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = meanedge,
                        brickdist_end = seq(0, maxbrick, length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 after brick
setup[!(bad) & n != 0, h1Abrickdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_end = meanedge,
                        brickdist_end = seq(0, maxbrick, length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# Predict
setup[!(bad) & n != 0, h2B := list_predict(mod, h2Bdat),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h2A := list_predict(mod, h2Adat),
      by = .(snail, brick)]

# setup[!(bad) & n != 0, h1Bedge := list(list(list_predict(mod, h1Bedgedat))),
#       by = .(snail, brick)]
# setup[!(bad) & n != 0, xBedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
#                                                .(edgedist_end = seq(0, maxedge, length.out = 100))])),
#       by = .(snail, brick)]
# setup[!(bad) & n != 0, h1Aedge := list(list(list_predict(mod, h1Aedgedat))),
#       by = .(snail, brick)]
# setup[!(bad) & n != 0, xAedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
#                                                .(edgedist_end = seq(0, maxedge, length.out = 100))])),
#       by = .(snail, brick)]
setup[!(bad) & n != 0, h1Bbrick := list(list(list_predict(mod, h1Bbrickdat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xBbrick := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                                .(brickdist_end = seq(0, maxbrick, length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Abrick := list(list(list_predict(mod, h1Abrickdat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xAbrick := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                                .(brickdist_end = seq(0, maxbrick, length.out = 100))])),
      by = .(snail, brick)]


rss <- setup[!(bad) & n > 0,.(snail, id, brick, h1Bbrick, h1Abrick, h2B, h2A, 
                              xBbrick, xAbrick)]


# rss[, rssBedge := dif_list(h1Bedge, h2B),
#     by = .(snail, brick)]
# rss[, stepBedge := list(list(seq(0,max(unlist(xBedge)), length.out = 100))),
#     by = .( brick)]
# rss[, rssAedge := dif_list(h1Aedge, h2A),
#     by = .(snail, brick)]
# rss[, stepAedge := list(list(seq(0,max(unlist(xAedge)), length.out = 100))),
#     by = .( brick)]
rss[, rssBbrick := dif_list(h1Bbrick, h2B),
    by = .(snail, brick)]
rss[, stepBbrick := list(list(seq(0,max(unlist(xBbrick)), length.out = 100))),
    by = .( brick)]
rss[, rssAbrick := dif_list(h1Abrick, h2A),
    by = .(snail, brick)]
rss[, stepAbrick := list(list(seq(0,max(unlist(xAbrick)), length.out = 100))),
    by = .( brick)]

# rss.long <- rss[, .(rss = unlist(rssBedge), x = unlist(xBedge), step = unlist(stepBedge), var = 'edgedist', BA = 'before', model = 'p1'),
#                 by = .(snail, brick)]
# rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAedge), x = unlist(xAedge), step = unlist(stepAedge), var = 'edgedist', BA = 'after', model = 'p1'),
#                                 by = .(snail, brick)])
rss.long <- rss[, .(rss = unlist(rssBbrick), x = unlist(xBbrick), step = unlist(stepBbrick), var = 'brickdist', BA = 'before', model = 'p1'),
                                by = .(snail, id, brick)]
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAbrick), x = unlist(xAbrick), step = unlist(stepAbrick), var = 'brickdist', BA = 'after', model = 'p1'),
                                by = .(snail, id, brick)])


p1.rss.1hr <- copy(rss.long)


move <- setup[!(bad) & n > 0,.(coef = unlist(coef), var = unlist(var), model = 'p1'), by = .(snail, id, brick)]
move <- merge(move, moveParams, by = 'snail', all.x = T)
p1.move.1hr <- copy(move)
p1.move.1hr <- p1.move.1hr[!(var %like% 'Acc')]

p1.mods.1hr <- setup[!(bad) & n > 0,.(snail, id, mod, model = paste(model, brick, sep = '.'))]

p1.issa.1hr <- setup[!(bad) & n > 0,.(snail, id, mod, issa, model = paste(model, brick, sep = '.'))]


#saveRDS(p1.rss.1hf, 'Data/derived/p1rss_1hr.Rds')

p1.rss.all<-readRDS('Data/derived/p1rss_1hr.Rds')


p1.rss.all <- p1.rss.1hr

### P1 graphs ----

p1.rss.all[,'disturbance'] <- ifelse(p1.rss.all$brick %like% 'g', 'undisturbed', 'disturbed')
p1.rss.all[,'snails2'] <- paste(p1.rss.all$snail, p1.rss.all$brick, sep = '.')
p1.rss.all[,disturbance.rss:=mean(rss, na.rm = T), by=.(step, disturbance)]
p1.rss.all[,brick.rss:=mean(rss), by=.(step, brick)]

#removed 'samp'
p1.rss <- p1.rss.all[brick != 'g1' & brick != 'g3',.(snail, brick, snails2, rss, x, var, BA, disturbance,
                                                     treatment = ifelse(BA =='before', 'before', disturbance))]
p1.rss[var =='brickdist',step:= seq(0, 65, length.out = 100), by=.(snail, treatment)]


p1.rss.brick.before <- ggplot(data=p1.rss.all[var == 'brickdist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
                              aes(x, rss)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  geom_smooth(size = 1.5, fill = 'purple', colour = 'purple', se = T) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  ggtitle("before")  +
  # ylim(-150,100) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))
p1.rss.brick.before

p1.rss.brick.after <- ggplot(data=p1.rss.all[var == 'brickdist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
                             aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = disturbance),  se = T) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  ggtitle("after")  +
  #ylim(-150,100) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))
p1.rss.brick.after

#### P1 sum graphs ----
p1.rss.sum <- p1.rss[, .(rss = mean(rss), se = se(rss)), by = .(var, step, treatment)]
p1.rss.sum[, upr:= rss + (1.96*se)]
p1.rss.sum[, lwr:= rss - (1.96*se)]
p1.rss[,'snails2'] <- paste(p1.rss$snail, p1.rss$treatment, sep = '.')
p1.rss$treatment <- factor(p1.rss$treatment, levels = c('before', 'undisturbed', 'disturbed'))

p1.rss.sum.brick <- ggplot(data=p1.rss.sum[var == 'brickdist'], 
                           aes(step, rss, color = treatment)) +
  geom_line(aes(group = treatment), show.legend = T) +
  geom_ribbon(aes(step, ymin = (lwr), ymax = (upr), fill=treatment), alpha = .2, show.legend = F) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  #ggtitle("before")  +
  #ylim(-200, 420) +
  # scale_colour_manual("", values = c('purple'))  +  
  # scale_fill_manual("", values = c('purple'))  + 
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))
p1.rss.sum.brick

p1.rss.brick<- ggplot(data=p1.rss[var == 'brickdist'], 
                      aes(step, rss, colour=treatment)) +
  geom_line(aes(group = snails2,alpha = .0001), size= .75,alpha = .5, linetype ='twodash', show.legend = F) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 2, aes(fill = treatment), se = F) +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  # ggtitle("after")  +
  #ylim(-200, 450) +
  scale_colour_colorblind()  +  
  scale_fill_colorblind()  + 
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 12))
p1.rss.brick


