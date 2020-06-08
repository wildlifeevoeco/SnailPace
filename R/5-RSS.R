### RSS ###
# Julie Turner
# started 13 May 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 
          'tidyr', 'ggplot2','survival','forcats', 'patchwork', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'
dat <- readRDS('Data/derived/ssa30ghosts.Rds')
dat.hr <- readRDS('Data/derived/ssa-hr-ghosts.Rds')
moveParams <- readRDS('Data/derived/moveParams-hr.Rds')
#dat <- dat[Stage!="Acc"] ## Can't limit to ToD=night because it won't work in interactions
dat$Stage <- factor(dat$Stage, levels = c("Acc", "B","A"))
dat$ToD_start <- as.factor(dat$ToD_start)
dat$Precipitation <- as.factor(dat$Precipitation)

dat.hr$Stage <- factor(dat.hr$Stage, levels = c("Acc", "B","A"))
dat.hr$ToD_start <- as.factor(dat.hr$ToD_start)
dat.hr$Precipitation <- as.factor(dat.hr$Precipitation)

# list of all snails
snails <- unique(dat$snail)


### FUNCTIONS ----
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



#### CORE ====
dat <- dat.hr
# Setup model name, explanatory and response variables
setup <- data.table(
  model = 'core',
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + strata(step_id_)'
)

# Which individuals should be dropped?
corebad <- 'P11a'
# setup[model == 'core', bad := snail %in% corebad]


# Run only on *good* individuals and those with > 0 rows
# setup[!(bad), n := 
#         dat[ghostbricks == .BY[[2]] & snail == .BY[[1]], .N],
#       by = .(snail, brick)]
# 
# setup[!(bad) & n != 0, mod := 
#         list_models(response, explanatory,
#                     dat[ghostbricks == .BY[[2]] & snail == .BY[[1]]]),
#       by = .(snail, brick)]

# Which bricks do we want to keep?
setup[model == 'core', lsbricks := list(c("C", "1", "2", "3"))]

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


move <- setup[,.(coef = unlist(coef), var = unlist(var), model), by = .(snail)]
move <- merge(move, moveParams, by = 'snail', all.x = T)

core.move <- copy(move)


core.mods <- setup[,.(snail, mod, model = model)]


# core_models[,"nbricks"] <- substr(core_models$snail, 3, 3)
# core_models[,"nbricks"] <- ifelse(core_models$nbricks=="4", "0", core_models$nbricks)
# core_models[,"Disturbance"] <- ifelse(core_models$nbricks=="0", "Control", "Disturbed")

#saveRDS(coreOUT, '~/snails/Data/derived/CoreModel.Rds')


### P1 and P2 ----
# Setup model name, explanatory and response variables
# Cross join individuals and bricks of interest
setup <- CJ(
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  model = 'p1',
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + log(edgedist_end + 1):Stage + log(brickdist_end + 1):Stage + log_sl:Stage + cos_ta:Stage + strata(step_id_)'
)


# Which individuals should be dropped?
p1bad.30mins <- c("P24b", "P11a", "P21a", "O12b", "O22b", "P12b", 
           "P22b", "P23a", "P23b", "O11a", "O13a")
p1bad <- c("O12b", "O14a", "O22b", "O24b", "O31a", "P21a", "P22b", "P23b", "P24b")
setup[model == 'p1', bad := snail %in% p1bad]


# Run only on *good* individuals and those with > 0 rows
setup[!(bad), n := 
      dat[ghostbricks == .BY[[2]] & snail == .BY[[1]], .N],
      by = .(snail, brick)]

setup[!(bad) & n != 0, mod := 
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
meanedge <- mean(dat$edgedist_end, na.rm = T)
maxedge <- max(dat$edgedist_end, na.rm = T)
meanbrick <- mean(dat$brickdist_end, na.rm = T)
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
setup[!(bad) & n != 0, h1Bedgedat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = seq(0, maxedge, length.out = 100),
                        brickdist_end = meanbrick,
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 after edge
setup[!(bad) & n != 0, h1Aedgedat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = meansl,
                         cos_ta = meanta,
                         ToD_start = factor('day', levels = levels(ToD_start)),
                         Temperature = meantemp,
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_end = seq(0, maxedge, length.out = 100),
                        brickdist_end = meanbrick,
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

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

setup[!(bad) & n != 0, h1Bedge := list(list(list_predict(mod, h1Bedgedat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xBedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(edgedist_end = seq(0, maxedge, length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Aedge := list(list(list_predict(mod, h1Aedgedat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xAedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(edgedist_end = seq(0, maxedge, length.out = 100))])),
      by = .(snail, brick)]
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


rss <- setup[!(bad) & n > 0,.(snail, brick, h1Bedge, h1Aedge, h1Bbrick, h1Abrick, h2B, h2A, 
                              xBedge, xAedge, xBbrick, xAbrick)]


rss[, rssBedge := dif_list(h1Bedge, h2B),
      by = .(snail, brick)]
rss[, stepBedge := list(list(seq(0,max(unlist(xBedge)), length.out = 100))),
    by = .( brick)]
rss[, rssAedge := dif_list(h1Aedge, h2A),
    by = .(snail, brick)]
rss[, stepAedge := list(list(seq(0,max(unlist(xAedge)), length.out = 100))),
    by = .( brick)]
rss[, rssBbrick := dif_list(h1Bbrick, h2B),
    by = .(snail, brick)]
rss[, stepBbrick := list(list(seq(0,max(unlist(xBbrick)), length.out = 100))),
    by = .( brick)]
rss[, rssAbrick := dif_list(h1Abrick, h2A),
    by = .(snail, brick)]
rss[, stepAbrick := list(list(seq(0,max(unlist(xAbrick)), length.out = 100))),
    by = .( brick)]

rss.long <- rss[, .(rss = unlist(rssBedge), x = unlist(xBedge), step = unlist(stepBedge), var = 'edgedist', BA = 'before', model = 'p1'),
    by = .(snail, brick)]
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAedge), x = unlist(xAedge), step = unlist(stepAedge), var = 'edgedist', BA = 'after', model = 'p1'),
                                by = .(snail, brick)])
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssBbrick), x = unlist(xBbrick), step = unlist(stepBbrick), var = 'brickdist', BA = 'before', model = 'p1'),
                                by = .(snail, brick)])
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAbrick), x = unlist(xAbrick), step = unlist(stepAbrick), var = 'brickdist', BA = 'after', model = 'p1'),
                                by = .(snail, brick)])

  
p1.rss <- copy(rss.long)


move <- setup[!(bad) & n > 0,.(coef = unlist(coef), var = unlist(var), model = 'p1'), by = .(snail, brick)]
move <- merge(move, moveParams, by = 'snail', all.x = T)

p1.move <- copy(move)
p1.move <- p1.move[!(var %like% 'Acc')]
core.move <- core.move[!(var %like% 'Acc')]

p1.mods <- setup[!(bad) & n > 0,.(snail, mod, model = paste(model, brick, sep = '.'))]

p1.issa <- setup[!(bad) & n > 0,.(snail, mod, issa, model = paste(model, brick, sep = '.'))]

# P1.g1.Out[,"nbricks"] <- 1
# P1.g2.Out[,"nbricks"] <- 2
# P1.g3.Out[,"nbricks"] <- 3
# P1.treats.Out[,"nbricks"] <- substr(P1.treats.Out$snail, 3, 3)
# 
# P1.g1.Out[,"Disturbance"] <- "Control"
# P1.g2.Out[,"Disturbance"] <- "Control"
# P1.g3.Out[,"Disturbance"] <- "Control"
# P1.treats.Out[,"Disturbance"] <- "Disturbed"
# 
# P1ModelOut <- rbind(P1.g1.Out, P1.g2.Out, P1.g3.Out, P1.treats.Out)

#saveRDS(P1ModelOut, '~/snails/Data/derived/P1Model.Rds')

### P1 graphs ----

p1.rss[,'disturbance'] <- ifelse(p1.rss$brick %like% 'g', 'undisturbed', 'disturbed')
p1.rss[,'snails2'] <- paste(p1.rss$snail, p1.rss$brick, sep = '.')
p1.rss[,disturbance.rss:=mean(rss, na.rm = T), by=.(step, disturbance)]
p1.rss[,brick.rss:=mean(rss), by=.(step, brick)]

p1.rss.edge.before <- ggplot(data=p1.rss[var == 'edgedist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
                         aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'edgedist'& BA=='before'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
 geom_smooth(size = 1.5, aes(fill = disturbance), se = F) +
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
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("before")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p1.rss.edge.after <- ggplot(data=p1.rss[var == 'edgedist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
                        aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = disturbance), se = F) +
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
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("after")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p1.rss.edge.before|p1.rss.edge.after


p1.rss.brick.before <- ggplot(data=p1.rss[var == 'brickdist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
                         aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  geom_smooth(size = 1.5, aes(fill = disturbance), se = F) +
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

p1.rss.brick.after <- ggplot(data=p1.rss[var == 'brickdist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
                        aes(x, rss, colour=disturbance)) +
  #geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
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

#### P1 graphs ----
p1.rss.edge.before|p1.rss.edge.after
p1.rss.brick.before|p1.rss.brick.after


### p2 graphs ---
p1.rss[,'brick2'] <-gsub("[^0-9.-]", "", p1.rss$brick)

p2.edge.before <- ggplot(data=p1.rss[var == 'edgedist'& BA=='before'], 
                         aes(x, rss, colour=brick2)) +
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_line(data=p1.rss[var == 'edgedist'& BA=='before'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
   #geom_smooth(size = 1.5, aes(fill = brick2), se =F) +
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
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("before")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.edge.after <- ggplot(data=p1.rss[var == 'edgedist'& BA=='after'], 
                        aes(x, rss, colour=brick2)) +
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_line(data=p1.rss[var == 'edgedist'& BA=='after'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  #geom_smooth(size = 1.5, aes(fill = brick2), se = F) +
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
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("after")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.edge.before|p2.edge.after


p2.brick.before <- ggplot(data=p1.rss[var == 'brickdist'& BA=='before'], 
                          aes(x, rss, colour=brick2)) +
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='before'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  #geom_smooth(size = 1.5, aes(fill = brick2), se =F) +
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
  ggtitle("before")  +
 # ylim(-100,50) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.brick.after <- ggplot(data=p1.rss[var == 'brickdist'& BA=='after'], 
                         aes(x, rss, colour=brick2)) +
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  # geom_smooth(size = 1.5, aes(fill = brick2), se = F) +
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
  ggtitle("after")  +
  #ylim(-100,150) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

#### P2 graphs ####
p2.edge.before|p2.edge.after
p2.brick.before|p2.brick.after

#### P3 and P4 ====

# Setup model name, explanatory and response variables
# Cross join individuals and bricks of interest
setup <- CJ(
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  model = 'p3',
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl +
  log(edgedist_start + 1):log_sl:Stage + log(edgedist_start + 1):cos_ta:Stage + 
  log(brickdist_start + 1):log_sl:Stage + log(brickdist_start + 1):cos_ta:Stage + 
  log_sl:Stage + cos_ta:Stage + strata(step_id_)'
)
# 
# setup <- CJ(
#   snail = unique(dat$snail),
#   brick = c("1", "2", "3", "g1", "g2", "g3"),
#   model = 'p3',
#   response = 'case_',
#   explanatory = 'log_sl + cos_ta + ToD_start:log_sl +
#   log(brickdist_start + 1):log_sl:Stage + log(brickdist_start + 1):cos_ta:Stage + 
#   log_sl:Stage + cos_ta:Stage + strata(step_id_)'
# )


# Which individuals should be dropped?
p3bad.30 <- c("P24b", "O24a", "P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "O13a", "P23a", "P23b", "O22a", "P13a",
           "P14a", "P22a", "P24a", "P31a")
#p3bad <- c('O11a','O12b', 'O13a', 'O14a','O22b', 'O23a', 'O24a', 'O24b', 'O31a', 'P13a','P14a', 'P21a', 'P22b', 'P23b', 'P24b')
p3bad <- c('O12b', 'O13a', 'O14a','O22b', 'O23a', 'O24a', 'O24b', 'O31a', 'P13a', 'P21a', 'P22b', 'P23b', 'P24b')

setup[model == 'p3', bad := snail %in% p3bad]


# Run only on *good* individual and those with > 0 rows
setup[, n := 
        dat[ghostbricks == .BY[[2]] & snail == .BY[[1]], .N],
      by = .(snail, brick)]

setup[!(bad) & n != 0, mod := 
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


setup[!(bad) & n != 0, issacoef := calc_coef(issa),
      by = .(snail, brick)]

p3.mods <- setup[!(bad) & n > 0 & !(is.null(mod)),.(snail, mod, model = paste(model, brick, sep = '.'))]

p3.issa <- setup[!(bad) & n > 0 & !(is.null(mod)),.(snail, mod, issa, model = paste(model, brick, sep = '.'))]
#p3.issa[, sl_distr_params(unlist(unlist(issa))), by = .(snail)]

move <- setup[!(bad) & n > 0,.(coef = unlist(coef), var = unlist(var), model = 'p3'), by = .(snail, brick)]


p3.move <- copy(move)

p3.move[,'BA'] <- ifelse(p3.move$var %like% 'StageAcc', 'acc', 
                         ifelse(p3.move$var %like% 'StageA', 'after',
                                ifelse(p3.move$var %like% 'StageB', 'before', NA)))
# p3.move <- p3.move[BA != 'acc']


p3.move <- p3.move[!(var %like% 'StageAcc')]
p3.move$var <- gsub(':', '-', p3.move$var)
p3.move$var <- gsub(' ', '', p3.move$var)
p3.move$var <- gsub('[[:punct:]]', '', p3.move$var)
p3.move$var <- gsub('StageA', '_after', p3.move$var)
p3.move$var <- gsub('StageB', '_before', p3.move$var)
p3.move$var <- gsub('logbrickdiststart1', '_brickdist', p3.move$var)
p3.move$var <- gsub('logedgediststart1', '_edgedist', p3.move$var)
unique(p3.move$var)

p3.move[,'snails2'] <- paste(p3.move$snail, p3.move$brick, sep = '.')


p3.wide <- dcast(data =p3.move, snail + brick ~ var, value.var = 'coef')

p3.wide <- setDT(merge(p3.wide, moveParams, by = 'snail', all.x = T))


edist <- seq(0,maxedge, length.out = 100)
bdist <- seq(0,maxbrick, length.out = 100)

p3.wide[!(is.na(logsl)), ed.spd.before:= list(list((shape+logsl_before+(logsl_before_edgedist*edist))*(scale))), by=.(snail, brick)]
p3.wide[!(is.na(logsl)), ed.spd.after:= list(list((shape+logsl_after+(logsl_after_edgedist*edist))*(scale))), by=.(snail, brick)]

p3.wide[!(is.na(logsl)), bd.spd.before:= list(list((shape+logsl_before+(logsl_before_brickdist*bdist))*(scale))), by=.(snail, brick)]
p3.wide[!(is.na(logsl)), bd.spd.after:= list(list((shape+logsl_after+(logsl_after_brickdist*bdist))*(scale))), by=.(snail, brick)]


p3.wide[!(is.na(logsl)), ed.dir.before:= list(list((kappa + costa_before + (costa_before_edgedist*edist)))), by=.(snail, brick)]
p3.wide[!(is.na(logsl)), ed.dir.after:= list(list((kappa + costa_after + (costa_after_edgedist*edist)))), by=.(snail, brick)]

p3.wide[!(is.na(logsl)), bd.dir.before:= list(list((kappa + costa_before + (costa_before_brickdist*bdist)))), by=.(snail, brick)]
p3.wide[!(is.na(logsl)), bd.dir.after:= list(list((kappa + costa_after + (costa_after_brickdist*bdist)))), by=.(snail, brick)]


p3.wide[!(is.na(logsl)), edist:= list(list(seq(0,maxedge, length.out = 100))), by=.(snail, brick)]
p3.wide[!(is.na(logsl)), bdist:= list(list(seq(0,maxbrick,length.out = 100))), by=.(snail, brick)]

speed <- p3.wide[!(is.na(logsl)),.(edist = unlist(edist), bdist = unlist(bdist), ed.spd.before = unlist(ed.spd.before), ed.spd.after = unlist(ed.spd.after),
                                   bd.spd.before = unlist(bd.spd.before), bd.spd.after = unlist(bd.spd.after),
                                   disturbance = ifelse(brick %like% 'g', 'undisturbed', 'disturbed')), by = .(snail, brick)]
speed[,'snails2'] <- paste(speed$snail, speed$brick, sep = '.')
speed[,'brick2'] <-gsub("[^0-9.-]", "", speed$brick)

direction <- p3.wide[!(is.na(logsl)),.(edist = unlist(edist), bdist = unlist(bdist), ed.dir.before = unlist(ed.dir.before), ed.dir.after = unlist(ed.dir.after),
                                   bd.dir.before = unlist(bd.dir.before), bd.dir.after = unlist(bd.dir.after),
                                   disturbance = ifelse(brick %like% 'g', 'undisturbed', 'disturbed')), by = .(snail, brick)]
direction[,'snails2'] <- paste(direction$snail, direction$brick, sep = '.')
direction[,'brick2'] <-gsub("[^0-9.-]", "", direction$brick)

speed.edge.before <- ggplot(data=speed[snail != 'P23a'], aes(x=edist, y=ed.spd.before/50, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
 # geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from edge (cm)") + ylab("Speed (m per hour)")
speed.edge.before 

speed.edge.after <- ggplot(data=speed[snail != 'P12a'], aes(x=edist, y=ed.spd.after/50, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
 # geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from edge (cm)") + ylab("Speed (m per hour)")
speed.edge.after 



speed.brick.before <- ggplot(data=speed[snail != 'O22a'], aes(x=bdist, y=bd.spd.before/50, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
 # geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from brick (cm)") + ylab("Speed (m per hour)")
speed.brick.before 

speed.brick.after <- ggplot(data=speed[snail!='P14a' & snail!='P12a'], aes(x=bdist, y=bd.spd.after/50, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  #geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from brick (cm)") + ylab("Speed (m per hour)")
speed.brick.after 



direction.edge.before <- ggplot(data=direction[snail != 'P23a' &snail != 'P22a'], aes(x=edist, y=ed.dir.before, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  #geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from edge (cm)") + ylab("direction")
direction.edge.before 

direction.edge.after <- ggplot(data=direction, aes(x=edist, y=ed.dir.after, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  #geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from edge (cm)") + ylab("direction")
direction.edge.after 



direction.brick.before <- ggplot(data=direction, aes(x=bdist, y=bd.dir.before, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
 # geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from brick (cm)") + ylab("direction")
direction.brick.before 

direction.brick.after <- ggplot(data=direction[snail != 'O22a'], aes(x=bdist, y=bd.dir.after, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  #geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from brick (cm)") + ylab("direction")
direction.brick.after 


#### Disturbance graphs ----
speed.edge.before|speed.edge.after
speed.brick.before|speed.brick.after
direction.edge.before|direction.edge.after
direction.brick.before|direction.brick.after

dat[,.(min = min(sl_, na.rm = T), max = max(sl_, na.rm = T), mean = mean(sl_, na.rm = T)), by = .(snail)]
dat[,.(min = min(ta_, na.rm = T), max = max(ta_, na.rm = T), mean = mean(ta_, na.rm = T)), by = .(snail)]

dat.obs <- dat[case_==TRUE]
hist(dat$sl_)
hist(dat.obs$sl_)
hist(dat$ta_)
hist(dat.obs$ta_)

ggplot(dat, aes(sl_, colour = snail)) +
  geom_density()

sl <- ggplot(dat.obs, aes(sl_, colour = snail)) +
  geom_density()

ggplot(dat, aes(ta_, colour = ToD_start)) +
  geom_density()

ta <- ggplot(dat.obs, aes(ta_, colour = snail)) +
  geom_density()
sl|ta

ggplot(dat[case_==TRUE], aes(sl_)) +
  geom_density(color='blue') + geom_histogram(bins = 500) +
  ylim(0, 10) +
  facet_wrap(vars(snail))

ggplot(dat[case_==TRUE], aes(sl_, y = ..density..)) +
 #geom_freqpoly(color='blue', bins= 50) + 
  geom_histogram(bins = 20) +
 # geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) +
  facet_wrap(vars(snail))

ggplot(dat[case_==TRUE & snail=='O11a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=20) +
  geom_line(aes(x=sl_, y=dgamma(sl_, gam[param =="shape" & snail=='O11a', vals], gam[param =="rate" & snail=='O11a', vals])), color="red", size = 1) 


snails
gam <- dat[case_==TRUE,.(vals = MASS::fitdistr(sl_, "gamma")[[1]],
                         param = names(MASS::fitdistr(sl_, "gamma")[[1]])), by = .(snail)]
gam.wide <- dcast(gam, snail ~ param, value.var = "vals")

gam.snails <- merge(dat[case_==TRUE,.(snail, sl_)], gam.wide, by = c('snail'), all.x = T)

ggplot(gam.snails) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  facet_wrap(vars(snail))

ggplot(gam.snails[snail==snails[1]]) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle(gam.snails$snail)

o11a <-ggplot(gam.snails[snail=='O11a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O11a')

o11b <-ggplot(gam.snails[snail=='O11b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O11b')

o12b <-ggplot(gam.snails[snail=='O12b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O12b')

o13a <-ggplot(gam.snails[snail=='O13a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O13a')

o14a <-ggplot(gam.snails[snail=='O14a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O14a')

o22a <-ggplot(gam.snails[snail=='O22a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=20) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O22a')

o22b <-ggplot(gam.snails[snail=='O22b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O22b')

o23a <-ggplot(gam.snails[snail=='O23a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O23a')

o24a <-ggplot(gam.snails[snail=='O24a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O24a')

o24b <-ggplot(gam.snails[snail=='O24b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O24b')

o31a <-ggplot(gam.snails[snail=='O31a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=10) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O31a')

p12a <-ggplot(gam.snails[snail=='P12a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P12a')

p13a <-ggplot(gam.snails[snail=='P13a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P13a')

p14a <-ggplot(gam.snails[snail=='P14a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P14a')

p21a <-ggplot(gam.snails[snail=='P21a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P21a')

p22a <-ggplot(gam.snails[snail=='P22a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P22a')

p22b <-ggplot(gam.snails[snail=='P22b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P22b')

p23a <-ggplot(gam.snails[snail=='P23a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P23a')

p23b <-ggplot(gam.snails[snail=='P23b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P23b')

p24a <-ggplot(gam.snails[snail=='P24a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P24a')

p24b <-ggplot(gam.snails[snail=='P24b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=10) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P24b')

p31a <-ggplot(gam.snails[snail=='P31a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P31a')


(o11a|o11b|o12b|o13a|o14a)/(o22a|o22b|o23a|o24a|o24b)/(o31a|p12a|p13a|p14a|p21a)/
  (p22a|p22b|p23a|p23b|p24a)/(p24b|p31a)

#### ALL MODELS ####

#all.mods <- rbind(core.mods, p1.mods, p3.mods)
#saveRDS(all.mods, 'Data/derived/allMods_hr.Rds')
all.mods <- readRDS('Data/derived/allMods_hr.Rds')

all.mods[, nMods := .N, by = snail]
tab <- all.mods[nMods > 1, calc_aictab(mod, model), by = .(snail)]
evi <- all.mods[nMods > 1 & !(model %like% 'g1') & !(model %like% 'g3'), evidence(calc_aictab(mod, model)), by = .(snail)]



