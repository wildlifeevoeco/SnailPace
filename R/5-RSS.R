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
# dat <- readRDS('Data/derived/ssa30ghosts.Rds')
dat.hr <- readRDS('Data/derived/ssa-exp-good-ghosts.Rds')
dat.2hr <- readRDS('Data/derived/ssa-exp2hr-goods-ghosts.Rds')
moveParams <- readRDS('Data/derived/moveParams-exp-goods.Rds')
moveParams.2hr <- readRDS('Data/derived/moveParams-exp2hr-goods.Rds')
#dat <- dat[Stage!="Acc"] ## Can't limit to ToD=night because it won't work in interactions
# dat$Stage <- factor(dat$Stage, levels = c("Acc", "B","A"))
# dat$ToD_start <- as.factor(dat$ToD_start)
# dat$Precipitation <- as.factor(dat$Precipitation)

dat.hr$Stage <- factor(dat.hr$Stage, levels = c("Acc", "B","A"))
dat.hr$ToD_start <- as.factor(dat.hr$ToD_start)
dat.hr$Precipitation <- as.factor(dat.hr$Precipitation)
#dat.hr$log_sl <- ifelse(is.finite(dat.hr$log_sl), dat.hr$log_sl, -50)
trusted.sl <- dat.hr[case_==TRUE & sl_>=0.001,.(snail,step_id_)]
dat.hr.trusted <- merge(dat.hr,trusted.sl, by = c('snail', 'step_id_'))


dat.2hr$Stage <- factor(dat.2hr$Stage, levels = c("Acc", "B","A"))
dat.2hr$ToD_start <- as.factor(dat.2hr$ToD_start)
dat.2hr$Precipitation <- as.factor(dat.2hr$Precipitation)
#dat.2hr$log_sl <- ifelse(is.finite(dat.2hr$log_sl), dat.2hr$log_sl, -50)
trusted.sl <- dat.2hr[case_==TRUE & sl_>=0.001,.(snail,step_id_)]
dat.2hr.trusted <- merge(dat.2hr,trusted.sl, by = c('snail', 'step_id_'))

# list of all snails
snails <- unique(dat.hr$snail)


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
dat <- dat.2hr
# Setup model name, explanatory and response variables
setup <- data.table(
  model = 'core',
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + strata(step_id_)'
)

# Which individuals should be dropped?
#corebad <- c('P22b') #, 'P13a') # good
#corebad <- c('O24a') #all
# corebad <- c('O22a','O22b', 'O24a','O23a','O24b')
#corebad <- c('P31a', 'P14a') #2hr all
# corebad <- c('P14a') #2hr all exp
# corebad <- c('O12b', 'O24b') #2hr all exp only moving
# no bad when 1hr and 2hr exp only moving steps ****
# corebad <- c('P31a') # 2hr gam goods all
# corebad <- c('O11a', 'O11b', 'P21a') # exp, goods, trusted 1hr
# corebad <- c('P31a', 'P22b', 'P13a', 'P14a') # exp all 1 hr
#corebad <- c('O22a', 'O24a') # exp 1 hr trusted

#corebad <- c('P31a', 'P22b', 'P14a') # exp 1 hr goods all
corebad <- c('P13a', 'P14a') # 2hr exp goods all

setup[model == 'core', bad := snail %in% corebad]



# Which bricks do we want to keep?
setup[ model == 'core', lsbricks := list(c("C", "1", "2", "3"))]

# Run only on *good* individual
setup[!(bad), mod :=
        list_models(response, explanatory,
                    dat[ghostbricks %in% unlist(lsbricks) &
                          snail == .BY[[1]]]),
      by = snail]

setup[!(bad), issa :=
        list_issa(response, explanatory,
                    dat[ghostbricks %in% unlist(lsbricks) &
                          snail == .BY[[1]]]),
      by = snail]

# coefs 

setup[!(bad), coef := calc_coef(mod),
      by = .(snail)]


setup[!(bad), var := calc_coef_names(mod),
      by = .(snail)]


move <- setup[!(bad),.(coef = unlist(coef), var = unlist(var), model), by = .(snail)]
move <- merge(move, moveParams, by = 'snail', all.x = T)

core.move.2hr <- copy(move)

core.issa.2hr <- setup[!(bad),.(snail, issa, model = model)]
core.mods.2hr <- setup[!(bad),.(snail, mod, model = model)]


unique(core.mods$snail)
unique(core.mods.2hr$snail)

core.used <- c('O11a', 'O22b', 'O24b', 'P12a', 'P22a', 'O24a', 'P13a', 'P21a', 'O31a', 
               'O12b', 'O22a', 'P23b', 'O14a', 'P24b')
core.used.2hr <- c('O11b', 'O23a', 'P24a', 'P23a', 'P31a', 'O13a', 
                   'P22b')
core.mods[, samp := '1hr']
core.mods.2hr[, samp := '2hr']
core.mods.all <- rbind(core.mods[snail %in% core.used],
                    core.mods.2hr[snail %in% core.used.2hr])

mod.used.all <- unique(p1.mods.all$snail)
bad.snail.all <- dat.hr[!(snail %in% mod.used.all), unique(snail)]
dat.hr[snail %in% bad.snail.all, mean(sl_), by = .(snail)]
dat.hr[!(snail %in% bad.snail.all), mean(sl_), by = .(snail)]

dat.hr[snail %in% bad.snail.all & sl_<0.001, .N, by = .(snail)]
dat.hr[!(snail %in% bad.snail.all) & sl_<0.001, .N, by = .(snail)]

nomove.1hr <- rbind(dat.hr[snail %in% bad.snail.all, .(nSteps = .N, nomove.rate = .SD[sl_<1,.N]/.N, minSL = quantile(sl_, 0.25), meanSL = mean(sl_), work = 'bad', samp = '1hr'), by = .(snail)],
  dat.hr[!(snail %in% bad.snail.all), .(nSteps = .N, nomove.rate = .SD[sl_<1,.N]/.N, minSL = quantile(sl_, 0.25), meanSL = mean(sl_), work = 'good', samp = '1hr'), by = .(snail)])
nomove.2hr <- rbind(dat.2hr[snail %in% bad.snail.all, .(nSteps = .N, nomove.rate = .SD[sl_<1,.N]/.N, minSL = quantile(sl_, 0.25), meanSL = mean(sl_), work = 'bad', samp = '2hr'), by = .(snail)],
                dat.2hr[!(snail %in% bad.snail.all), .(nSteps = .N, nomove.rate = .SD[sl_<1,.N]/.N, minSL = quantile(sl_, 0.25), meanSL = mean(sl_), work = 'good', samp = '2hr'), by = .(snail)])
nomove <- rbind(nomove.1hr, nomove.2hr)

nomove[,mean(nomove.rate), by=.(work)]
nomove[,mean(meanSL), by=.(work)]
nomove[,mean(minSL), by=.(work)]
nomove[,mean(nSteps), by=.(work)]
t.test(nomove.rate ~ work, nomove)
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
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl  + log(edgedist_end + 1):Stage + log(brickdist_end + 1):Stage + log_sl:Stage + cos_ta:Stage + strata(step_id_)'
)


# Which individuals should be dropped?
# p1bad.30mins <- c("P24b", "P11a", "P21a", "O12b", "O22b", "P12b", 
#            "P22b", "P23a", "P23b", "O11a", "O13a")

#p1bad <- c('O11b', "O12b", 'O13a', "O14a", "O22b", 'O24a', "O31a", 'P12a', 'P13a', "P21a", "P22b", "P23b", "P24b")

#p1bad <- c('P22b', 'P13a', "O12b", "O14a", 'O22a', 'O31a', "P21a", 'P23a', "P23b", "P24b", 'P31a')
#p1bad <- c('P22b', 'P13a', "O12b", "O14a", 'O31a', 'P14a', "P21a", "P23b", "P24b") #exp all

#p1bad <- c('P22b', 'P13a', 'O11b', "O12b", "O14a", 'O22a', 'P12a', "P21a", 'P22a', 'P23a', "P23b", "P24b")

#p1bad <- c('O24b', "O12b", "O14a", 'O22b', 'P12a', 'P14a', "P21a", 'P22a', 'P22b', "P23b", "P24b")
#p1bad <- c('O24a', "O12b", 'O13a', "O14a", 'O22b', 'O24b', 'O31a', 'P13a', 'P14a', "P21a", 'P22a', 'P22b', "P23b", 'P24a', 'P31a') # exp no 0 steps

#p1bad <- c('P22b', 'O11a', 'O12b', "O14a", 'P21a', 'P23b') #exp good
#p1bad <- c('P31a', 'P14a', "O12b", "O14a", 'O23a', 'O24a', 'O24b', 'O31a', 'P12a', 'P13a', 'P14a', 'P21a', 'P22b', 'P23b') #2hr all
#p1bad <- c('P14a', 'O12b', 'O13a', 'O14a', 'O22a', 'O24b', 'O31a', 'P13a', 'P21a', 'P22b', 'P23a', 'P23b', 'P31a') #2hr all exp
#p1bad <- c('O12b', 'O24b', 'O13a', 'O14a', 'O22a', 'O22b', 'O24a', 'O31a', 'P12a', 'P13a', 'P21a', 'P22a', 'P22b', 'P23a', 'P23b', 'P24a', 'P24b') #2hr all exp only moving
#p1bad <- c('O12b', 'O13a', 'O14a', 'O22a', 'O22b', 'O24a', 'O24b', 'O31a', 'P12a', 'P13a', 'P14a', 'P21a', 'P22b', 'P23a', 'P23b', 'P24a', 'P24b') # 2hr exp goods moving only
#p1bad <- c('P31a', 'P12a', 'P13a', 'P14a', 'O14a', 'O24b', 'O31a', 'O12b', 'O13a', 'P21a', 'P22b', 'P23b', 'P24b') # 2hr gam goods all
# p1bad <- c('O12b', "O14a", 'O22a', 'O22b', 'P12a','P14a', 'P21a', 'P22b', 'P23b', 'P24b') # 1 hr exp good trusted
# p1bad <- c('P31a', 'P22b', 'P13a', 'P14a', 'O11b', 'O12b', 'O14a', 'O22a', 'O22b', 'O24b', 'O31a', 'P21a', 'P23a', 'P23b', 'P24b') # exp all 1 hr
#p1bad <- c('O22a', 'O24a', 'O12b', 'O13a', 'O14a', 'O22b', 'O31a', 'P13a', 'P21a', 'P22a', 'P22b', 'P23a', 'P23b', 'P24a', 'P24b') # exp 1 hr trusted


#p1bad <- c('P31a', 'P22b', 'P14a', 'O11b', 'O12b', 'O13a', 'O14a', 'O22a', 'O31a', 'P14a', 'P21a', 'P23b', 'P24b') # exp 1 hr good
p1bad <- c('P13a', 'P14a', 'O12b', 'O14a', 'O22a', 'O22b', 'O24b', 'O31a', 'P12a', 'P21a', 'P22b', 'P23b', 'P24b') # 2hr exp goods all

setup[model == 'p1', bad := snail %in% p1bad]


# Run only on *good* individuals and those with > 0 rows
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

  
p1.rss.2hr <- copy(rss.long)


move <- setup[!(bad) & n > 0,.(coef = unlist(coef), var = unlist(var), model = 'p1'), by = .(snail, brick)]
move <- merge(move, moveParams.2hr, by = 'snail', all.x = T)

p1.move.2hr <- copy(move)
p1.move.2hr <- p1.move[!(var %like% 'Acc')]
core.move.2hr <- core.move[!(var %like% 'Acc')]

p1.mods.2hr <- setup[!(bad) & n > 0,.(snail, mod, model = paste(model, brick, sep = '.'))]

p1.issa.2hr <- setup[!(bad) & n > 0,.(snail, mod, issa, model = paste(model, brick, sep = '.'))]

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

##### check who from each model (1 or 2hr) ----
unique(p1.rss$snail)
unique(p1.rss.2hr$snail)

p1.used <- c('O11a', 'O22b', 'O24b', 'P12a', 'P22a', 'O24a', 'P13a')
p1.used.2hr <- c('O11b', 'O23a', 'P24a', 'P23a', 'P31a', 'O13a')
p1.rss[, samp := '1hr']
p1.rss.2hr[, samp := '2hr']
p1.rss.all <- rbind(p1.rss[snail %in% p1.used],
                    p1.rss.2hr[snail %in% p1.used.2hr])

#saveRDS(p1.rss.all, 'Data/derived/p1rss_1-2hr.Rds')

p1.mods.all <- rbind(p1.mods[snail %in% p1.used],
                    p1.mods.2hr[snail %in% p1.used.2hr])

### P1 graphs ----

# p1.rss.2hr[,'disturbance'] <- ifelse(p1.rss.2hr$brick %like% 'g', 'undisturbed', 'disturbed')
# p1.rss.2hr[,'snails2'] <- paste(p1.rss.2hr$snail, p1.rss.2hr$brick, sep = '.')
# p1.rss.2hr[,disturbance.rss:=mean(rss, na.rm = T), by=.(step, disturbance)]
# p1.rss.2hr[,brick.rss:=mean(rss), by=.(step, brick)]

p1.rss.edge.before <- ggplot(data=p1.rss.all[var == 'edgedist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
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

p1.rss.edge.after <- ggplot(data=p1.rss.all[var == 'edgedist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
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


p1.rss.brick.before <- ggplot(data=p1.rss.all[var == 'brickdist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
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

p1.rss.brick.after <- ggplot(data=p1.rss.all[var == 'brickdist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
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
#p1.rss.2hr[,'brick2'] <-gsub("[^0-9.-]", "", p1.rss.2hr$brick)

p2.edge.before <- ggplot(data=p1.rss.all[var == 'edgedist'& BA=='before'], 
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

p2.edge.after <- ggplot(data=p1.rss.all[var == 'edgedist'& BA=='after'], 
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


p2.brick.before <- ggplot(data=p1.rss.all[var == 'brickdist'& BA=='before'], 
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

p2.brick.after <- ggplot(data=p1.rss.all[var == 'brickdist'& BA=='after'], 
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
# p3bad.30 <- c("P24b", "O24a", "P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "O13a", "P23a", "P23b", "O22a", "P13a",
#            "P14a", "P22a", "P24a", "P31a")

#p3bad <- c('O11a','O12b', 'O13a', 'O14a','O22b', 'O23a', 'O24a', 'O24b', 'O31a', 'P13a','P14a', 'P21a', 'P22b', 'P23b', 'P24b')

# p3bad <- c('O11b', 'O12b', 'O13a', 'O14a','O22b', 'O23a', 'O24a', 'O24b', 'O31a', 'P12a', 'P13a', 'P14a', 'P21a', 
#            'P22b', 'P23a', 'P23b', 'P24b')
  
#p3bad <- c('P22b', 'P13a', 'O12b', 'O14a', 'O23a', 'O24a', 'O31a', 'P21a', 'P23b', 'P24b')
#p3bad <- c('P22b', 'P13a', 'O11a','O12b', 'O13a', 'O14a')
#p3bad <- c('P31a', 'P14a', 'O11b','O12b', 'O14a', 'O23a', 'O24a', 'O24b', 'P21a', 'P22a', 'O31a', 'P12a', 'P22b', 'P23a', 'P23b', 'P24a', 'P24b') #2hr all
#p3bad <- c('P14a', 'O12b', 'O14a', 'O22a', 'O23a', 'O24a', 'O24b', 'O31a', 'P12a', 'P13a', 'P21a', 'P22b', 'P23a', 'P23b', 'P24b') #2hr all exp
#p3bad <- c('P31a', 'P12a', 'P13a', 'P14a', 'O14a', 'O24b', 'O31a', 'O12b', 'O22a', 'O23a', 'O24a', 'P21a', 'P22b','P23b', 'P24b') # 2hr gam goods all
#p3bad <- c('O22a', 'O24a', 'O11b', 'O12b', 'O13a', 'O14a', 'O22b', 'O24b', 'O31a', 'P12a', 'P13a', 'P21a', 'P22a', 'P22b','P23b', 'P24a', 'P24b') # exp 1 hr trusted

#p3bad <- c('P31a', 'P22b', 'P14a', 'P13a', 'O13a', 'P24a','O24a', 'O31a', 'O12b', 'O14a', 'O22a', 'O22b', 'P21a', 'P22b', 'P23a','P23b', 'P24b') # exp 1 hr good
p3bad <- c('P13a', 'P14a', 'O12b', 'O13a', 'O14a', 'O22a', 'O24a', 'O24b', 'O31a', 'P21a', 'P22b', 'P23b', 'P24b') # 2hr exp goods all

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

p3.mods.2hr <- setup[!(bad) & n > 0 & !(is.null(mod)),.(snail, mod, model = paste(model, brick, sep = '.'))]

p3.issa.2hr <- setup[!(bad) & n > 0 & !(is.null(mod)),.(snail, mod, issa, model = paste(model, brick, sep = '.'))]
#p3.issa[, sl_distr_params(unlist(unlist(issa))), by = .(snail)]

move <- setup[!(bad) & n > 0,.(coef = unlist(coef), var = unlist(var), model = 'p3'), by = .(snail, brick)]


p3.move.2hr <- copy(move)

p3.move.2hr[,'BA'] <- ifelse(p3.move.2hr$var %like% 'StageAcc', 'acc', 
                         ifelse(p3.move.2hr$var %like% 'StageA', 'after',
                                ifelse(p3.move.2hr$var %like% 'StageB', 'before', NA)))
# p3.move <- p3.move[BA != 'acc']



p3.move.2hr <- p3.move.2hr[!(var %like% 'StageAcc')]
p3.move.2hr$var <- gsub(':', '-', p3.move.2hr$var)
p3.move.2hr$var <- gsub(' ', '', p3.move.2hr$var)
p3.move.2hr$var <- gsub('[[:punct:]]', '', p3.move.2hr$var)
p3.move.2hr$var <- gsub('StageA', '_after', p3.move.2hr$var)
p3.move.2hr$var <- gsub('StageB', '_before', p3.move.2hr$var)
p3.move.2hr$var <- gsub('logbrickdiststart1', '_brickdist', p3.move.2hr$var)
p3.move.2hr$var <- gsub('logedgediststart1', '_edgedist', p3.move.2hr$var)
unique(p3.move.2hr$var)

p3.move.2hr[,'snails2'] <- paste(p3.move.2hr$snail, p3.move.2hr$brick, sep = '.')


p3.wide.2hr <- dcast(data =p3.move.2hr, snail + brick ~ var, value.var = 'coef')

p3.wide.2hr <- setDT(merge(p3.wide.2hr, moveParams.2hr, by = 'snail', all.x = T))


edist <- seq(0,maxedge, length.out = 100)
bdist <- seq(0,maxbrick, length.out = 100)

# gamma distribution: shape and scale for speed
# exponential: shape = 1, scale =1/rate

p3.wide.2hr[!(is.na(logsl)), ed.spd.before:= list(list((1+logsl_before+(logsl_before_edgedist*edist))*(1/rate))), by=.(snail, brick)]
p3.wide.2hr[!(is.na(logsl)), ed.spd.after:= list(list((1+logsl_after+(logsl_after_edgedist*edist))*(1/rate))), by=.(snail, brick)]

p3.wide.2hr[!(is.na(logsl)), bd.spd.before:= list(list((1+logsl_before+(logsl_before_brickdist*bdist))*(1/rate))), by=.(snail, brick)]
p3.wide.2hr[!(is.na(logsl)), bd.spd.after:= list(list((1+logsl_after+(logsl_after_brickdist*bdist))*(1/rate))), by=.(snail, brick)]


p3.wide.2hr[!(is.na(logsl)), ed.dir.before:= list(list((kappa + costa_before + (costa_before_edgedist*edist)))), by=.(snail, brick)]
p3.wide.2hr[!(is.na(logsl)), ed.dir.after:= list(list((kappa + costa_after + (costa_after_edgedist*edist)))), by=.(snail, brick)]

p3.wide.2hr[!(is.na(logsl)), bd.dir.before:= list(list((kappa + costa_before + (costa_before_brickdist*bdist)))), by=.(snail, brick)]
p3.wide.2hr[!(is.na(logsl)), bd.dir.after:= list(list((kappa + costa_after + (costa_after_brickdist*bdist)))), by=.(snail, brick)]


p3.wide.2hr[!(is.na(logsl)), edist:= list(list(seq(0,maxedge, length.out = 100))), by=.(snail, brick)]
p3.wide.2hr[!(is.na(logsl)), bdist:= list(list(seq(0,maxbrick,length.out = 100))), by=.(snail, brick)]

speed.2hr <- p3.wide.2hr[!(is.na(logsl)),.(edist = unlist(edist), bdist = unlist(bdist), ed.spd.before = unlist(ed.spd.before), ed.spd.after = unlist(ed.spd.after),
                                   bd.spd.before = unlist(bd.spd.before), bd.spd.after = unlist(bd.spd.after),
                                   disturbance = ifelse(brick %like% 'g', 'undisturbed', 'disturbed')), by = .(snail, brick)]
speed.2hr[,'snails2'] <- paste(speed.2hr$snail, speed.2hr$brick, sep = '.')
speed.2hr[,'brick2'] <-gsub("[^0-9.-]", "", speed.2hr$brick)

direction.2hr <- p3.wide.2hr[!(is.na(logsl)),.(edist = unlist(edist), bdist = unlist(bdist), ed.dir.before = unlist(ed.dir.before), ed.dir.after = unlist(ed.dir.after),
                                   bd.dir.before = unlist(bd.dir.before), bd.dir.after = unlist(bd.dir.after),
                                   disturbance = ifelse(brick %like% 'g', 'undisturbed', 'disturbed')), by = .(snail, brick)]
direction.2hr[,'snails2'] <- paste(direction.2hr$snail, direction.2hr$brick, sep = '.')
direction.2hr[,'brick2'] <-gsub("[^0-9.-]", "", direction.2hr$brick)

##### check who from each model (1 or 2hr) ----
unique(p3.wide$snail)
unique(p3.wide.2hr$snail)
speed[ed.spd.before<0, unique(snail)]
speed.2hr[ed.spd.before<0, unique(snail)]

p3.used <- c('O24b', 'O11a', 'P22a')
p3.used.2hr <- c('O11b', 'P24a', 'O23a', 'P12a', 'O22b', 'P23a', 'P31a')

speed.used <- speed[snail %in% p3.used, .(edist, bdist, ed.spd.before = ed.spd.before/100, ed.spd.after = ed.spd.after/100,
                                          bd.spd.before = bd.spd.before/100, bd.spd.after = bd.spd.after/100,
                                          disturbance, snail, snails2, brick, brick2, samp = '1hr')]

speed.used.2hr <- speed.2hr[snail %in% p3.used.2hr, .(edist, bdist, ed.spd.before = ed.spd.before/50, ed.spd.after = ed.spd.after/50,
                                          bd.spd.before = bd.spd.before/50, bd.spd.after = bd.spd.after/50,
                                          disturbance, snail, snails2, brick, brick2, samp = '2hr')]

speed.all <- rbind(speed.used, speed.used.2hr)
#saveRDS(speed.all, 'Data/derived/speed_1-2hr.Rds')

direction.used <- direction[snail %in% p3.used, .(edist, bdist, ed.dir.before = ed.dir.before, ed.dir.after = ed.dir.after,
                                          bd.dir.before = bd.dir.before, bd.dir.after = bd.dir.after,
                                          disturbance, snail, snails2, brick, brick2, samp = '1hr')]

direction.used.2hr <- direction.2hr[snail %in% p3.used.2hr, .(edist, bdist, ed.dir.before = ed.dir.before, ed.dir.after = ed.dir.after,
                                                      bd.dir.before = bd.dir.before, bd.dir.after = bd.dir.after,
                                                      disturbance, snail, snails2, brick, brick2, samp = '2hr')]

direction.all <- rbind(direction.used, direction.used.2hr)
#saveRDS(direction.all, 'Data/derived/direction_1-2hr.Rds')


p3.mods.all <- rbind(p3.mods[snail %in% p3.used, samp:='1hr'],
                     p3.mods.2hr[snail %in% p3.used.2hr, samp:='2hr'])


speed.edge.before <- ggplot(data=speed.all [snail != 'O11b'], aes(x=edist, y=ed.spd.before, color = brick2)) + 
  geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
 # geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  ylim(-5, 75) +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from edge (cm)") + ylab("Speed (m per hour)")
speed.edge.before 

speed.edge.after <- ggplot(data=speed.all, aes(x=edist, y=ed.spd.after, color = brick2)) + 
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



speed.brick.before <- ggplot(data=speed.all, aes(x=bdist, y=bd.spd.before, color = brick2)) + 
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

speed.brick.after <- ggplot(data=speed.all[snail!='O11b'], aes(x=bdist, y=bd.spd.after, color = brick2)) + 
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




speed.edge.before.dist <- ggplot(data=speed.all[snail!='O11b' & ed.spd.before >=-5], aes(x=bdist, y=ed.spd.before, color = disturbance)) + 
  #geom_line( size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance),size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from edge (cm)") + ylab("Speed (m per hour)")
speed.edge.before.dist

speed.edge.after.dist <- ggplot(data=speed.all, aes(x=bdist, y=ed.spd.after, color = disturbance)) + 
  #geom_line( size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance),size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from edge (cm)") + ylab("Speed (m per hour)")
speed.edge.after.dist


speed.brick.before.dist <- ggplot(data=speed.all, aes(x=bdist, y=bd.spd.before, color = disturbance)) + 
  #geom_line( size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance),size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from brick (cm)") + ylab("Speed (m per hour)")
speed.brick.before.dist

speed.brick.after.dist <- ggplot(data=speed.all[snail!='O11b'], aes(x=bdist, y=bd.spd.after, color = disturbance)) + 
  #geom_line( size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance),size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from brick (cm)") + ylab("Speed (m per hour)")
speed.brick.after.dist



direction.edge.before <- ggplot(data=direction.all, aes(x=edist, y=ed.dir.before, color = brick2)) + 
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
  xlab("Distance from edge (cm)") + ylab("Concentration of turn angle")
direction.edge.before 

direction.edge.after <- ggplot(data=direction.all, aes(x=edist, y=ed.dir.after, color = brick2)) + 
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
  xlab("Distance from edge (cm)") + ylab("Concentration of turn angle")
direction.edge.after 



direction.brick.before <- ggplot(data=direction.all, aes(x=bdist, y=bd.dir.before, color = brick2)) + 
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
  xlab("Distance from brick (cm)") + ylab("Concentration of turn angle")
direction.brick.before 

direction.brick.after <- ggplot(data=direction.all[snail != 'O24b'], aes(x=bdist, y=bd.dir.after, color = brick2)) + 
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
  xlab("Distance from brick (cm)") + ylab("Concentration of turn angle")
direction.brick.after 




direction.edge.before.dist <- ggplot(data=direction.all, aes(x=edist, y=ed.dir.before, color = disturbance)) + 
  #geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance), size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from edge (cm)") + ylab("Concentration of turn angle")
direction.edge.before.dist 

direction.edge.after.dist <- ggplot(data=direction.all, aes(x=edist, y=ed.dir.after, color = disturbance)) + 
  #geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance), size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from edge (cm)") + ylab("Concentration of turn angle")
direction.edge.after.dist 



direction.brick.before.dist <- ggplot(data=direction.all, aes(x=bdist, y=bd.dir.before, color = disturbance)) + 
  #geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance), size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from brick (cm)") + ylab("Concentration of turn angle")
direction.brick.before.dist 

direction.brick.after.dist <- ggplot(data=direction.all[snail != 'O24b'], aes(x=bdist, y=bd.dir.after, color = disturbance)) + 
  #geom_line(aes(group=snails2, linetype = disturbance), size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(aes(fill=disturbance), size = 2, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b) after ") +
  xlab("Distance from brick (cm)") + ylab("Concentration of turn angle")
direction.brick.after.dist 


#### Disturbance graphs ----
speed.edge.before|speed.edge.after
speed.brick.before|speed.brick.after
direction.edge.before|direction.edge.after
direction.brick.before|direction.brick.after

speed.edge.before.dist|speed.edge.after.dist
speed.brick.before.dist|speed.brick.after.dist
direction.edge.before.dist|direction.edge.after.dist
direction.brick.before.dist|direction.brick.after.dist

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
  #geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) +
  facet_wrap(vars(snail))


snails
gam <- dat[case_==TRUE & sl_ > 0,.(vals = MASS::fitdistr(sl_, "gamma")[[1]],
                         param = names(MASS::fitdistr(sl_, "gamma")[[1]])), by = .(snail)]
gam.wide <- dcast(gam, snail ~ param, value.var = "vals")

gam.snails <- merge(dat[case_==TRUE,.(snail, sl_)], gam.wide, by = c('snail'), all.x = T)

expo <- dat[case_==TRUE & sl_ > 0,.(vals = MASS::fitdistr(sl_, "exponential")[[1]],
                                   param = names(MASS::fitdistr(sl_, "exponential")[[1]])), by = .(snail)]
expo.wide <- dcast(expo, snail ~ param, value.var = "vals")

expo.snails <- merge(dat[case_==TRUE,.(snail, sl_)], expo.wide, by = c('snail'), all.x = T)

ggplot(gam.snails) +
  geom_histogram(aes(sl_, y = ..density..), bins=50) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  facet_wrap(vars(snail))

ggplot(expo.snails) +
  geom_histogram(aes(sl_, y = ..density..), bins=70) +
  geom_line(aes(x=sl_, y=dexp(sl_, rate[1])), color="blue", size = 1) +
  facet_wrap(vars(snail))

ggplot(gam.snails[snail==snails[1]]) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle(gam.snails$snail)



#### ALL MODELS ####

#all.mods <- rbind(core.mods, p1.mods, p3.mods)

all.mods <- readRDS('Data/derived/allMods_hr.Rds')

all.mods <- rbind(core.mods[,samp:= '1hr'], p1.mods[,samp:= '1hr'], p3.mods[,samp:= '1hr'],
                  core.mods.2hr[,samp:= '2hr'], p1.mods.2hr[,samp:= '2hr'], p3.mods.2hr[,samp:= '2hr'])
#saveRDS(all.mods, 'Data/derived/allMods_1-2hr.Rds')

good.mods <- rbind(core.mods.all, p1.mods.all, p3.mods.all)
#saveRDS(good.mods, 'Data/derived/goodMods_1-2hr.Rds')

good.mods[, nMods := .N, by = snail]
# tab.good <- good.mods[nMods > 1, calc_aictab(mod, model), by = .(snail, samp)]
# evi.good <- good.mods[nMods > 1 & !(model %like% 'g1') & !(model %like% 'g3'), evidence(calc_aictab(mod, model)), by = .(snail)]



all.mods[, sampMods := paste(samp,model, sep = '.')]
tab <- all.mods[nMods > 1, calc_aictab(mod, model), by = .(snail, samp)]
evi <- all.mods[nMods > 1 & !(model %like% 'g1') & !(model %like% 'g3'), evidence(calc_aictab(mod, model)), by = .(snail, samp)]


