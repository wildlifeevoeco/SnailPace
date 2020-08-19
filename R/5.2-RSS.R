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


### P1 and P2 ----
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


