### RSS ###
# Julie Turner
# started 13 May 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2','survival','forcats')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'
dat <- readRDS('Data/derived/ssa30ghosts.Rds')
#dat <- dat[Stage!="Acc"] ## Can't limit to ToD=night because it won't work in interactions
dat$Stage <- factor(dat$Stage, levels = c("Acc", "B","A"))
dat$ToD_start <- as.factor(dat$ToD_start)
dat$Precipitation <- as.factor(dat$Precipitation)

# list of all snails
snails <- unique(dat$snail)

# function for model list
list_models <- function(resp, expl, DT) {
  list(clogit(reformulate(expl, resp),
                   model = T, data = DT))
}

list_predict <- function(mod, ND) {
  mapply(function(m, n) predict(m, newdata = n, type = 'lp'),
         m = mod, n = ND)
}

#### CORE ====
usnails <- unique(dat$snail)
nsnails <- length(usnails)
setup <- data.table(
  model = rep(c('core', 'p1'), each = nsnails),
  snail = usnails,
  response = rep(c('case_', 'case_'), each = nsnails),
  explanatory = rep(c('log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + Precipitation:log_sl + strata(step_id_)',
                      'log_sl + cos_ta + ToD_start:log_sl +
                          Temperature:log_sl + log(edgedist_end + 1):Stage +
                    log(brickdist_end + 1):Stage + log_sl:Stage + cos_ta:Stage +
                          strata(step_id_)'),
                    each = nsnails)
)


corebad <- 'P11a'
p1bad <- c("P24b", "P11a", "P21a", "O12b", "O22b", "P12b", 
           "P22b", "P23a", "P23b", "O11a", "O13a")
setup[model == 'core' & snail %in% corebad, bad := TRUE]
setup[model == 'p1bad' & snail %in% p1bad, bad := TRUE]

setup[model == 'core', lsbricks := list(c("C", "1", "2", "3"))]
setup[model == 'p1bad', lsbricks := list('C')]


setup[!(bad),
      list_models(response, explanatory,
                  dat[ghostbricks %in% listbricks & snail == .BY[[1]]]), 
      snail]


# list of snails core runs for
coreSnails <- snails[!(snails %in% c('P11a'))]

# list of core models by snail
core_models <- 
  dat[ghostbricks %in% listbricks & snail %in% coreSnails,
      .(mod = list_models(
          'case_',
          'log_sl + cos_ta + ToD_start:log_sl +
                          Temperature:log_sl + Precipitation:log_sl +
                          strata(step_id_)',
          .SD),
        newdat = list(data.table(
          .SD[, .(log_sl = mean(log_sl),
                  cos_ta = mean(cos_ta),
                  ToD_start = factor('day', levels = levels(ToD_start)),
                  Temperature = mean(Temperature),
                  Precipitation = factor('no', levels = levels(Precipitation)),
                  step_id_ = step_id_[1])]))
        ), 
      by = snail]

core_models[, pred := list_predict(mod, newdat), by = snail]




core_models[,"nbricks"] <- substr(core_models$snail, 3, 3)
core_models[,"nbricks"] <- ifelse(core_models$nbricks=="4", "0", core_models$nbricks)
core_models[,"Disturbance"] <- ifelse(core_models$nbricks=="0", "Control", "Disturbed")

#saveRDS(coreOUT, '~/snails/Data/derived/CoreModel.Rds')

#### P1 ====

# list of snails core runs for
p1Snails <- snails[!(snails %in% c("P24b", "P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "P23a", "P23b", "O11a", "O13a"))]


# list of core models by snail
p1_models <- dat[ghostbricks != 'C' & snail %in% p1Snails,
                   .(mod=list_models('case_', 'log_sl + cos_ta + ToD_start:log_sl +
                          Temperature:log_sl + log(edgedist_end + 1):Stage +
                    log(brickdist_end + 1):Stage + log_sl:Stage + cos_ta:Stage +
                          strata(step_id_)', .SD)), by=.(ghostbricks, snail)]

p1_h2 <- dat[ghostbricks != 'C' & snail %in% p1Snails,
               .(log_sl = mean(log_sl), cos_ta = mean(cos_ta), ToD_start = factor('day', levels = levels(ToD_start)),
                 Temperature = mean(Temperature), Stage = factor('Acc', levels = levels(Stage)), 
                 edgedist_end = mean(edgedist_end), brickdist_end = mean(brickdist_end)),
               by = .(ghostbricks, snail)]

p1_models[,h2:=list_predict(mod = mod, ND = p1_h2)]



P1.g1.Out[,"nbricks"] <- 1
P1.g2.Out[,"nbricks"] <- 2
P1.g3.Out[,"nbricks"] <- 3
P1.treats.Out[,"nbricks"] <- substr(P1.treats.Out$snail, 3, 3)

P1.g1.Out[,"Disturbance"] <- "Control"
P1.g2.Out[,"Disturbance"] <- "Control"
P1.g3.Out[,"Disturbance"] <- "Control"
P1.treats.Out[,"Disturbance"] <- "Disturbed"

P1ModelOut <- rbind(P1.g1.Out, P1.g2.Out, P1.g3.Out, P1.treats.Out)

#saveRDS(P1ModelOut, '~/snails/Data/derived/P1Model.Rds')

#### P2 ====

P2Model <- function(y, SL, TA, ToD, Temp, edgedist_end, brickdist_end, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_end:Stage +
                    brickdist_end:Stage + SL:Stage + TA:Stage + strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}


badsnails <- c("P24b")
P2.g3.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g3",
                {
                  print(.BY[[1]])
                  P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                          log(brickdist_end + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b")
P2.g2.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g2",
                {
                  print(.BY[[1]])
                  P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                          log(brickdist_end + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b")
P2.g1.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g1",
                {
                  print(.BY[[1]])
                  P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                          log(brickdist_end + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P11a", "P21a", "O11a")
P2.treat1.Out<- dat[!(snail %in% badsnails) & Treatment=="1",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O12b", "O22b", "P12b", "P22b")
P2.treat2.Out<- dat[!(snail %in% badsnails) & Treatment=="2",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O13a","P23b", "P23a")
P2.treat3.Out<- dat[!(snail %in% badsnails) & Treatment=="3",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

P2.g1.Out[,"nbricks"] <- 1
P2.g2.Out[,"nbricks"] <- 2
P2.g3.Out[,"nbricks"] <- 3
P2.treat1.Out[,"nbricks"] <- 1
P2.treat2.Out[,"nbricks"] <- 2
P2.treat3.Out[,"nbricks"] <- 3

P2.g1.Out[,"Disturbance"] <- "Control"
P2.g2.Out[,"Disturbance"] <- "Control"
P2.g3.Out[,"Disturbance"] <- "Control"
P2.treat1.Out[,"Disturbance"] <- "Disturbed"
P2.treat2.Out[,"Disturbance"] <- "Disturbed"
P2.treat3.Out[,"Disturbance"] <- "Disturbed"

P2ModelOut <- rbind(P2.g1.Out, P2.g2.Out, P2.g3.Out, P2.treat1.Out, P2.treat2.Out, P2.treat3.Out)

#saveRDS(P2ModelOut, '~/snails/Data/derived/P2Model.Rds')

#### P3 ====

P3Model <- function(y, SL, TA, ToD, Temp, edgedist_start, brickdist_start,
                    Treatment, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_start:SL:Stage + edgedist_start:TA:Stage +
                    brickdist_start:SL:Stage + brickdist_start:TA:Stage + SL:Stage + TA:Stage + strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

badsnails <- c("P24b", "O24a")
P3.g3.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g3",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P3.g2.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g2",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P3.g1.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g1",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "O13a", "P23a", "P23b", "O22a")
P3.treats.Out<- dat[!(snail %in% badsnails) & Treatment!="C",
                    {
                      print(.BY[[1]])
                      P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Treatment, Stage, step_id_)
                    },
                    by = .(snail)]

P3.g1.Out[,"nbricks"] <- 1
P3.g2.Out[,"nbricks"] <- 2
P3.g3.Out[,"nbricks"] <- 3
P3.treats.Out[,"nbricks"] <- substr(P3.treats.Out$snail, 3, 3)

P3.g1.Out[,"Disturbance"] <- "Control"
P3.g2.Out[,"Disturbance"] <- "Control"
P3.g3.Out[,"Disturbance"] <- "Control"
P3.treats.Out[,"Disturbance"] <- "Disturbed"


P3ModelOut <- rbind(P3.g1.Out, P3.g2.Out, P3.g3.Out, P3.treats.Out)
#saveRDS(P3ModelOut, '~/snails/Data/derived/P3Model.Rds')

#### P4 ====

P4Model<- function(y, SL, TA, ToD, Temp, edgedist_start, brickdist_start, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_start:SL:Stage +
                    edgedist_start:TA:Stage + brickdist_start:SL:Stage + brickdist_start:TA:Stage + SL:Stage + TA:Stage + strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

badsnails <- c("P24b", "O24a")
P4.g3.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g3",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P4.g2.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g2",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P4.g1.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g1",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P11a", "P21a")
P4.treat1.Out<- dat[!(snail %in% badsnails) & Treatment=="1",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("P12b", "O12b", "P22b", "O22b", "O22a")
P4.treat2.Out<- dat[!(snail %in% badsnails) & Treatment=="2",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O13a", "P23b", "P13a")
P4.treat3.Out<- dat[!(snail %in% badsnails) & Treatment=="3",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Stage, step_id_)
                    },
                    by = .(snail)]

P4.g1.Out[,"nbricks"] <- 1
P4.g2.Out[,"nbricks"] <- 2
P4.g3.Out[,"nbricks"] <- 3
P4.treat1.Out[,"nbricks"] <- 1
P4.treat2.Out[,"nbricks"] <- 2
P4.treat3.Out[,"nbricks"] <- 3

P4.g1.Out[,"Disturbance"] <- "Control"
P4.g2.Out[,"Disturbance"] <- "Control"
P4.g3.Out[,"Disturbance"] <- "Control"
P4.treat1.Out[,"Disturbance"] <- "Disturbed"
P4.treat2.Out[,"Disturbance"] <- "Disturbed"
P4.treat3.Out[,"Disturbance"] <- "Disturbed"

P4ModelOut <- rbind(P4.g1.Out, P4.g2.Out, P4.g3.Out, P4.treat1.Out, P4.treat2.Out, P4.treat3.Out)

#saveRDS(P4ModelOut, '~/snails/Data/derived/P4Model.Rds')
