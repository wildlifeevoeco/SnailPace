### MODELS ###

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2','survival','forcats')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/snails/Data/raw/'
derived <- '~/snails/Data/derived/'
dat <- readRDS('~/snails/Data/derived/ssa30ghosts.Rds')
dat <- dat[Stage!="Acc"]
#### CORE ====

Core <- function(y, SL, TA, ToD, Temp, Precipitation, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precipitation:SL +
                    strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

listbricks <- c("C", "1", "2", "4")
coreOUT<- dat[snail != 'P11a' & ghostbricks %in% listbricks,
              {
                print(.BY[[1]])
                Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precipitation, step_id_)
              },
              by = snail]

saveRDS(coreOUT, '~/snails/Data/derived/CoreModel.Rds')

#### P1 ====

P1Model<- function(y, SL, TA, ToD, Temp, edgedist_end, 
                 brickdist_end, Stage, strata1) {
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
P1.g3.Out<- dat[!(snail %in% badsnails)&ghostbricks=='g3',
                 {
                   print(.BY[[1]])
                   P1Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1),
                           log(brickdist_end + 1), Stage, step_id_)
                 },
                 by = .(snail)]

P1.g2.Out<- dat[!(snail %in% badsnails)&ghostbricks=='g2',
             {
               print(.BY[[1]])
               P1Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1),
                       log(brickdist_end + 1), Stage, step_id_)
             },
             by = .(snail)]

P1.g1.Out<- dat[!(snail %in% badsnails)&ghostbricks=='g1',
                {
                  print(.BY[[1]])
                  P1Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1),
                          log(brickdist_end + 1), Stage, step_id_)
                },
                by = .(snail)]
badsnails <- c("P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "P23a", "P23b")
P1.treats.Out<- dat[!(snail %in% badsnails)&Treatment!='C',
                {
                  print(.BY[[1]])
                  P1Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1),
                          log(brickdist_end + 1), Stage, step_id_)
                },
                by = .(snail)]
P1ModelOut <- rbind(P1.g1.Out, P1.g2.Out, P1.g3.Out, P1.treats.Out)

#saveRDS(P1ModelOut, '~/snails/Data/derived/P1Model.Rds')

#### P2 ====

P2Model <- function(y, SL, TA, ToD, Temp, edgedist_end, brickdist_end, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_end:Stage +
                  brickdist_end:Stage + strata(strata1))
  
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

badsnails <- c("P11a", "P21a")
P2.treat1.Out<- dat[!(snail %in% badsnails) & Treatment=="1",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O12b", "P12b", "P22b")
P2.treat2.Out<- dat[!(snail %in% badsnails) & Treatment=="2",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("P23b")
P2.treat3.Out<- dat[!(snail %in% badsnails) & Treatment=="3",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

P2ModelOut <- rbind(P2.g1.Out, P2.g2.Out, P2.g3.Out, P2.treat1.Out, P2.treat2.Out, P2.treat3.Out)

saveRDS(P2ModelOut, '~/snails/Data/derived/P2Model.Rds')

#### P3 ====

P3Model <- function(y, SL, TA, ToD, Temp, edgedist_start, brickdist_start,
                    Treatment, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_start:SL + edgedist_start:TA +
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

P3.g3.Out<- dat[ghostbricks=="g3",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

P3.g2.Out<- dat[ghostbricks=="g2",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

P3.g1.Out<- dat[ghostbricks=="g1",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "O13a", "P23a", "P23b")
P3.treats.Out<- dat[!(snail %in% badsnails) & Treatment!="C",
                  {
                    print(.BY[[1]])
                    P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                            log(brickdist_end + 1), Treatment, Stage, step_id_)
                  },
                  by = .(snail)]

P3ModelOut <- rbind(P3.g1.Out, P3.g2.Out, P3.g3.Out, P3.treats.Out)
#saveRDS(P3ModelOut, '~/snails/Data/derived/P3Model.Rds')

#### P4 ====

P4Model<- function(y, SL, TA, ToD, Temp, edgedist_start, brickdist_start, 
                  Treatment, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_start:SL:Treatment +
                    edgedist_start:TA:Treatment + brickdist_start:SL:Treatment + brickdist_start:TA:Treatment +
                    strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

badsnails <- c("O11a")
P4.g3.Out<- dat[!(snail %in% badsnails) & Treatment=="g3",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Treatment, step_id_)
                    },
                    by = .(snail)]

P4.g2.Out<- dat[!(snail %in% badsnails) & Treatment=="g2",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, step_id_)
                },
                by = .(snail)]

P4.g1.Out<- dat[!(snail %in% badsnails) & Treatment=="g1",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, step_id_)
                },
                by = .(snail)]

P4.treats.Out<- dat[!(snail %in% badsnails) & Treatment!="C",
                   {
                     print(.BY[[1]])
                     P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                             log(brickdist_start + 1), Treatment, step_id_)
                   },
                   by = .(snail)]

P4ModelOut <- rbind(P4.g1.Out, P4.g2.Out, P4.g3.Out, P4.treats.Out)

#saveRDS(P4ModelOut, '~/snails/Data/derived/P4Model.Rds')
