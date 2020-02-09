### MODELS ###

### Packages ----
# remotes::install_github('bbolker/broom.mixed')
# remotes::install_github('ropensci/spatsoc')
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2','survival','forcats')
lapply(libs, require, character.only = TRUE)




### Input data ----
raw <- '~/Honours stuff/Snails/Data/raw/'
derived <- '~/Honours stuff/Snails/Data/derived/'
dat <- readRDS('~/Honours stuff/Snails/Data/derived/ssaAll_snails2019.Rds')

### CORE ###

Core <- function(y, SL, TA, ToD, Temp, Precip, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL +
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

coreOUT<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, step_id_), by = .(snail)]

### P1 ###

Core <- function(y, SL, TA, ToD, Temp, Precip, edgedist_end, 
                 brickedge1_end, brickedge2_end, brickedge3_end, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL + edgedist_end +
                    brickedge1_end + brickedge2_end + brickedge3_end + strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

coreOUT.p1<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, log(edgedist_end + 1),
                       log(brickedge1_end + 1), log(brickedge2_end + 1), log(brickedge3_end + 1), 
                       step_id_), by = .(snail)]

### P2 ###

Core <- function(y, SL, TA, ToD, Temp, Precip, edgedist_end, brickedge1_end, 
                 brickedge2_end, brickedge3_end, Treatment, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL + edgedist_end:Treatment +
                  brickedge1_end:Treatment + brickedge2_end:Treatment + brickedge3_end:Treatment +
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

coreOUT.p2<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, log(edgedist_end + 1), 
                       log(brickedge1_end + 1), log(brickedge2_end + 1), log(brickedge3_end + 1), 
                       Treatment, step_id_), by = .(snail)]

### P3 ###

Core <- function(y, SL, TA, ToD, Temp, Precip, edgedist_start, brickedge1_start, 
                 brickedge2_start, brickedge3_start, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL + edgedist_start:SL + edgedist_start:TA +
                    brickedge1_start:SL + brickedge1_start:TA + brickedge2_start:SL + brickedge2_start:TA +
                    brickedge3_start:SL + brickedge3_start:TA +strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

coreOUT.p3<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, log(edgedist_start + 1), 
                       log(brickedge1_start + 1), log(brickedge2_start + 1), log(brickedge3_start + 1), 
                       step_id_), by = .(snail)]

### P4 ###

Core <- function(y, SL, TA, ToD, Temp, Precip, edgedist_end, brickedge1_end, 
                 brickedge2_end, brickedge3_end, Treatment, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL + edgedist_start:SL:Treatment +
                    edgedist_start:TA:Treatment + brickedge1_start:SL:Treatment + brickedge1_start:TA:Treatment +
                    brickedge2_start:SL:Treatment + brickedge2_start:TA:Treatment + brickedge3_start:SL:Treatment +
                    brickedge3_start:TA:Treatment + strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

coreOUT.p4<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, log(edgedist + 1), 
                       log(brickedge1_start + 1), log(brickedge2_start + 1), log(brickedge3_start + 1), 
                       Treatment, step_id_), by = .(snail)]
