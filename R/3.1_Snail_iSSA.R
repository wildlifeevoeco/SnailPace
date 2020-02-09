### ANALYSES ###

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

Core <- function(y, SL, TA, ToD, Temp, Precip, edgedist_end, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL + edgedist_end +
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

coreOUT.p1<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, log(edgedist + 1), step_id_), by = .(snail)]

### P2 ###

Core <- function(y, SL, TA, ToD, Temp, Precip, edgedist_end, brickdist_end, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precip:SL + edgedist_end +
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

coreOUT.p1<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precip, log(edgedist + 1), step_id_), by = .(snail)]

