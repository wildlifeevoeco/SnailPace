### MODELS ###

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2','survival','forcats')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/snails/Data/raw/'
derived <- '~/snails/Data/derived/'
dat <- readRDS('~/snails/Data/derived/ssaAll_snails2019.Rds')

### CORE ###

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

coreOUT<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precipitation, step_id_), by = .(snail)]

saveRDS(coreOUT, '~/snails/Data/derived/CoreModel.Rds')

### P1 ###

Core <- function(y, SL, TA, ToD, Temp, Precipitation, edgedist_end, 
                 brickedge1_end, brickedge2_end, brickedge3_end, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precipitation:SL + edgedist_end +
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

coreOUT.p1<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precipitation, log(edgedist_end + 1),
                       log(brickedge1_end + 1), log(brickedge2_end + 1), log(brickedge3_end + 1), 
                       step_id_), by = .(snail)]
saveRDS(coreOUT.p1, '~/snails/Data/derived/P1Model.Rds')

### P2 ###

Core <- function(y, SL, TA, ToD, Temp, Precipitation, edgedist_end, brickedge1_end, 
                 brickedge2_end, brickedge3_end, Treatment, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precipitation:SL + edgedist_end:Treatment +
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

###ISSUES HERE### 
coreOUT.p2<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precipitation, log(edgedist_end + 1), 
                       log(brickedge1_end + 1), log(brickedge2_end + 1), log(brickedge3_end + 1), 
                       Treatment, step_id_), by = .(snail)]

saveRDS(coreOUT.p2, '~/snails/Data/derived/P2Model.Rds')

### P3 ###

Core <- function(y, SL, TA, ToD, Temp, Precipitation, edgedist_start, brickedge1_start, 
                 brickedge2_start, brickedge3_start, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precipitation:SL + edgedist_start:SL + edgedist_start:TA +
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

coreOUT.p3<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precipitation, log(edgedist_start + 1), 
                       log(brickedge1_start + 1), log(brickedge2_start + 1), log(brickedge3_start + 1), 
                       step_id_), by = .(snail)]
saveRDS(coreOUT.p3, '~/snails/Data/derived/P3Model.Rds')

### P4 ###

Core <- function(y, SL, TA, ToD, Temp, Precipitation, edgedist_start, brickedge1_start, 
                 brickedge2_start, brickedge3_start, Treatment, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + Precipitation:SL + edgedist_start:SL:Treatment +
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

coreOUT.p4<- dat[,Core(case_, log_sl, cos_ta, ToD_start, Temperature, Precipitation, log(edgedist_start + 1), 
                       log(brickedge1_start + 1), log(brickedge2_start + 1), log(brickedge3_start + 1), 
                       Treatment, step_id_), by = .(snail)]

saveRDS(coreOUT.p4, '~/snails/Data/derived/P4Model.Rds')
