### Exploratory stuff ###

### Packages ----
libs <- c('data.table', 'dplyr')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/snails/Data/raw/'
derived <- '~/snails/Data/derived/'
dat <- readRDS('~/snails/Data/derived/ssa30ghosts.Rds')
#dat <- dat[Stage!="Acc"]

SE_function <- function(x){
  sd(x)/sqrt(length(x))
  }

dat[,.(meansl=mean(sl_), SEsl=SE_function(sl_), meanta=mean(ta_), SEta=SE_function(ta_)), by=.(Stage)]

dat.sum.day <-dat[,.(meansl=mean(sl_), SEsl=SE_function(sl_), meanta=mean(ta_), SEta=SE_function(ta_)), by=.(ToD_start)]

dat.sum <- dat[,.(meansl=mean(sl_), SEsl=SE_function(sl_), meanta=mean(ta_), SEta=SE_function(ta_)), by=.(Stage, Treatment)]

# Use dose as a factor rather than numeric
Stagefac <- dat.sum
Stagefac$Stage <- factor(Stagefac$Stage, levels = c("Acc", "B", "A"))

ToDfac <- dat.sum.day
ToDfac$ToD_start <- factor(ToDfac$ToD_start)


# Error bars represent standard error of the mean
ggplot(Stagefac, aes(x= Treatment, y=meansl, fill=Stage)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meansl-SEsl, ymax=meansl+SEsl),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# Error bars represent standard error of the mean
ggplot(Stagefac, aes(x= Treatment, y=meanta, fill=Stage)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanta-SEta, ymax=meanta+SEta),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# Error bars represent standard error of the mean
ggplot(ToDfac, aes(x= ToD_start , y=meansl)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meansl-SEsl, ymax=meansl+SEsl),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# Error bars represent standard error of the mean
ggplot(ToDfac, aes(x= ToD_start , y=meanta)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanta-SEta, ymax=meanta+SEta),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

dat[,'Hours'] <-(format(dat$t1_, "%H:%M:%S"))

dat[,"nhours"] <- gsub("[^[:digit:]]", "", dat$Hours)  
plot(dat$nhours, dat$sl_)

dat.sum.hrs <-dat[Treatment=="C",.(meansl=mean(sl_), SEsl=SE_function(sl_), meanta=mean(ta_), SEta=SE_function(ta_)), by=.(nhours)]

hoursfac <- dat.sum.hrs
hoursfac$nhours <- factor(hoursfac$nhours)

ggplot(hoursfac, aes(x= nhours , y=meansl)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meansl-SEsl, ymax=meansl+SEsl),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))





