#### Figures ====

### Packages ----
libs <- c('ggplot2','ggpubr','data.table', 'dplyr', 'tidyr', 'tidyverse', 'patchwork')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- '~/snails/Data/raw/'
derived <- '~/snails/Data/derived/'
CoreModel <- readRDS('~/snails/Data/derived/CoreModel.Rds')
P1Model <- readRDS('~/snails/Data/derived/P1Model.Rds')
P2Model <- readRDS('~/snails/Data/derived/P2Model.Rds')
P3Model <- readRDS('~/snails/Data/derived/P3Model.Rds')
P4Model <- readRDS('~/snails/Data/derived/P4Model.Rds')

### Combine models ----
#nbricks has to be character
P2Model[,"nbricks"] <- as.character(P2Model$nbricks)
P4Model[,"nbricks"] <- as.character(P4Model$nbricks)

Core.m <- melt(CoreModel)
P1.m <- melt(P1Model)
P2.m <- melt(P2Model)
P3.m <- melt(P3Model)
P4.m <- melt(P4Model)

Core.m[,"Treatment"] <- substr(Core.m$snail, 3, 3)
P1.m[,"Treatment"] <- substr(P1.m$snail, 3, 3)
P2.m[,"Treatment"] <- substr(P2.m$snail, 3, 3)
P3.m[,"Treatment"] <- substr(P3.m$snail, 3, 3)
P4.m[,"Treatment"] <- substr(P4.m$snail, 3, 3)

all.m <- rbind(Core.m, P1.m, P2.m, P3.m, P4.m)

all.coef <- all.m[term=='coef' & variable!="AIC"]

ba.coef <- all.coef[variable %like% "Stage" & !(variable %like% "StageAcc") ]

ba.coef$Treatment <- ifelse(ba.coef$Treatment=="4", "C", ba.coef$Treatment)

## Extract stage variable 
ba.coef[,"ba"] <- ifelse(ba.coef$variable %like% "StageA", "A", "B")

## New variable of treatment/control and stage combined
ba.coef[,"stage.ct"] <- paste(ba.coef$Treatment, ba.coef$ba, sep = "")


## Extract variables!!

ba.coef[,"variable2"] <- str_remove_all((ba.coef$variable), "StageA")
ba.coef[,"variable3"] <- str_remove_all((ba.coef$variable2), "StageB")
ba.coef[,"variable4"] <- str_remove_all((ba.coef$variable3), ":")

ba.coef <- ba.coef[,.(snail, term, value, Treatment, ba, stage.ct, variable=variable4, nbricks, Disturbance)]

#saveRDS(ba.coef, '~/snails/Data/derived/ba-coef.Rds')

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

####   P1 FIGURES   ----

## Create P1 coefficients
p1.coef <- P1.m[variable %like% "Stage" & !(variable %like% "StageAcc") ]

p1.coef[,"ba"] <- ifelse(p1.coef$variable %like% "StageA", "A", "B")
p1.coef[,"ba"] <- factor(p1.coef$ba, levels = c("B", "A"))
p1.coef$Treatment <- ifelse(p1.coef$Treatment=="4", "C", p1.coef$Treatment)

## New variable of treatment/control and stage combined
p1.coef[,"stage.ct"] <- paste(p1.coef$Disturbance, p1.coef$ba, sep = "")

## Extract variables from string
p1.coef[,"variable2"] <- str_remove_all((p1.coef$variable), "StageA")
p1.coef[,"variable3"] <- str_remove_all((p1.coef$variable2), "StageB")
p1.coef[,"variable4"] <- str_remove_all((p1.coef$variable3), ":")

## Rename variables
p1.coef$variable4 <- ifelse(p1.coef$variable4=="edgedist_end", "Edge Distance (end)", p1.coef$variable4)
p1.coef$variable4 <- ifelse(p1.coef$variable4=="brickdist_end", "Brick Distance (end)", p1.coef$variable4)
p1.coef$variable4 <- ifelse(p1.coef$variable4=="SL", "log Step Length", p1.coef$variable4)
p1.coef$variable4 <- ifelse(p1.coef$variable4=="TA", "cos Turn Angle", p1.coef$variable4)

p1.coef <- p1.coef[,.(snail, term, value, Disturbance, ba, variable=variable4, nbricks)]

