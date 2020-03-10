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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

####   CORE MODEL   ----

## Create core coefficients
core.coef <- Core.m[!(variable %like% "AIC")]

## Rename variables
core.coef$variable <- ifelse(core.coef$variable=="SL", "log Step Length", core.coef$variable)
core.coef$variable <- ifelse(core.coef$variable=="2", "cos Turn Angle", core.coef$variable)
core.coef$variable <- ifelse(core.coef$variable=="3", "log Step Length:Time of Day", core.coef$variable)
core.coef$variable <- ifelse(core.coef$variable=="4", "log Step Length:Temperature", core.coef$variable)
core.coef$variable <- ifelse(core.coef$variable=="5", "log Step Length:Precipitation", core.coef$variable)
core.coef[,"variable"] <- factor(core.coef$variable, levels = c("log Step Length", "cos Turn Angle", "log Step Length:Time of Day",
                                                                  "log Step Length:Temperature", "log Step Length:Precipitation"))
core.coef <- core.coef[,.(snail, term, value, Disturbance, variable, nbricks)]

### PLOT FOR ALL TREATMENTS ###

cbPalette = c("red", "blue")
core.fig <- ggplot(core.coef, aes(variable, (value))) +
  geom_boxplot(aes(fill = Disturbance),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Disturbance),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("Core Model") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

core.fig

####   P1 FIGURES   ----

## Create P1 coefficients
p1.coef <- P1.m[variable %like% "Stage" & !(variable %like% "StageAcc") ]

p1.coef[,"Stage"] <- ifelse(p1.coef$variable %like% "StageA", "After", "Before")
p1.coef[,"Stage"] <- factor(p1.coef$Stage, levels = c("Before", "After"))
p1.coef$Treatment <- ifelse(p1.coef$Treatment=="4", "C", p1.coef$Treatment)

## Extract variables from string
p1.coef[,"variable2"] <- str_remove_all((p1.coef$variable), "StageA")
p1.coef[,"variable3"] <- str_remove_all((p1.coef$variable2), "StageB")
p1.coef[,"variable4"] <- str_remove_all((p1.coef$variable3), ":")

## Rename variables
p1.coef$variable4 <- ifelse(p1.coef$variable4=="edgedist_end", "Edge Distance (end)", p1.coef$variable4)
p1.coef$variable4 <- ifelse(p1.coef$variable4=="brickdist_end", "Brick Distance (end)", p1.coef$variable4)
p1.coef$variable4 <- ifelse(p1.coef$variable4=="SL", "log Step Length", p1.coef$variable4)
p1.coef$variable4 <- ifelse(p1.coef$variable4=="TA", "cos Turn Angle", p1.coef$variable4)
p1.coef[,"variable4"] <- factor(p1.coef$variable4, levels = c("Edge Distance (end)","Brick Distance (end)",
                                                              "log Step Length", "cos Turn Angle" ))
p1.coef <- p1.coef[,.(snail, term, value, Disturbance, Stage, variable=variable4, nbricks)]

### PLOT FOR ALL TREATMENTS ###

## CONTROL ##
#Used control 2 but look into this!!

cbPalette = c("red", "blue")
p1.control2 <- ggplot(p1.coef[Disturbance=="Control" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p1.control2

## DISTURBED ##

#Disturbances

cbPalette = c("red", "blue")
p1.disturbed <- ggplot(p1.coef[Disturbance=="Disturbed"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p1.disturbed

## COMBINED PLOTS ## 

p1.figure <- p1.control2/p1.disturbed + plot_annotation('P1')
p1.figure

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

####   P2 FIGURES   ----

## Create P2 coefficients
p2.coef <- P2.m[variable %like% "Stage" & !(variable %like% "StageAcc") ]

p2.coef[,"Stage"] <- ifelse(p2.coef$variable %like% "StageA", "After", "Before")
p2.coef[,"Stage"] <- factor(p2.coef$Stage, levels = c("Before", "After"))
p2.coef$Treatment <- ifelse(p2.coef$Treatment=="4", "C", p2.coef$Treatment)

## Extract variables from string
p2.coef[,"variable2"] <- str_remove_all((p2.coef$variable), "StageA")
p2.coef[,"variable3"] <- str_remove_all((p2.coef$variable2), "StageB")
p2.coef[,"variable4"] <- str_remove_all((p2.coef$variable3), ":")

## Rename variables
p2.coef$variable4 <- ifelse(p2.coef$variable4=="edgedist_end", "Edge Distance (end)", p2.coef$variable4)
p2.coef$variable4 <- ifelse(p2.coef$variable4=="brickdist_end", "Brick Distance (end)", p2.coef$variable4)
p2.coef$variable4 <- ifelse(p2.coef$variable4=="SL", "log Step Length", p2.coef$variable4)
p2.coef$variable4 <- ifelse(p2.coef$variable4=="TA", "cos Turn Angle", p2.coef$variable4)
p2.coef[,"variable4"] <- factor(p2.coef$variable4, levels = c("Edge Distance (end)","Brick Distance (end)",
                                                              "log Step Length", "cos Turn Angle" ))
p2.coef <- p2.coef[,.(snail, term, value, Disturbance, Stage, variable=variable4, nbricks)]

### PLOT TREATMENT 1 ###

## CONTROL 1 ##

cbPalette = c("red", "blue")
p2.control1 <- ggplot(p2.coef[Disturbance=="Control" & nbricks=="1"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p2.control1

## DISTURBED 1##

#Disturbances

cbPalette = c("red", "blue")
p2.brick1 <- ggplot(p2.coef[Disturbance=="Disturbed" & nbricks=="1"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p2.brick1

## COMBINED PLOTS ## 

p2.treat1 <- p2.control1/p2.brick1 + plot_annotation('P2 - Treatment 1')
p2.treat1

### PLOT TREATMENT 2 ###

## CONTROL 2##

cbPalette = c("red", "blue")
p2.control2 <- ggplot(p2.coef[Disturbance=="Control" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p2.control2

## DISTURBED ##

#Disturbances

cbPalette = c("red", "blue")
p2.brick2 <- ggplot(p2.coef[Disturbance=="Disturbed" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p2.brick2

## COMBINED PLOTS ## 

p2.treat2 <- p2.control2/p2.brick2 + plot_annotation('P2 - Treatment 2')
p2.treat2

### PLOT TREATMENT 3 ###

## CONTROL 3##

cbPalette = c("red", "blue")
p2.control3 <- ggplot(p2.coef[Disturbance=="Control" & nbricks=="3"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p2.control3

## DISTURBED ##

#Disturbances

cbPalette = c("red", "blue")
p2.brick3 <- ggplot(p2.coef[Disturbance=="Disturbed" & nbricks=="3"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p2.brick3

## COMBINED PLOTS ## 

p2.treat3 <- p2.control3/p2.brick3 + plot_annotation('P2 - Treatment 3 (4 bricks)')
p2.treat3


####   P3 FIGURES   ----

## Create P3 coefficients

p3.coef <- P3.m[variable %like% "Stage" & !(variable %like% "StageAcc") ]

p3.coef[,"Stage"] <- ifelse(p3.coef$variable %like% "StageA", "After", "Before")
p3.coef[,"Stage"] <- factor(p3.coef$Stage, levels = c("Before", "After"))
p3.coef$Treatment <- ifelse(p3.coef$Treatment=="4", "C", p3.coef$Treatment)

## Extract variables from string
p3.coef[,"variable2"] <- str_remove_all((p3.coef$variable), "StageA")
p3.coef[,"variable3"] <- str_remove_all((p3.coef$variable2), "StageB")
p3.coef[,"variable4"] <- str_remove_all((p3.coef$variable3), ":")

## Rename variables

p3.coef$variable4 <- ifelse(p3.coef$variable4=="SL", "log Step Length", p3.coef$variable4)
p3.coef$variable4 <- ifelse(p3.coef$variable4=="TA", "cos Turn Angle", p3.coef$variable4)
p3.coef$variable4 <- ifelse(p3.coef$variable4=="SLedgedist_start", "log Step Length:Edge Distance (start)", p3.coef$variable4)
p3.coef$variable4 <- ifelse(p3.coef$variable4=="TAedgedist_start", "cos Turn Angle:Edge Distance (start)", p3.coef$variable4)
p3.coef$variable4 <- ifelse(p3.coef$variable4=="SLbrickdist_start", "log Step Length:Brick Distance (start)", p3.coef$variable4)
p3.coef$variable4 <- ifelse(p3.coef$variable4=="TAbrickdist_start", "cos Turn Angle:Brick Distance (start)", p3.coef$variable4)

uniquevar <- as.data.table(unique(p3.coef$variable4))

p3.coef[,"variable4"] <- factor(p3.coef$variable4, levels = c("log Step Length", "cos Turn Angle", "log Step Length:Edge Distance (start)",
                                                              "cos Turn Angle:Edge Distance (start)","log Step Length:Brick Distance (start)",
                                                              "cos Turn Angle:Brick Distance (start)"))

p3.coef <- p3.coef[,.(snail, term, value, Disturbance, Stage, variable=variable4, nbricks)]

### PLOT FOR ALL TREATMENTS ###

## CONTROL ##
#Used control 2 but look into this!!

cbPalette = c("red", "blue")
p3.control2 <- ggplot(p3.coef[Disturbance=="Control" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p3.control2

## DISTURBED ##

#Disturbances

cbPalette = c("red", "blue")
p3.disturbed <- ggplot(p3.coef[Disturbance=="Disturbed"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p3.disturbed

## COMBINED PLOTS ## 

p3.figure <- p3.control2/p3.disturbed + plot_annotation('P3')
p3.figure

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

####   P4 FIGURES   ----

## Create P1 coefficients
p4.coef <- P4.m[variable %like% "Stage" & !(variable %like% "StageAcc") ]

p4.coef[,"Stage"] <- ifelse(p4.coef$variable %like% "StageA", "After", "Before")
p4.coef[,"Stage"] <- factor(p4.coef$Stage, levels = c("Before", "After"))
p4.coef$Treatment <- ifelse(p4.coef$Treatment=="4", "C", p4.coef$Treatment)

## Extract variables from string
p4.coef[,"variable2"] <- str_remove_all((p4.coef$variable), "StageA")
p4.coef[,"variable3"] <- str_remove_all((p4.coef$variable2), "StageB")
p4.coef[,"variable4"] <- str_remove_all((p4.coef$variable3), ":")

## Rename variables
p4.coef$variable4 <- ifelse(p4.coef$variable4=="SLedgedist_start", "log Step Length:Edge Distance (start)", p4.coef$variable4)
p4.coef$variable4 <- ifelse(p4.coef$variable4=="TAedgedist_start", "cos Turn Angle:Edge Distance (start)", p4.coef$variable4)
p4.coef$variable4 <- ifelse(p4.coef$variable4=="SLbrickdist_start", "log Step Length:Brick Distance (start)", p4.coef$variable4)
p4.coef$variable4 <- ifelse(p4.coef$variable4=="TAbrickdist_start", "cos Turn Angle:Brick Distance (start)", p4.coef$variable4)
p4.coef$variable4 <- ifelse(p4.coef$variable4=="SL", "log Step Length", p4.coef$variable4)
p4.coef$variable4 <- ifelse(p4.coef$variable4=="TA", "cos Turn Angle", p4.coef$variable4)
p4.coef[,"variable4"] <- factor(p4.coef$variable4, levels = c("log Step Length", "cos Turn Angle", "log Step Length:Edge Distance (start)",
                                                              "cos Turn Angle:Edge Distance (start)", "log Step Length:Brick Distance (start)",
                                                              "cos Turn Angle:Brick Distance (start)"))
p4.coef <- p4.coef[,.(snail, term, value, Disturbance, Stage, variable=variable4, nbricks)]

### PLOT TREATMENT 1 ###

## CONTROL 1 ##

cbPalette = c("red", "blue")
p4.control1 <- ggplot(p4.coef[Disturbance=="Control" & nbricks=="1"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p4.control1

## DISTURBED 1##

#Disturbances

cbPalette = c("red", "blue")
p4.brick1 <- ggplot(p4.coef[Disturbance=="Disturbed" & nbricks=="1"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p4.brick1

## COMBINED PLOTS ## 

p4.treat1 <- p4.control1/p4.brick1 + plot_annotation('P4 - Treatment 1 (1 brick)')
p4.treat1

### PLOT TREATMENT 2 ###

## CONTROL 2##

cbPalette = c("red", "blue")
p4.control2 <- ggplot(p4.coef[Disturbance=="Control" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p4.control2

## DISTURBED ##

#Disturbances

cbPalette = c("red", "blue")
p4.brick2 <- ggplot(p4.coef[Disturbance=="Disturbed" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p4.brick2

## COMBINED PLOTS ## 

p4.treat2 <- p4.control2/p4.brick2 + plot_annotation('P4 - Treatment 2 (2 bricks)')
p4.treat2

### PLOT TREATMENT 3 ###

## CONTROL 3##

cbPalette = c("red", "blue")
p4.control3 <- ggplot(p4.coef[Disturbance=="Control" & nbricks=="3"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("a) Control") +
  #scale_fill_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p4.control3

## DISTURBED ##

#Disturbances

cbPalette = c("red", "blue")
p4.brick3 <- ggplot(p4.coef[Disturbance=="Disturbed" & nbricks=="3"], aes(variable, (value))) +
  geom_boxplot(aes(fill = Stage),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = Stage),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  ylab('beta') +
  ggtitle("b) Disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,20)

p4.brick3

## COMBINED PLOTS ## 

p4.treat3 <- p4.control3/p4.brick3 + plot_annotation('P4 - Treatment 3 (4 bricks)')
p4.treat3
