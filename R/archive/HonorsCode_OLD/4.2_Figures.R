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

#### P1 ----
p1.coef <- P1.m[variable %like% "Stage" & !(variable %like% "StageAcc") ]

p1.coef[,"ba"] <- ifelse(p1.coef$variable %like% "StageA", "A", "B")
p1.coef[,"ba"] <- factor(p1.coef$ba, levels = c("B", "A"))
p1.coef$Treatment <- ifelse(p1.coef$Treatment=="4", "C", p1.coef$Treatment)
## New variable of treatment/control and stage combined

p1.coef[,"stage.ct"] <- paste(p1.coef$Disturbance, p1.coef$ba, sep = "")


p1.coef[,"variable2"] <- str_remove_all((p1.coef$variable), "StageA")
p1.coef[,"variable3"] <- str_remove_all((p1.coef$variable2), "StageB")
p1.coef[,"variable4"] <- str_remove_all((p1.coef$variable3), ":")

p1.coef <- p1.coef[,.(snail, term, value, Disturbance, ba, variable=variable4, nbricks)]

# # format of data needs to be a column of the covariates (here = term), a column of the values (here = delta), and a column of groupings (in my case COD)
# ### ALL TREATS ====
# cbPalette = c("red", "blue", "purple", "green")
# 
# beta.variables <- ggplot(ba.coef, aes(variable, (value) , fill = Treatment)) +
#   geom_boxplot(aes(fill = Treatment),# notch = TRUE, notchwidth = 0.7,
#                outlier.color = NA, lwd = 0.6,
#                alpha = 0.25) +
#   geom_jitter(aes(color = Treatment),
#               position = position_jitterdodge(.35),
#               size = 2, alpha = 0.4) +
#   #ggtitle('Interaction with community identity') +
#   geom_hline(aes(yintercept = 0), lty = 2) +
#   theme(#legend.position = 'none',
#     axis.title = element_text(size = 16, color = 'black'),
#     axis.text = element_text(size = 14, color = 'black'),
#     plot.title=element_text(size = 16, hjust=0),
#     axis.line = element_line(colour = "black"),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     strip.background = element_rect(colour="black", size = 1, fill = "white"),
#     strip.text = element_text(size = 14)) +
#   xlab('') +
#   ylab('beta') +
#   #ggtitle("c) change between control and case") +
#   scale_fill_manual(values = cbPalette) +
#   scale_color_manual(values = cbPalette)# + ylim(-2,2)
# 
# beta.variables

## Extract stage variable 
ba.coef[,"ba"] <- ifelse(ba.coef$variable %like% "StageA", "A", "B")

## New variable of treatment/control and stage combined
ba.coef[,"stage.ct"] <- paste(ba.coef$Treatment, ba.coef$ba, sep = "")


## Extract variables!!

ba.coef[,"variable2"] <- str_remove_all((ba.coef$variable), "StageA")
ba.coef[,"variable3"] <- str_remove_all((ba.coef$variable2), "StageB")
ba.coef[,"variable4"] <- str_remove_all((ba.coef$variable3), ":")

ba.coef <- ba.coef[,.(snail, term, value, Treatment, ba, stage.ct, variable=variable4)]

## Rename variables
# 
# ba.coef.names <- unique(ba.coef$variable)
# 
# ba.coef.betas.names <- c("edgedist_start", "edgedist_end", "brickdist_start", "brickdist_end", "SL", "TA", 
#                          "SLbrickdist_start","SLbrickdist_end", "TAbrickdist_start", "TAbrickdist_end",
#                          "SLedgedist_start", "SLedgedist_end", "TAedgedist_start", "TAedgedist_end",)
# 
# all.indiv.betas <- everyone.all.indiv[term %chin% everyone.all.betas.names]
# # everyone.betas <- c("log_sl", "cos_ta", "land_end_adjforest", "land_end_adjopen", "land_end_adjwet", "log(1 + roadDist_end)",
# #                 "log(1 + distance2)", "log(1 + packDistadj_end)")
# 
# everyone.all.indiv.betas$term <- factor(everyone.all.indiv.betas$term, levels = everyone.all.betas.names, labels = c("log_sl", "cos_ta",'forest', "open", "wet", "roadDist",
#                                                                                                                      "nnDist", "boundaryDist",
#                                                                                                                      "log_sl-ttd", "cos_ta-ttd", "forest-ttd", "open-ttd", "wet-ttd", "roadDist-ttd",
#                                                                                                                      "nnDist-ttd", "boundaryDist-ttd"))

saveRDS(ba.coef, '~/snails/Data/derived/ba-coef.Rds')

### Read in ba.coef ----
ba.coef <- readRDS('~/snails/Data/derived/ba-coef.Rds')

### P1 FIGURES ----


cbPalette = c("red", "blue")
p1.control1 <- ggplot(p1.coef[Disturbance=="Control" & nbricks=="1"], aes(variable, (value))) +
  geom_boxplot(aes(fill = ba),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = ba),
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
  ggtitle("a) control") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p1.control1

#Disturbances

cbPalette = c("red", "blue")
p1.brick1 <- ggplot(p1.coef[Disturbance=="Disturbed" & nbricks=="1"], aes(variable, (value))) +
  geom_boxplot(aes(fill = ba),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = ba),
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
  ggtitle("b) disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-10,35)

p1.brick1

p1.control1/p1.brick1

# Treatment 2

cbPalette = c("red", "blue")
p1.control2 <- ggplot(p1.coef[Disturbance=="Control" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = ba),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = ba),
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
  ggtitle("a) control") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

p1.control2

#Disturbances

cbPalette = c("red", "blue")
p1.brick2 <- ggplot(p1.coef[Disturbance=="Disturbed" & nbricks=="2"], aes(variable, (value))) +
  geom_boxplot(aes(fill = ba),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = ba),
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
  ggtitle("b) disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

p1.brick2

p1.control2/p1.brick2

# Treatment 3

cbPalette = c("red", "blue")
p1.control3 <- ggplot(p1.coef[Disturbance=="Control" & nbricks=="4"], aes(variable, (value))) +
  geom_boxplot(aes(fill = ba),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = ba),
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
  ggtitle("a) control") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

p1.control3

#Disturbances

cbPalette = c("red", "blue")
p1.brick3 <- ggplot(p1.coef[Disturbance=="Disturbed" & nbricks=="4"], aes(variable, (value))) +
  geom_boxplot(aes(fill = ba),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = ba),
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
  ggtitle("b) disturbed") +
  # scale_fill_manual(values = cbPalette) +
  # scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

p1.brick2

p1.control2/p1.brick2


##Combined treat 1 and control
figuretreat1 <- ggarrange(treat1betas, treatcbetas,
                    labels = c("Treatment 1", "Control"), # fix placement of these
                    ncol = 1, nrow = 2)

figuretreat1

### TREATMENT 2 FIGURES ----

treat2coef <-ba.coef[Treatment=="2", value, by = .(stage.ct)]


# TREAT 2
cbPalette = c("red", "blue")

treat2betas <- ggplot(treat2coef, aes(stage.ct, (value))) +
  geom_boxplot(aes(fill = stage.ct),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = stage.ct),
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
  #ggtitle("c) change between control and case") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  ylim(-15,15)

treat2betas

#CONTROL

cbPalette = c("red", "blue")
treatcbetas <- ggplot(treatccoef, aes(stage.ct, (value))) +
  geom_boxplot(aes(fill = stage.ct),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = stage.ct),
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
  #ggtitle("c) change between control and case") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

treatcbetas

##Combined treat 2 and control
figuretreat2 <- ggarrange(treat2betas, treatcbetas,
                          labels = c("Treatment 2", "Control"),
                          ncol = 1, nrow = 2)

figuretreat2

### TREATMENT 3 FIGURES ----

treat3coef <-ba.coef[Treatment=="3", value, by = .(stage.ct)]


# TREAT 3
cbPalette = c("red", "blue")

treat3betas <- ggplot(treat3coef, aes(stage.ct, (value))) +
  geom_boxplot(aes(fill = stage.ct),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = stage.ct),
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
  #ggtitle("c) change between control and case") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

treat3betas

#CONTROL

cbPalette = c("red", "blue")
treatcbetas <- ggplot(treatccoef, aes(stage.ct, (value))) +
  geom_boxplot(aes(fill = stage.ct),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = stage.ct),
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
  #ggtitle("c) change between control and case") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) + 
  ylim(-15,15)

treatcbetas

##Combined treat 3 and control
figuretreat3 <- ggarrange(treat3betas, treatcbetas,
                          labels = c("Treatment 3", "Control"),
                          ncol = 1, nrow = 2)

figuretreat3
