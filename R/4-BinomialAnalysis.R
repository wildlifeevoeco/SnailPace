### Binomial ###
# Julie Turner
# started 17 August 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'lubridate', 'lme4', 'broom.mixed',
          'tidyr', 'ggplot2','survival', 'patchwork', 'AICcmodavg', 'ggthemes')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'

dat.hr <- readRDS('Data/derived/ssa-exp-good-ghosts.Rds')
moveParams <- readRDS('Data/derived/moveParams-exp-goods.Rds')
dat.snails <- fread('Data/derived/summary_movement.csv')


dat.hr$Stage <- factor(dat.hr$Stage, levels = c("Acc", "B","A"))
dat.hr$ToD_start <- as.factor(dat.hr$ToD_start)
dat.hr$Precipitation <- as.factor(dat.hr$Precipitation)



dat.obs <- dat.hr[case_==TRUE & ghostbricks != 'g1' & ghostbricks != 'g3' & ghostbricks != 'C' & !(Stage %like% 'Acc')]
dat.obs <- merge(dat.obs, dat.snails, by = 'snail')
dat.obs$disturbance <- factor(dat.obs$disturbance, levels = c('undisturbed', 'disturbed'))
dat.obs[,group:=ifelse(Stage == 'B', 'before', 
                       ifelse(Stage=='A' & ghostbricks =='g2', 'undisturbed', 'disturbed'))]

dat.obs[,moved:= ifelse(sl_==0,0,1)]



dat.obs.prop <- unique(dat.obs[, .(id, group, disturbance, BnA, propmove=sum(moved)/.N, 
                                   Temperature = mean(Temperature)), by = .(snail, Stage)])


#### ANALYSIS ----

intx.mod <- glmer(moved ~ Stage + Stage:disturbance + Temperature + (1|id), data = dat.obs, family = 'binomial')
summary(intx.mod)
tidy(intx.mod)
indivs <- tidy(intx.mod, effect = 'ran_vals')


prop.intx.mod <- glm(propmove ~ Stage + Stage:disturbance + Temperature, data = dat.obs.prop, family = 'binomial')
summary(prop.intx.mod)

group.mod <- glmer(moved ~ group * Temperature + (1|id), data = dat.obs, family = 'binomial')
summary(group.mod)
tidy(group.mod)
group.indivs <- tidy(group.mod, effect = 'ran_vals')


#### GRAPHS ----

propmove <- ggplot(dat.obs.prop, aes(group, (1-propmove), color = group))+
  geom_boxplot(aes(fill = group), alpha = 0.1, outlier.shape = NA, show.legend = F) + 
  geom_jitter(aes(shape = BnA)) +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  xlab('') + ylab('Proportion of steps when snails do not move') +
  theme(legend.position = "none") +
  scale_color_colorblind() + scale_fill_colorblind()
propmove


