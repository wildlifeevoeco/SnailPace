### Binomial ###
# Julie Turner
# started 17 August 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'lubridate', 'lme4', 'broom.mixed', 'performance', 'ggeffects',
          'tidyr', 'ggplot2','survival', 'patchwork', 'AICcmodavg', 'ggthemes', 'targets')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'

tar_load(combtreats)
dat.hr <- combtreats
tar_load(distparams)
moveParams <- distparams
dat.snails <- fread('Data/derived/summary_movement.csv')


dat.hr$Stage <- factor(dat.hr$Stage, levels = c("Acc", "B","A"))
dat.hr$ToD_start <- as.factor(dat.hr$ToD_start)
dat.hr$Precipitation <- as.factor(dat.hr$Precipitation)



dat.obs <- dat.hr[case_==TRUE & ghostbricks != 'g1' & ghostbricks != 'g3' & ghostbricks != 'C' & !(Stage %like% 'Acc')]
dat.obs <- merge(dat.obs, dat.snails[, .(snail, name = id, disturbance, nTotal, nSteps, nMove, BnA)], by.x = 'id', by.y = 'snail')
dat.obs$disturbance <- factor(dat.obs$disturbance, levels = c('undisturbed', 'disturbed'))
dat.obs[,group:=ifelse(stage == 'B', 'before', 
                       ifelse(stage=='A' & ghostbricks =='g2', 'undisturbed', 'disturbed'))]

dat.obs[,moved:= ifelse(sl_==0,0,1)]
dat.obs[,nSteps := .N, by =.(id)]



dat.obs.prop <- unique(dat.obs[, .(id, group, disturbance, BnA, nSteps, propmove=sum(moved)/.N, 
                                   Temperature = mean(temp)), by = .(id, stage)])
dat.obs.prop$group <- factor(dat.obs.prop$group, levels = c('before', 'undisturbed', 'disturbed'))


dups <- dat.obs.prop[, if (.N>1) .SD, by=id]
#### ANALYSIS ----

intx.mod <- glmer(moved ~ (stage + stage:disturbance) * (temp) + (1|id), data = dat.obs, family = 'binomial')
summary(intx.mod)
tidy(intx.mod)
indivs <- tidy(intx.mod, effect = 'ran_vals')
check_model(intx.mod)
# 
# prop.intx.mod <- glm(propmove ~ Stage + Stage:disturbance + Temperature, data = dat.obs.prop, family = 'binomial')
# summary(prop.intx.mod)
# check_model(prop.intx.mod)

group.mod <- glmer(moved ~ group * temp + (1|id), data = dat.obs[id %in% dups$id], family = 'binomial')
summary(group.mod)
tidy(group.mod)
group.indivs <- tidy(group.mod, effect = 'ran_vals')
check_model(group.mod)


#### GRAPHS ----
pred.intx <- ggpredict(intx.mod, terms = c( 'temp','stage', 'disturbance'))
plot(pred.intx)

pred<- ggpredict(group.mod, terms = c('temp', 'group'))
pred<- ggpredict(group.mod, terms = c('group', 'temp'))
pred<- ggpredict(group.mod, terms = c('group'))
plot(pred)


propmove <- ggplot(dups[nSteps>=30], aes(group, (1-propmove), color = group))+
  geom_boxplot(aes(fill = group), alpha = 0.1, outlier.shape = NA, show.legend = F) + 
  geom_jitter() +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  xlab('') + ylab('Proportion of steps when snails do not move') +
  theme(legend.position = "none") +
  scale_color_colorblind() + scale_fill_colorblind()
propmove


