### RSS ###
# Julie Turner
# started 13 May 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 
          'tidyr', 'ggplot2','survival','forcats', 'patchwork')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'
dat <- readRDS('Data/derived/ssa30ghosts.Rds')
#dat <- dat[Stage!="Acc"] ## Can't limit to ToD=night because it won't work in interactions
dat$Stage <- factor(dat$Stage, levels = c("Acc", "B","A"))
dat$ToD_start <- as.factor(dat$ToD_start)
dat$Precipitation <- as.factor(dat$Precipitation)

# list of all snails
snails <- unique(dat$snail)


### FUNCTIONS ----
# function for model list
list_models <- function(resp, expl, DT) {
  list(list(clogit(reformulate(expl, resp),
              model = T, data = DT)))
}

list_predict <- function(mod, ND) {
  mapply(function(m, n) predict(m, newdata = n, type = 'lp'),
         m = mod, n = ND)
}

#' @param ls list column
#' @param val constant value
dif_list <- function(ls, val) {
  list(lapply(ls, function(l) l - val))
}

#### CORE ====
# Setup model name, explanatory and response variables
setup <- data.table(
  model = 'core',
  snail = unique(dat$snail),
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + Precipitation:log_sl + strata(step_id_)'
)

# Which individuals should be dropped?
corebad <- 'P11a'
setup[model == 'core', bad := snail %in% corebad]


# Which bricks do we want to keep?
setup[model == 'core', lsbricks := list(c("C", "1", "2", "3"))]

# Run only on *good* individual
setup[!(bad), mod :=
        list_models(response, explanatory,
                    dat[ghostbricks %in% unlist(lsbricks) &
                          snail == .BY[[1]]]),
      by = snail]

# Setup new data
setup[!(bad) & model == 'core', newdat := 
        list(list(dat[ghostbricks %in% unlist(lsbricks) & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Precipitation = factor('no', levels = levels(Precipitation)),
                        step_id_ = step_id_[1])])),
      by = snail]
 

# Predict
setup[!(bad), pred := list_predict(mod, newdat), by = snail]
core <- copy(setup)


# core_models[,"nbricks"] <- substr(core_models$snail, 3, 3)
# core_models[,"nbricks"] <- ifelse(core_models$nbricks=="4", "0", core_models$nbricks)
# core_models[,"Disturbance"] <- ifelse(core_models$nbricks=="0", "Control", "Disturbed")

#saveRDS(coreOUT, '~/snails/Data/derived/CoreModel.Rds')


### P1 and P2 ----
# Setup model name, explanatory and response variables
# Cross join individuals and bricks of interest
setup <- CJ(
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  model = 'p1',
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + log(edgedist_end + 1):Stage + log(brickdist_end + 1):Stage + log_sl:Stage + cos_ta:Stage + strata(step_id_)'
)


# Which individuals should be dropped?
p1bad <- c("P24b", "P11a", "P21a", "O12b", "O22b", "P12b", 
           "P22b", "P23a", "P23b", "O11a", "O13a")
setup[model == 'p1', bad := snail %in% p1bad]


# Run only on *good* individuals and those with > 0 rows
setup[!(bad), n := 
      dat[ghostbricks == .BY[[2]] & snail == .BY[[1]], .N],
      by = .(snail, brick)]

setup[!(bad) & n != 0, mod := 
        list_models(response, explanatory,
                    dat[ghostbricks == .BY[[2]] & snail == .BY[[1]]]),
      by = .(snail, brick)]

# Setup new data
# h2 before
setup[!(bad) & n != 0, h2Bdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = mean(edgedist_end, na.rm = TRUE),
                        brickdist_end = mean(brickdist_end, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h2 After
setup[!(bad) & n != 0, h2Adat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_end = mean(edgedist_end, na.rm = TRUE),
                        brickdist_end = mean(brickdist_end, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 before edge
setup[!(bad) & n != 0, h1Bedgedat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = seq(0, max(edgedist_end, na.rm = TRUE), length.out = 100),
                        brickdist_end = mean(brickdist_end, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 after edge
setup[!(bad) & n != 0, h1Aedgedat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_end = seq(0, max(edgedist_end, na.rm = TRUE), length.out = 100),
                        brickdist_end = mean(brickdist_end, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 before brick
setup[!(bad) & n != 0, h1Bbrickdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = mean(edgedist_end, na.rm = TRUE),
                        brickdist_end = seq(0, max(brickdist_end, na.rm = TRUE), length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 after edge
setup[!(bad) & n != 0, h1Abrickdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_end = mean(edgedist_end, na.rm = TRUE),
                        brickdist_end = seq(0, max(brickdist_end, na.rm = TRUE), length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# Predict
setup[!(bad) & n != 0, h2B := list_predict(mod, h2Bdat),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h2A := list_predict(mod, h2Adat),
      by = .(snail, brick)]

setup[!(bad) & n != 0, h1Bedge := list(list(list_predict(mod, h1Bedgedat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xBedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(edgedist_end = seq(0, max(edgedist_end, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Aedge := list(list(list_predict(mod, h1Aedgedat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xAedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(edgedist_end = seq(0, max(edgedist_end, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Bbrick := list(list(list_predict(mod, h1Bbrickdat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xBbrick := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(brickdist_end = seq(0, max(edgedist_end, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Abrick := list(list(list_predict(mod, h1Abrickdat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xAbrick := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                                .(brickdist_end = seq(0, max(edgedist_end, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]

rss <- setup[!(bad) & n > 0,.(snail, brick, h1Bedge, h1Aedge, h1Bbrick, h1Abrick, h2B, h2A, 
                              xBedge, xAedge, xBbrick, xAbrick)]


rss[, rssBedge := dif_list(h1Bedge, h2B),
      by = .(snail, brick)]
rss[, stepBedge := list(list(seq(0,max(unlist(xBedge)), length.out = 100))),
    by = .( brick)]
rss[, rssAedge := dif_list(h1Aedge, h2A),
    by = .(snail, brick)]
rss[, stepAedge := list(list(seq(0,max(unlist(xAedge)), length.out = 100))),
    by = .( brick)]
rss[, rssBbrick := dif_list(h1Bbrick, h2B),
    by = .(snail, brick)]
rss[, stepBbrick := list(list(seq(0,max(unlist(xBbrick)), length.out = 100))),
    by = .( brick)]
rss[, rssAbrick := dif_list(h1Abrick, h2A),
    by = .(snail, brick)]
rss[, stepAbrick := list(list(seq(0,max(unlist(xAbrick)), length.out = 100))),
    by = .( brick)]

rss.long <- rss[, .(rss = unlist(rssBedge), x = unlist(xBedge), step = unlist(stepBedge), var = 'edgedist', BA = 'before', model = 'p1'),
    by = .(snail, brick)]
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAedge), x = unlist(xAedge), step = unlist(stepAedge), var = 'edgedist', BA = 'after', model = 'p1'),
                                by = .(snail, brick)])
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssBbrick), x = unlist(xBbrick), step = unlist(stepBbrick), var = 'brickdist', BA = 'before', model = 'p1'),
                                by = .(snail, brick)])
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAbrick), x = unlist(xAbrick), step = unlist(stepAbrick), var = 'brickdist', BA = 'after', model = 'p1'),
                                by = .(snail, brick)])

  
p1 <- copy(rss.long)


# P1.g1.Out[,"nbricks"] <- 1
# P1.g2.Out[,"nbricks"] <- 2
# P1.g3.Out[,"nbricks"] <- 3
# P1.treats.Out[,"nbricks"] <- substr(P1.treats.Out$snail, 3, 3)
# 
# P1.g1.Out[,"Disturbance"] <- "Control"
# P1.g2.Out[,"Disturbance"] <- "Control"
# P1.g3.Out[,"Disturbance"] <- "Control"
# P1.treats.Out[,"Disturbance"] <- "Disturbed"
# 
# P1ModelOut <- rbind(P1.g1.Out, P1.g2.Out, P1.g3.Out, P1.treats.Out)

#saveRDS(P1ModelOut, '~/snails/Data/derived/P1Model.Rds')

p1[,'disturbance'] <- ifelse(p1$brick %like% 'g', 'undisturbed', 'disturbed')
p1[,'snails2'] <- paste(p1$snail, p1$brick, sep = '.')
p1[,disturbance.rss:=mean(rss, na.rm = T), by=.(step, disturbance)]
p1[,brick.rss:=mean(rss), by=.(step, brick)]

p1.edge.before <- ggplot(data=p1[var == 'edgedist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
                         aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1[var == 'edgedist'& BA=='before'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
 geom_smooth(size = 1.5, aes(fill = disturbance), method = 'glm', se = F) +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("before")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p1.edge.after <- ggplot(data=p1[var == 'edgedist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
                        aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = disturbance), method = 'glm', se = F) +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("after")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p1.edge.before|p1.edge.after


p1.brick.before <- ggplot(data=p1[var == 'brickdist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
                         aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = disturbance), method = 'glm', se = F) +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  ggtitle("before")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p1.brick.after <- ggplot(data=p1[var == 'brickdist'& BA=='after'& brick != 'g1' & brick != 'g3'], 
                        aes(x, rss, colour=disturbance)) +
  geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1[var == 'brickdist'& BA=='after'],aes(step,mean.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
   geom_smooth(size = 1.5, aes(fill = disturbance), method = 'glm', se = F) +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  ggtitle("after")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p1.brick.before|p1.brick.after


## p2 graphs

p2.edge.before <- ggplot(data=p1[var == 'edgedist'& BA=='before'], 
                         aes(x, rss, colour=brick)) +
  geom_line(aes(group = snails2,alpha = .0001), linetype ='twodash', show.legend = F) +
  geom_line(data=p1[var == 'edgedist'& BA=='before'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  # geom_smooth(size = 1.5, aes(fill = brick), method = 'loess') +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("before")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.edge.after <- ggplot(data=p1[var == 'edgedist'& BA=='after'], 
                        aes(x, rss, colour=brick)) +
  geom_line(aes(group = snails2,alpha = .0001), linetype ='twodash', show.legend = F) +
  geom_line(data=p1[var == 'edgedist'& BA=='after'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  # geom_smooth(size = 1.5, aes(fill = brick), method = 'loess') +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from edge (cm)") +
  ggtitle("after")  +
  #ylim(-0.7,1.3) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.edge.before|p2.edge.after


p2.brick.before <- ggplot(data=p1[var == 'brickdist'& BA=='before'], 
                          aes(x, rss, colour=brick)) +
  geom_line(aes(group = snails2,alpha = .0001), linetype ='twodash', show.legend = F) +
  geom_line(data=p1[var == 'brickdist'& BA=='before'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  #geom_smooth(size = 1.5, aes(fill = brick), method = 'loess') +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  ggtitle("before")  +
  ylim(-100,50) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.brick.after <- ggplot(data=p1[var == 'brickdist'& BA=='after'], 
                         aes(x, rss, colour=brick)) +
  geom_line(aes(group = snails2,alpha = .0001), linetype ='twodash', show.legend = F) +
  geom_line(data=p1[var == 'brickdist'& BA=='after'],aes(step,brick.rss, group = brick), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  # geom_smooth(size = 1.5, aes(fill = brick), method = 'loess') +
  # geom_line(data=logRSS.pop[var == 'forest'& ttd=='1 day'], aes(x, rss, colour=COD)) +
  geom_hline(yintercept = 0,colour = "black",lty = 2, size = .7) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  theme_bw()  + theme(
    #panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .7)) +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
  ylab("logRSS") + xlab("Distance from brick (cm)") +
  ggtitle("after")  +
  ylim(-100,50) +
  # scale_colour_manual("", values = c("gray", "black", "gray33", 'blue'))  +  
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))

p2.brick.before|p2.brick.after

#### P3 and P4 ====

# Setup model name, explanatory and response variables
# Cross join individuals and bricks of interest
setup <- CJ(
  snail = unique(dat$snail),
  brick = c("1", "2", "3", "g1", "g2", "g3"),
  model = 'p3',
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + 
  log(edgedist_start + 1):log_sl:Stage + log(edgedist_start + 1):cos_ta:Stage + 
  log(brickdist_start + 1):log_sl:Stage + log(brickdist_start + 1):cos_ta:Stage + 
  log_sl:Stage + cos_ta:Stage + strata(step_id_)'
)


# Which individuals should be dropped?
p3bad <- c("P24b", "O24a", "P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "O13a", "P23a", "P23b", "O22a")
setup[model == 'p3', bad := snail %in% p3bad]


# Run only on *good* individual and those with > 0 rows
setup[!(bad), n := 
        dat[ghostbricks == .BY[[2]] & snail == .BY[[1]], .N],
      by = .(snail, brick)]

setup[!(bad) & n != 0, mod := 
        list_models(response, explanatory,
                    dat[ghostbricks == .BY[[2]] & snail == .BY[[1]]]),
      by = .(snail, brick)]

# Setup new data
# h2 before
setup[!(bad) & n != 0, h2Bdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_start = mean(edgedist_start, na.rm = TRUE),
                        brickdist_start = mean(brickdist_start, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h2 After
setup[!(bad) & n != 0, h2Adat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_start = mean(edgedist_start, na.rm = TRUE),
                        brickdist_start = mean(brickdist_start, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 before edge
setup[!(bad) & n != 0, h1Bedgedat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_start = seq(0, max(edgedist_start, na.rm = TRUE), length.out = 100),
                        brickdist_start = mean(brickdist_start, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 after edge
setup[!(bad) & n != 0, h1Aedgedat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_start = seq(0, max(edgedist_start, na.rm = TRUE), length.out = 100),
                        brickdist_start = mean(brickdist_start, na.rm = TRUE),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 before brick
setup[!(bad) & n != 0, h1Bbrickdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_start = mean(edgedist_start, na.rm = TRUE),
                        brickdist_start = seq(0, max(brickdist_start, na.rm = TRUE), length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# h1 after edge
setup[!(bad) & n != 0, h1Abrickdat :=
        list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                      .(log_sl = mean(log_sl),
                        cos_ta = mean(cos_ta),
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = mean(Temperature),
                        Stage = factor('A', levels = levels(Stage)),
                        edgedist_start = mean(edgedist_start, na.rm = TRUE),
                        brickdist_start = seq(0, max(brickdist_start, na.rm = TRUE), length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail, brick)]

# Predict
setup[!(bad) & n != 0, h2B := list_predict(mod, h2Bdat),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h2A := list_predict(mod, h2Adat),
      by = .(snail, brick)]

setup[!(bad) & n != 0, h1Bedge := list(list(list_predict(mod, h1Bedgedat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xBedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(edgedist_start = seq(0, max(edgedist_start, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Aedge := list(list(list_predict(mod, h1Aedgedat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xAedge := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                               .(edgedist_start = seq(0, max(edgedist_start, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Bbrick := list(list(list_predict(mod, h1Bbrickdat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xBbrick := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                                .(brickdist_start = seq(0, max(edgedist_start, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]
setup[!(bad) & n != 0, h1Abrick := list(list(list_predict(mod, h1Abrickdat))),
      by = .(snail, brick)]
setup[!(bad) & n != 0, xAbrick := list(list(dat[ghostbricks == .BY[[2]] & snail == .BY[[1]],
                                                .(brickdist_start = seq(0, max(edgedist_start, na.rm = TRUE), length.out = 100))])),
      by = .(snail, brick)]

rss <- setup[!(bad) & n > 0,.(snail, brick, h1Bedge, h1Aedge, h1Bbrick, h1Abrick, h2B, h2A, 
                              xBedge, xAedge, xBbrick, xAbrick)]


rss[, rssBedge := dif_list(h1Bedge, h2B),
    by = .(snail, brick)]
rss[, rssAedge := dif_list(h1Aedge, h2A),
    by = .(snail, brick)]
rss[, rssBbrick := dif_list(h1Bbrick, h2B),
    by = .(snail, brick)]
rss[, rssAbrick := dif_list(h1Abrick, h2A),
    by = .(snail, brick)]

rss.long <- rss[, .(rss = unlist(rssBedge), x = unlist(xBedge), var = 'edgedist', BA = 'before', model = 'p1'),
                by = .(snail, brick)]
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAedge), x = unlist(xAedge), var = 'edgedist', BA = 'after', model = 'p1'),
                                by = .(snail, brick)])
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssBbrick), x = unlist(xBbrick), var = 'brickdist', BA = 'before', model = 'p1'),
                                by = .(snail, brick)])
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssAbrick), x = unlist(xAbrick), var = 'brickdist', BA = 'after', model = 'p1'),
                                by = .(snail, brick)])


p2 <- copy(rss.long)


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

badsnails <- c("P11a", "P21a", "O11a")
P2.treat1.Out<- dat[!(snail %in% badsnails) & Treatment=="1",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O12b", "O22b", "P12b", "P22b")
P2.treat2.Out<- dat[!(snail %in% badsnails) & Treatment=="2",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O13a","P23b", "P23a")
P2.treat3.Out<- dat[!(snail %in% badsnails) & Treatment=="3",
                    {
                      print(.BY[[1]])
                      P2Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Stage, step_id_)
                    },
                    by = .(snail)]

P2.g1.Out[,"nbricks"] <- 1
P2.g2.Out[,"nbricks"] <- 2
P2.g3.Out[,"nbricks"] <- 3
P2.treat1.Out[,"nbricks"] <- 1
P2.treat2.Out[,"nbricks"] <- 2
P2.treat3.Out[,"nbricks"] <- 3

P2.g1.Out[,"Disturbance"] <- "Control"
P2.g2.Out[,"Disturbance"] <- "Control"
P2.g3.Out[,"Disturbance"] <- "Control"
P2.treat1.Out[,"Disturbance"] <- "Disturbed"
P2.treat2.Out[,"Disturbance"] <- "Disturbed"
P2.treat3.Out[,"Disturbance"] <- "Disturbed"

P2ModelOut <- rbind(P2.g1.Out, P2.g2.Out, P2.g3.Out, P2.treat1.Out, P2.treat2.Out, P2.treat3.Out)

#saveRDS(P2ModelOut, '~/snails/Data/derived/P2Model.Rds')

#### P3 ====

P3Model <- function(y, SL, TA, ToD, Temp, edgedist_start, brickdist_start,
                    Treatment, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_start:SL:Stage + edgedist_start:TA:Stage +
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

badsnails <- c("P24b", "O24a")
P3.g3.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g3",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P3.g2.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g2",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P3.g1.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g1",
                {
                  print(.BY[[1]])
                  P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Treatment, Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P11a", "P21a", "O12b", "O22b", "P12b", "P22b", "O13a", "P23a", "P23b", "O22a")
P3.treats.Out<- dat[!(snail %in% badsnails) & Treatment!="C",
                    {
                      print(.BY[[1]])
                      P3Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_end + 1), 
                              log(brickdist_end + 1), Treatment, Stage, step_id_)
                    },
                    by = .(snail)]

P3.g1.Out[,"nbricks"] <- 1
P3.g2.Out[,"nbricks"] <- 2
P3.g3.Out[,"nbricks"] <- 3
P3.treats.Out[,"nbricks"] <- substr(P3.treats.Out$snail, 3, 3)

P3.g1.Out[,"Disturbance"] <- "Control"
P3.g2.Out[,"Disturbance"] <- "Control"
P3.g3.Out[,"Disturbance"] <- "Control"
P3.treats.Out[,"Disturbance"] <- "Disturbed"


P3ModelOut <- rbind(P3.g1.Out, P3.g2.Out, P3.g3.Out, P3.treats.Out)
#saveRDS(P3ModelOut, '~/snails/Data/derived/P3Model.Rds')

#### P4 ====

P4Model<- function(y, SL, TA, ToD, Temp, edgedist_start, brickdist_start, Stage, strata1) {
  # Make the model
  model <- clogit(y ~ SL + TA + ToD:SL +Temp:SL + edgedist_start:SL:Stage +
                    edgedist_start:TA:Stage + brickdist_start:SL:Stage + brickdist_start:TA:Stage + SL:Stage + TA:Stage + strata(strata1))
  
  sum.model <- summary(model)$coefficients
  # Transpose the coef of the model and cast as data.table
  term <- c('coef','hr','se','z','p')
  coefOut <- data.table(t(sum.model))
  
  # Return combined columns
  # print(summary(model))
  print(AIC(model))
  return(data.table(term, coefOut, AIC=AIC(model)))
}

badsnails <- c("P24b", "O24a")
P4.g3.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g3",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P4.g2.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g2",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P24b", "O24a")
P4.g1.Out<- dat[!(snail %in% badsnails) & ghostbricks=="g1",
                {
                  print(.BY[[1]])
                  P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                          log(brickdist_start + 1), Stage, step_id_)
                },
                by = .(snail)]

badsnails <- c("P11a", "P21a")
P4.treat1.Out<- dat[!(snail %in% badsnails) & Treatment=="1",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("P12b", "O12b", "P22b", "O22b", "O22a")
P4.treat2.Out<- dat[!(snail %in% badsnails) & Treatment=="2",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Stage, step_id_)
                    },
                    by = .(snail)]

badsnails <- c("O13a", "P23b", "P13a")
P4.treat3.Out<- dat[!(snail %in% badsnails) & Treatment=="3",
                    {
                      print(.BY[[1]])
                      P4Model(case_, log_sl, cos_ta, ToD_start, Temperature, log(edgedist_start + 1), 
                              log(brickdist_start + 1), Stage, step_id_)
                    },
                    by = .(snail)]

P4.g1.Out[,"nbricks"] <- 1
P4.g2.Out[,"nbricks"] <- 2
P4.g3.Out[,"nbricks"] <- 3
P4.treat1.Out[,"nbricks"] <- 1
P4.treat2.Out[,"nbricks"] <- 2
P4.treat3.Out[,"nbricks"] <- 3

P4.g1.Out[,"Disturbance"] <- "Control"
P4.g2.Out[,"Disturbance"] <- "Control"
P4.g3.Out[,"Disturbance"] <- "Control"
P4.treat1.Out[,"Disturbance"] <- "Disturbed"
P4.treat2.Out[,"Disturbance"] <- "Disturbed"
P4.treat3.Out[,"Disturbance"] <- "Disturbed"

P4ModelOut <- rbind(P4.g1.Out, P4.g2.Out, P4.g3.Out, P4.treat1.Out, P4.treat2.Out, P4.treat3.Out)

#saveRDS(P4ModelOut, '~/snails/Data/derived/P4Model.Rds')
