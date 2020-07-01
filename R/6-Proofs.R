### Proofs ###
# Julie Turner
# started 30 June 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'tidyr', 'ggplot2', 'patchwork')
lapply(libs, require, character.only = TRUE)

### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'
p1.rss.all<-readRDS('Data/derived/p1rss_1-2hr.Rds')
p1.rss.all[,'disturbance'] <- ifelse(p1.rss.all$brick %like% 'g', 'undisturbed', 'disturbed')
p1.rss.all[,'snails2'] <- paste(p1.rss.all$snail, p1.rss.all$brick, sep = '.')
p1.rss.all[,disturbance.rss:=mean(rss, na.rm = T), by=.(step, disturbance)]
p1.rss.all[,brick.rss:=mean(rss), by=.(step, brick)]

speed.all <- readRDS('Data/derived/speed_1-2hr.Rds')

set.seed(53)
iterations <- 100
#### proof 1hr vs 2hr ####

speed.hr <- lapply(seq(0, iterations), function(iter){
  sub<-unique(speed.all[,.(snail, samp)])
  if (iter == 0) {
    sub[, randSamp := samp]
  } else {
    sub[, randSamp := sample(samp)]
  }
  sub[,iteration:=iter]
})
speed.ls <- rbindlist(speed.hr)
hr.proof <- merge(speed.all, speed.ls, by= c('snail', 'samp'), allow.cartesian = T)
hr.proof[iteration == 0, observed := 'yes']
hr.proof[iteration != 0, observed := 'no']
hr.proof[,'snailsIter'] <- paste(hr.proof$snails2, hr.proof$iteration, sep = '.')

p1 <- ggplot(data=hr.proof [snail != 'O11b' & ed.spd.before>=0], aes(x=edist, y=ed.spd.before, color = randSamp)) + 
  geom_line(aes(group = snailsIter, linetype = observed), size=1, alpha=.5) +
  geom_smooth(aes(linetype = observed, fill= randSamp), size = 2, se = T, method = 'lm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  #ylim(-5, 75) +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a) before ") +
  xlab("Distance from edge (cm)") + ylab("Speed (m per hour)")
p1


#### proof before disturbance ####
rss.dist <- lapply(seq(0, iterations), function(iter){
  sub<-unique(p1.rss.all[,.(snail, disturbance)])
  if (iter == 0) {
    sub[, randSamp := disturbance]
  } else {
    sub[, randSamp := sample(disturbance)]
  }
  sub[,iteration:=iter]
})
rss.dist.ls <- rbindlist(rss.dist)
dist.proof <- merge(p1.rss.all, rss.dist.ls, by= c('snail', 'disturbance'), allow.cartesian = T)
dist.proof[iteration == 0, observed := 'yes']
dist.proof[iteration != 0, observed := 'no']
dist.proof[,'snailsIter'] <- paste(dist.proof$snails2, dist.proof$iteration, sep = '.')

p2 <- ggplot(data=dist.proof[var == 'edgedist'& BA=='before' & brick != 'g1' & brick != 'g3'], 
             aes(x, rss, colour= randSamp)) +
  #geom_line(aes(group = snail,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'edgedist'& BA=='before'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = randSamp, linetype = observed), se = T, method = 'lm') +
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
p2

#### proof treatment diffs ####

