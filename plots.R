### polts ###
# Julie Turner
# revised 17 August 2021

### Packages ----
libs <- c('data.table', 'dplyr', 'ggeffects',
          'tidyr', 'ggplot2','patchwork', 'ggthemes', 'targets')
lapply(libs, require, character.only = TRUE)
tar_load(rss)
tar_load(predicted_speed)


#### RSS ####
rss <- rss[ghostbricks != 'g2' & ghostbricks != 'g3'] 
rss[,treatment:= factor(ghostbricks, levels = c('g1', '1', '2', '3'), labels = c('control', '1', '2', '4'))]
rss[stage=='B', stage:= 'before']
rss[stage=='A', stage:= 'after']

#### RSS PLOTS ####
rss.edge<- ggplot(data=rss[var == 'edge'], 
                     aes(x, rss, colour=treatment)) +
  geom_line(aes(group = id_treat,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = treatment),  se = F, method = 'glm') +
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
  facet_wrap('stage') +
  #ylim(-150,100) +
  scale_color_colorblind() +
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))
rss.edge

rss.brick<- ggplot(data=rss[var == 'brick' ], 
                      aes(x, rss, colour=treatment)) +
  geom_line(aes(group = id_treat,alpha = .0001), linetype ='twodash', show.legend = F) +
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  geom_smooth(size = 1.5, aes(fill = treatment),  se = F, method = 'glm') +
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
  facet_wrap('stage') +
  ylim(-50,50) +
  scale_color_colorblind() +
  theme(legend.key = element_blank()) + theme(legend.position = 'right') + theme(legend.text = element_text(size = 10))
rss.brick



#### SPEED ####
speed <- predicted_speed[,.(id, brick, bd.spd.before, bd.spd.after, bdist, ed.spd.after, ed.spd.before, edist, disturbance)]
speed[, id_treat := paste(id, brick, sep = '_')]
speed[bd.spd.before<0, bd.spd.before:=0]
speed[bd.spd.after<0, bd.spd.after:=0]
speed[ed.spd.before<0, ed.spd.before:=0]
speed[ed.spd.after<0, ed.spd.after:=0]

speed[,brick:= factor(brick, levels = c('g3', '1', '2', '3'), labels = c('control', '1', '2', '4'))]

#### SPEED PLOTS ####
brick.spd.before <- ggplot(data=speed, aes(x=bdist, y=bd.spd.before, color = brick)) + 
  geom_line(aes(group= id_treat, linetype = 'dashed'), linetype = 'dashed', size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(size = 2, se = F, method = 'glm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  scale_color_colorblind() +
  ggtitle("before ") +
  ylim(0,7000) +
  xlab("Distance from brick (cm)") + ylab("Speed (cm per hour)")
brick.spd.before

brick.spd.after <- ggplot(data=speed[id != 'O24b'], aes(x=bdist, y=bd.spd.after, color = brick)) + 
  geom_line(aes(group= id_treat), linetype = 'dashed', size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(size = 2, se = F, method = 'glm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("after ") +
  ylim(0,7000) +
  scale_color_colorblind() +
  xlab("Distance from brick (cm)") + ylab("Speed (cm per hour)")

brick.spd.before + brick.spd.after



edge.spd.before <- ggplot(data=speed[id != 'O24b'], aes(x=edist, y=ed.spd.before, color = brick)) + 
  geom_line(aes(group= id_treat), linetype = brick, size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(size = 2, se = F, method = 'glm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  scale_color_colorblind() +
  ggtitle("before ") +
  ylim(0,5000) +
  xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")
edge.spd.before

edge.spd.after <- ggplot(data=speed[id != 'O24b'], aes(x=edist, y=ed.spd.after, color = brick)) + 
  geom_line(aes(group= id_treat), linetype = 'dashed', size=1, alpha=.5) +
  #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(size = 2, se = T, method = 'glm')+
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("after ") +
  ylim(0,5000) +
  scale_color_colorblind() +
  xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")

edge.spd.before + edge.spd.after
