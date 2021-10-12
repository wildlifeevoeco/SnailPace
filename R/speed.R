# TODO: necessary?
# speed <- coefs[,.(bdist = unlist(bdist), 
#                   bd.spd.before = unlist(bd.spd.before), bd.spd.after = unlist(bd.spd.after),
#                   edist = unlist(edist),
#                   ed.spd.before = unlist(ed.spd.before), ed.spd.after = unlist(ed.spd.after),
#                   disturbance = ifelse(brick %like% 'g', 'undisturbed', 'disturbed')), by = .(id, brick)]

# TODO: add in
# speed[,'brick2'] <-gsub("[^0-9.-]", "", speed.1hr$brick)
# speed[, id_treat := paste(id, brick, sep = '_')]
# speed[bd.spd.before <0, bd.spd.before:=0]
# speed[bd.spd.after <0, bd.spd.after:=0]
# speed[ed.spd.before <0, ed.spd.before:=0]
# speed[ed.spd.after <0, ed.spd.after:=0]

# input: speed
plot_speed_brick <- function(DT) {
  DT[,'brick2'] <-gsub("[^0-9.-]", "", DT$brick)
  DT[, id_treat := paste(id, brick, sep = '_')]
  DT[bd.spd.before <0, bd.spd.before:=0]
  DT[bd.spd.after <0, bd.spd.after:=0]
  DT[ed.spd.before <0, ed.spd.before:=0]
  DT[ed.spd.after <0, ed.spd.after:=0]
  before <- ggplot(data=DT, aes(x=bdist, y=bd.spd.before, color = brick)) + 
    geom_line(aes(group= id_treat, linetype = brick), size=1, alpha=.5) +
    #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
    geom_smooth(size = 2, se = F, method = 'glm')+
    theme_classic() +
    theme(text = element_text(size=15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) + 
    #  theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("before ") +
    scale_color_colorblind() +
    xlab("Distance from brick (cm)") + ylab("Speed (cm per hour)")
  
  after <- ggplot(data=DT, aes(x=bdist, y=bd.spd.after, color = brick)) + 
    geom_line(aes(group= id_treat, linetype = brick), size=1, alpha=.5) +
    #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
    geom_smooth(size = 2, se = F, method = 'glm')+
    theme_classic() +
    theme(text = element_text(size=15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) + 
    #  theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("after ") +
    #ylim(0,1200) +
    scale_color_colorblind() +
    xlab("Distance from brick (cm)") + ylab("Speed (cm per hour)")
  
  before + after
}

plot_speed_edge <- function(DT) {
  before <- ggplot(data=DT, aes(x=edist, y=ed.spd.before, color = brick)) + 
    geom_line(aes(group= id_treat, linetype = brick), size=1, alpha=.5) +
    #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
    geom_smooth(size = 2, se = F, method = 'glm')+
    theme_classic() +
    theme(text = element_text(size=15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) + 
    #  theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("before ") +
    ylim(0,5000)+
    scale_color_colorblind() +
    xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")
  
  after <- ggplot(data=DT, aes(x=edist, y=ed.spd.after, color = brick)) + 
    geom_line(aes(group= id_treat, linetype = brick), size=1, alpha=.5) +
    #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
    geom_smooth(size = 2, se = F, method = 'glm')+
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
  
  before + after
}

