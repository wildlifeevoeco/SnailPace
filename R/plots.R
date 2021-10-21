# === Plots ---------------------------------------------------------------




# RSS ---------------------------------------------------------------------
theme_rss <- theme_bw() + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = 'black', size = .7),
  plot.title = element_text(size = 12, hjust = 0.05),
  axis.text.x = element_text(size = 12),
  axis.title = element_text(size = 15),
  axis.text.y = element_text(size = 12),
  axis.ticks.length = unit(-0.25, 'cm'),
  legend.key = element_blank(),
  legend.position = 'right', 
  legend.text = element_text(size = 10)
)

plot_rss_edge <- function(rss) {
  rss[, disturbance := ifelse(ghostbricks %like% 'g', 'undisturbed', 'disturbed')]
  
  ggplot(data = rss[var == 'edge' & ghostbricks != 'g2' & ghostbricks != 'g3'],
         aes(x, rss, colour = ghostbricks)) +
    geom_line(aes(group = id_treat, alpha = .0001), 
              linetype = 'twodash',
              show.legend = FALSE) +
    geom_smooth(
      size = 1.5,
      aes(fill = ghostbricks),
      se = TRUE,
      method = 'glm'
    ) +
    geom_hline(
      yintercept = 0,
      colour = 'black',
      lty = 2,
      size = .7
    ) +
    labs(x = 'Distance from edge (cm)', y = 'logRSS') +
    facet_wrap('stage') +
    theme_rss
}


plot_rss_brick <- function(rss) {
  rss[, disturbance := ifelse(ghostbricks %like% 'g', 'undisturbed', 'disturbed')]
  
  ggplot(data = rss[var == 'brick' &
                      ghostbricks != 'g2' & ghostbricks != 'g3'],
         aes(x, rss, colour = ghostbricks)) +
    geom_line(aes(group = id_treat, alpha = .0001),
              linetype = 'twodash',
              show.legend = FALSE) +
    geom_smooth(
      size = 1.5,
      aes(fill = ghostbricks),
      se = TRUE,
      method = 'glm'
    ) +
    geom_hline(
      yintercept = 0,
      colour = 'black',
      lty = 2,
      size = .7
    ) +
    labs(x = 'Distance from edge (cm)', y = 'logRSS') +
    facet_wrap('stage') +
    theme_rss
}




# Speed -------------------------------------------------------------------
todo <- function(variables) {
  # TODO: see speed.R, delete after
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
    geom_line(aes(group= id_treat), linetype = 'dashed', size=1, alpha=.5) +
    #geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
    geom_smooth(size = 2, se = F, method = 'glm')+
    theme_classic() +
    theme(text = element_text(size=15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) + 
    #  theme(legend.position = "none") +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("before") +
    ylim(0,5000) +
    scale_color_colorblind() +
    xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")
  
  edge.spd.before
  
  edge.spd.after <- ggplot(data=speed[id != 'O24b'], aes(x=edist, y=ed.spd.after, color = brick)) + 
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
    ylim(0,5000) +
    scale_color_colorblind() +
    xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")
  
  edge.spd.before + edge.spd.after
  
}





# Binomial ----------------------------------------------------------------
plot_binomial <- function(model) {
  pred.intx <-
    ggpredict(model, terms = c('temp', 'stage', 'treatment'))
  
  plot(pred.intx) + 
    scale_color_colorblind() + 
    scale_fill_colorblind()
}
