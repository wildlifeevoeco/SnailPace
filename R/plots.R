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
  
  # TODO: remove?
  #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
  #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
  #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
  #ylim(-150,100) +
  # scale_colour_manual('', values = c('gray', 'black', 'gray33', 'blue'))  +  

  
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
      
    # TODO: remove?
    #geom_line(data=p1.rss[var == 'brickdist'& BA=='after'],aes(step,disturbance.rss, group = disturbance), size = 1) +
    #geom_point(shape = 1, aes(alpha = .001), show.legend = F) +
    #geom_ribbon(aes(x, ymin = (rss - 1.96*se), ymax = (rss + 1.96*se), fill=COD, alpha = .2))+
    #ylim(-150,100) +
    # scale_colour_manual('', values = c('gray', 'black', 'gray33', 'blue'))  +  
      
      
}
