# === Plots ---------------------------------------------------------------




# RSS ---------------------------------------------------------------------
theme_rss <- theme_bw() + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = 'black', size = .7),
  plot.title = element_text(size = 12, hjust = 0.05),
  axis.text.x = element_text(size = 12, margin = margin(10, 10, 10, 10, 'pt')),
  axis.title = element_text(size = 15),
  axis.text.y = element_text(size = 12, margin = margin(10, 10, 10, 10, 'pt')),
  axis.ticks.length = unit(-0.25, 'cm'),
  legend.key = element_blank(),
  legend.position = 'right', 
  legend.text = element_text(size = 10)
)

prep_rss <- function(rss) {
  rss[, treatment := factor(
    ghostbricks,
    levels = c('g1', '1', '2', '3'),
    labels = c('control', '1', '2', '4')
  )]
  rss[stage == 'B', stage := 'before']
  rss[stage == 'A', stage := 'after']
  rss[, disturbance := ifelse(ghostbricks %like% 'g', 'undisturbed', 'disturbed')]
  return(rss)
}

plot_rss_edge <- function(rss) {
  prep_rss(rss)
  
  ggplot(data = rss[var == 'edge'], aes(x, rss, colour = treatment)) +
    geom_line(aes(group = id_treat, alpha = .0001),
              linetype = 'twodash',
              show.legend = F) +
    geom_smooth(size = 1.5,
                aes(fill = treatment),
                se = F,
                method = 'glm') +
    geom_hline(
      yintercept = 0,
      colour = 'black',
      lty = 2,
      size = .7
    ) +
    ylab('logRSS') + xlab('Distance from edge (cm)') +
    facet_wrap(~stage) +
    scale_color_colorblind() +
    theme_rss
}


plot_rss_brick <- function(rss) {
  prep_rss(rss)
  
  ggplot(data = rss[var == 'brick'],
         aes(x, rss, colour = treatment)) +
    geom_line(aes(group = id_treat, alpha = .0001),
              linetype = 'twodash',
              show.legend = F) +
    geom_smooth(size = 1.5,
                aes(fill = treatment),
                se = F,
                method = 'glm') +
    geom_hline(
      yintercept = 0,
      colour = 'black',
      lty = 2,
      size = .7
    ) +
    ylab('logRSS') + xlab('Distance from brick (cm)') +
    facet_wrap('stage') +
    ylim(-50,50) +
    scale_color_colorblind() +
    theme_rss
}




# Speed -------------------------------------------------------------------
theme_speed <- theme_bw() + 
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) +
  theme(plot.margin = margin(0.1, 1, .1, .1, 'cm'))

plot_speed_brick <- function(speed) {
  brick_speed_before <- 
    ggplot(data = speed,
           aes(x = bdist, y = bd.spd.before, color = brick)) +
    geom_line(
      aes(group = id_treat, linetype = 'dashed'),
      linetype = 'dashed',
      size = 1,
      alpha = .5
    ) +
    geom_smooth(size = 2, se = F, method = 'glm') +
    scale_color_colorblind() +
    ggtitle('before ') +
    ylim(0, 7000) +
    xlab('Distance from brick (cm)') + ylab('Speed (cm per hour)') + 
    theme_speed
  
  brick_speed_after <-
    ggplot(data = speed[id != 'O24b'], 
           aes(x = bdist, y = bd.spd.after, color = brick)) +
    geom_line(
      aes(group = id_treat),
      linetype = 'dashed',
      size = 1,
      alpha = .5
    ) +
    geom_smooth(size = 2, se = F, method = 'glm') +
    ggtitle('after ') +
    ylim(0, 7000) +
    scale_color_colorblind() +
    xlab('Distance from brick (cm)') + ylab('Speed (cm per hour)') + 
    theme_speed
  
  brick_speed_before + brick_speed_after
}

plot_speed_edge <- function(speed) {
  edge_speed_before <- 
    ggplot(data = speed[id != 'O24b'], 
           aes(x = edist, y = ed.spd.before, color = brick)) + 
    geom_line(
      aes(group = id_treat),
      linetype = 'dashed',
      size = 1,
      alpha = .5
    ) +
    geom_smooth(size = 2, se = F, method = 'glm') +
    ggtitle('before') +
    ylim(0, 5000) +
    scale_color_colorblind() +
    xlab('Distance from edge (cm)') + ylab('Speed (cm per hour)') + 
    theme_speed
  
  edge_speed_after <-
    ggplot(data = speed[id != 'O24b'],
           aes(x = edist, y = ed.spd.after, color = brick)) +
    geom_line(
      aes(group = id_treat),
      linetype = 'dashed',
      size = 1,
      alpha = .5
    ) +
    geom_smooth(size = 2, se = F, method = 'glm') +
    ggtitle('after ') +
    ylim(0, 5000) +
    scale_color_colorblind() +
    xlab('Distance from edge (cm)') + ylab('Speed (cm per hour)') + 
    theme_speed
  
  edge_speed_before + edge_speed_after
}



# Binomial ----------------------------------------------------------------
plot_binomial <- function(model) {
  pred.intx <-
    ggpredict(model, terms = c('temp', 'stage', 'treatment'))
  
  plot(pred.intx) + 
    scale_color_colorblind() + 
    scale_fill_colorblind()
}
