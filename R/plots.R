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

prep_rss <- function(DT) {
  setDT(DT)
  subDT <- DT[!ghostbricks %in% c('g2', 'g3')]
  subDT[, treatment := factor(
    ghostbricks,
    levels = c('g1', '1', '2', '3'),
    labels = c('control', '1', '2', '4')
  )]
  subDT[stage == 'B', stage := 'before']
  subDT[stage == 'A', stage := 'after']
  subDT[, disturbance := ifelse(ghostbricks %like% 'g', 'undisturbed', 'disturbed')]
  subDT
}

plot_rss_edge <- function(DT) {
  DT <- prep_rss(DT)
  
  g <- ggplot(data = DT[var == 'edge'], aes(x, rss, colour = treatment)) +
    geom_line(aes(group = id_treat, alpha = .0001),
              linetype = 'twodash',
              show.legend = FALSE) +
    geom_smooth(size = 1.5,
              aes(fill = treatment),
              se = FALSE,
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
  
  file_name <- file.path('figures', 'plot_rss_edge.png')
  ggsave(filename = file_name,
         plot = g, 
         width = 10,
         device = 'png')
  return(file_name)
}


plot_rss_brick <- function(DT) {
  DT <- prep_rss(DT)
  
  g <- ggplot(data = DT[var == 'brick'],
         aes(x, rss, colour = treatment)) +
    geom_line(aes(group = id_treat, alpha = .0001),
              linetype = 'twodash',
              show.legend = F) +
    geom_smooth(size = 1.5,
                aes(fill = treatment),
                se = FALSE,
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
  
  file_name <- file.path('figures', 'plot_rss_brick.png')
  ggsave(filename = file_name,
         plot = g,
         width = 10,
         device = 'png')
  return(file_name)
}




# Speed -------------------------------------------------------------------
theme_speed <- theme_bw() + 
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 15)) +
  theme(plot.margin = margin(0.1, 1, .1, .1, 'cm'))

prep_speed <- function(DT) {
  setDT(DT)
  
  DT[bd.spd.before < 0, bd.spd.before := 0]
  DT[bd.spd.after < 0, bd.spd.after := 0]
  DT[ed.spd.before < 0, ed.spd.before := 0]
  DT[ed.spd.after < 0, ed.spd.after := 0]
  
  DT[, brick := factor(
    brick,
    levels = c('g3', '1', '2', '3'),
    labels = c('control', '1', '2', '4')
  )]
  DT[, id_treat := paste(id, brick, sep = '_')]
  return(DT)
}

plot_speed_brick <- function(DT) {
  DT <- prep_speed(DT)
  brick_speed_before <- 
    ggplot(data = DT,
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
    ggplot(data = DT[id != 'O24b'], 
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
  
  g <- brick_speed_before + brick_speed_after +
    plot_layout(guides = 'collect')
  
  file_name <- file.path('figures', 'plot_speed_brick.png')
  ggsave(filename = file_name,
         plot = g, 
         width = 10,
         device = 'png')
  return(file_name)
}

plot_speed_edge <- function(DT) {
  DT <- prep_speed(DT)
  edge_speed_before <- 
    ggplot(data = DT[id != 'O24b'], 
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
    ggplot(data = DT[id != 'O24b'],
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
  
  g <- edge_speed_before + edge_speed_after +
    plot_layout(guides = 'collect')
  
  file_name <- file.path('figures', 'plot_speed_edge.png')
  ggsave(filename = file_name,
         plot = g,
         width = 10,
         device = 'png')
  return(file_name)
}



# Binomial ----------------------------------------------------------------
plot_binomial <- function(model) {
  pred.intx <-
    ggpredict(model, terms = c('temp', 'stage', 'treatment'))
  
  g <- plot(pred.intx) + 
    scale_color_colorblind() + 
    scale_fill_colorblind() + 
    labs(x = 'Temperature (°C)', y = 'Moved') + 
    theme_bw()
  
  file_name <- file.path('figures', 'plot_binomial.png')
  ggsave(filename = file_name,
         plot = g, 
         device = 'png')
  return(file_name)
}
