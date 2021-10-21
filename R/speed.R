# === Speed ---------------------------------------------------------------



# Plot speed: brick -------------------------------------------------------
# input: speed
plot_speed_brick <- function(DT) {
  DT[, 'brick2'] <- gsub("[^0-9.-]", "", DT$brick)
  DT[, id_treat := paste(id, brick, sep = '_')]
  DT[bd.spd.before < 0, bd.spd.before := 0]
  DT[bd.spd.after < 0, bd.spd.after := 0]
  DT[ed.spd.before < 0, ed.spd.before := 0]
  DT[ed.spd.after < 0, ed.spd.after := 0]
  before <- ggplot(data = DT, aes(x = bdist, y = bd.spd.before, color = brick)) +
    geom_line(aes(group = id_treat, linetype = brick),
              size = 1,
              alpha = .5) +
    geom_smooth(size = 2, se = FALSE, method = 'glm') +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("before ") +
    scale_color_colorblind() +
    xlab("Distance from brick (cm)") + ylab("Speed (cm per hour)")
  
  after <- ggplot(data = DT, aes(x = bdist, y = bd.spd.after, color = brick)) +
    geom_line(aes(group = id_treat, linetype = brick),
              size = 1,
              alpha = .5) +
    geom_smooth(size = 2, se = FALSE, method = 'glm') +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("after ") +
    scale_color_colorblind() +
    xlab("Distance from brick (cm)") + ylab("Speed (cm per hour)")
  
  before + after
}



# Plot speed: edge --------------------------------------------------------
plot_speed_edge <- function(DT) {
  before <- ggplot(data = DT, aes(x = edist, y = ed.spd.before, color = brick)) +
    geom_line(aes(group = id_treat, linetype = brick),
              size = 1,
              alpha = .5) +
    geom_smooth(size = 2, se = FALSE, method = 'glm') +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("before ") +
    ylim(0, 5000) +
    scale_color_colorblind() +
    xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")
  
  after <- ggplot(data = DT, aes(x = edist, y = ed.spd.after, color = brick)) +
    geom_line(aes(group = id_treat, linetype = brick),
              size = 1,
              alpha = .5) +
    geom_smooth(size = 2, se = FALSE, method = 'glm') +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x =  element_text(size = 15)) +
    theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
    ggtitle("after ") +
    ylim(0, 5000) +
    scale_color_colorblind() +
    xlab("Distance from edge (cm)") + ylab("Speed (cm per hour)")
  
  before + after
}