# TODO: make this consistent with other tidy_model?
clean_model_names <- function(model) {
  # making names easier to deal with
  model$term <- gsub(':', '-', model$term)
  model$term <- gsub(' ', '', model$term)
  model$term <- gsub('[[:punct:]]', '', model$term)
  model$term <- gsub('stageA', 'after', model$term)
  model$term <- gsub('stageB', 'before', model$term)
  model$term <- gsub('Ilogsl1', 'logsl_', model$term)
  model$term <- gsub('Ilogbrickdiststart1', 'brickdist_', model$term)
  model$term <- gsub('Ilogedgediststart1', 'edgedist_', model$term)
  
  model
}

tidy_coefs <- function(model, distparams) {
  if (is.null(distparams)) return(NULL)
  
  coefs <- dcast(model, level ~ term, value.var = 'estimate')
  coefs <- coefs %>% tidyr::separate((level), c('id', 'brick'), sep = '_')
  coefs <- merge(coefs, distparams, by = 'id')
}

make_predict_seq <- function(combtreats, model) {
  maxedge <- max(combtreats$edgedist_end, na.rm = T)
  maxbrick <- 65
  
  list(
    bdist = seq(0, maxbrick, length.out = 100),
    edist = seq(0, maxedge, length.out = 100),
    logsltemp = model$estimate[[2]],
    meanedge = mean(combtreats$edgedist_end, na.rm = TRUE),
    maxedge = maxedge,
    meanbrick = mean(combtreats$brickdist_end, na.rm = TRUE),
    maxbrick = maxbrick,
    meantemp = mean(combtreats$temp, na.rm = TRUE)
  )
}

predict_speed <- function(coefs, seqs) {
  if (is.null(coefs)) return(NULL)
  
  bdist <- seqs$bdist
  edist <- seqs$edist
  logsltemp <- seqs$logsltemp
  meanedge <- seqs$meanedge
  maxedge <- seqs$maxedge
  meanbrick <- seqs$meanbrick
  maxbrick <- seqs$maxbrick
  meantemp <- seqs$meantemp
  
  pred_length <- 100
  repcoef <- coefs[rep(.N, pred_length)]
  
  repcoef[, bd.spd.before := 
          (1 + logsl_ + logsl_before + (logsltemp * meantemp) + 
             (brickdist_logsl_before * bdist) + 
             (edgedist_logsl_before * meanedge)) * (1 / rate),
        by = .(id, brick)]
  repcoef[, bd.spd.after := 
          (1 + logsl_ + logsl_after + (logsltemp * meantemp) + 
             (brickdist_logsl_after * bdist) + 
             (edgedist_logsl_before * meanedge)) * (1 / rate), 
        by = .(id, brick)]
  repcoef[, bdist := seq(0, maxbrick, length.out = 100), 
        by = .(id, brick)]
  repcoef[, ed.spd.before := 
          (1 + logsl_ + logsl_before + (logsltemp * meantemp) + 
             (edgedist_logsl_before * edist) + 
             (brickdist_logsl_after * meanbrick)) * (1 / rate), 
        by = .(id, brick)]
  repcoef[, ed.spd.after := 
          (1 + logsl_ + logsl_after + (logsltemp * meantemp) + 
             (edgedist_logsl_after * edist) + 
             (brickdist_logsl_after * meanbrick)) * (1 / rate),
        by = .(id, brick)]
  repcoef[, edist := seq(0, maxedge, length.out = 100), 
        by = .(id, brick)]
  
  repcoef[, disturbance := ifelse(brick %like% 'g', 'undisturbed', 'disturbed'), 
        by = .(id, brick)]
  
  repcoef
  
}




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

