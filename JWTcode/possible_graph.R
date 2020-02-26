# graph code to show betas

# format of data needs to be a column of the covariates (here = term), a column of the values (here = delta), and a column of groupings (in my case COD)

cbPalette = c("#A95AA1", "#85C0F9", "#0F2080")

p.delta <- ggplot(full.indiv, aes(term, (delta), fill = COD)) +
  geom_boxplot(aes(fill = COD),# notch = TRUE, notchwidth = 0.7,
               outlier.color = NA, lwd = 0.6,
               alpha = 0.25) +
  geom_jitter(aes(color = COD),
              position = position_jitterdodge(.35),
              size = 2, alpha = 0.4) +
  #ggtitle('Interaction with community identity') +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme(#legend.position = 'none',
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'black'),
    plot.title=element_text(size = 16, hjust=0),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    strip.text = element_text(size = 14)) +
  xlab('') +
  ylab('delta beta') +
  ggtitle("c) change between control and case") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette)# + ylim(-2,2)