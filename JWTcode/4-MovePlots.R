
libs <- c('data.table', 'ggplot2','grid','gridExtra','lattice', 'patchwork')
lapply(libs, require, character.only=TRUE)


###

relpath = "output/models/allwolves/"

###
move_pars_dat = readRDS(paste(relpath,"move_distr_par.rds",sep=""))

move_covs_dat = readRDS(paste(relpath,"move_mod.rds",sep=""))


###for each wolf get the estimates you want and then go through models
# Make a vector of all unique wolf IDs 
###05 and 21 removed - check them out
wolf_IDs <- c('01', '02','03','O4','06','07','08','09','10','11','12','13','14','16','19','22','24','25','26', '27')

###define aggregate data tables
move_calcs_all = NULL


###LOOP OVER ALL WOLF IDs

for(wolfid in wolf_IDs){

move_covs_dat01 = move_covs_dat[wolf %in% wolfid]
move_pars_dat01 = move_pars_dat[wolf %in% wolfid]

tfkill = seq(0,96)

shape = move_pars_dat01$shape
scale = move_pars_dat01$scale
kappa = move_pars_dat01$kappa


covi = move_covs_dat01[term == "log_sl_"]
Bi = covi$estimate
covij = move_covs_dat01[term == "log_sl_:tfkill"]
Bij = covij$estimate

covk = move_covs_dat01[term == "cos_ta_"]
Bk = covk$estimate
covkj = move_covs_dat01[term == "cos_ta_:tfkill"]
Bkj = covkj$estimate

speed = (shape+Bi+(Bij*tfkill))*(scale)

direction = (kappa+Bk+(Bkj*tfkill))

move_calcs01 = data.table(wolfid, Bi,Bij, tfkill, speed, Bk, Bkj, direction) 
move_calcs_all = rbind(move_calcs_all,move_calcs01)

}

mean(move_pars_dat$shape)
mean(move_pars_dat$scale)
mean(move_pars_dat$shape*move_pars_dat$scale)
mean(move_pars_dat$kappa)

speedplot <- ggplot(data=move_calcs_all, aes(x=tfkill, y=speed)) + 
  geom_line(aes(group=wolfid), size=1, alpha=.5) +
  geom_hline(yintercept=790.9842, linetype='dashed', size = 1) +
  geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=23)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 23)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("a. Move - Speed ") +
  xlab("Time from kill (hours)") + ylab("Speed (meters per hour)")
speedplot 

turnplot <- ggplot(data=move_calcs_all, aes(x=tfkill, y=direction)) + 
  geom_line(aes(group=wolfid), size=1, alpha=.5) +
  geom_hline(yintercept=0.08410254, linetype='dashed', size = 1) +
  geom_smooth(size = 2, se = FALSE)+
  theme_classic() +
  theme(text = element_text(size=23)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =  element_text(size = 23)) + 
  #  theme(legend.position = "none") +
  theme(plot.margin = margin(0.1, 1, .1, .1, "cm")) +
  ggtitle("b. Move - Directionality ") +
  xlab("Time from kill (hours)") + ylab("Concentration of turn angle")
turnplot


png('output/plots/moveparplots', width = 16000, height = 7500, res=1000, units="px")

speedplot+turnplot

dev.off()




