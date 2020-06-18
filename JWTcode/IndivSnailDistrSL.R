snails
gam <- dat[case_==TRUE & sl_ > 0,.(vals = MASS::fitdistr(sl_, "gamma")[[1]],
                                   param = names(MASS::fitdistr(sl_, "gamma")[[1]])), by = .(snail)]
gam.wide <- dcast(gam, snail ~ param, value.var = "vals")

gam.snails <- merge(dat[case_==TRUE,.(snail, sl_)], gam.wide, by = c('snail'), all.x = T)

expo <- dat[case_==TRUE & sl_ > 0,.(vals = MASS::fitdistr(sl_, "exponential")[[1]],
                                    param = names(MASS::fitdistr(sl_, "exponential")[[1]])), by = .(snail)]
expo.wide <- dcast(expo, snail ~ param, value.var = "vals")

expo.snails <- merge(dat[case_==TRUE,.(snail, sl_)], expo.wide, by = c('snail'), all.x = T)

ggplot(gam.snails) +
  geom_histogram(aes(sl_, y = ..density..), bins=50) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  facet_wrap(vars(snail))

ggplot(expo.snails) +
  geom_histogram(aes(sl_, y = ..density..), bins=70) +
  geom_line(aes(x=sl_, y=dexp(sl_, rate[1])), color="blue", size = 1) +
  facet_wrap(vars(snail))

ggplot(gam.snails[snail==snails[1]]) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle(gam.snails$snail)



o11a <-ggplot(gam.snails[snail=='O11a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O11a')

o11b <-ggplot(gam.snails[snail=='O11b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O11b')

o12b <-ggplot(gam.snails[snail=='O12b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O12b')

o13a <-ggplot(gam.snails[snail=='O13a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O13a')

o14a <-ggplot(gam.snails[snail=='O14a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O14a')

o22a <-ggplot(gam.snails[snail=='O22a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=20) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O22a')

o22b <-ggplot(gam.snails[snail=='O22b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O22b')

o23a <-ggplot(gam.snails[snail=='O23a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O23a')

o24a <-ggplot(gam.snails[snail=='O24a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O24a')

o24b <-ggplot(gam.snails[snail=='O24b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O24b')

o31a <-ggplot(gam.snails[snail=='O31a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=10) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('O31a')

p12a <-ggplot(gam.snails[snail=='P12a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P12a')

p13a <-ggplot(gam.snails[snail=='P13a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P13a')

p14a <-ggplot(gam.snails[snail=='P14a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P14a')

p21a <-ggplot(gam.snails[snail=='P21a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P21a')

p22a <-ggplot(gam.snails[snail=='P22a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P22a')

p22b <-ggplot(gam.snails[snail=='P22b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P22b')

p23a <-ggplot(gam.snails[snail=='P23a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P23a')

p23b <-ggplot(gam.snails[snail=='P23b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P23b')

p24a <-ggplot(gam.snails[snail=='P24a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P24a')

p24b <-ggplot(gam.snails[snail=='P24b']) +
  geom_histogram(aes(sl_, y = ..density..), bins=10) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P24b')

p31a <-ggplot(gam.snails[snail=='P31a']) +
  geom_histogram(aes(sl_, y = ..density..), bins=15) +
  geom_line(aes(x=sl_, y=dgamma(sl_, shape[1], rate[1])), color="blue", size = 1) +
  ggtitle('P31a')


(o11a|o11b|o12b|o13a|o14a)/(o22a|o22b|o23a|o24a|o24b)/(o31a|p12a|p13a|p14a|p21a)/
  (p22a|p22b|p23a|p23b|p24a)/(p24b|p31a)