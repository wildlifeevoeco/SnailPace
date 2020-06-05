### RSS ###
# Julie Turner
# started 13 May 2020

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 
          'tidyr', 'ggplot2','survival','forcats', 'patchwork', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)

### Input data ----
# Import your data 


#### Basically we'll be making lists of models within a data.table to automate the way we make calculations
## this can be applied to a lot of things
## so I included some extra functions you may want for other things

### FUNCTIONS ----

# function for model list
list_models <- function(resp, expl, DT) {
  list(list(clogit(reformulate(expl, resp),
                   model = T, data = DT)))
}

# function for predict list for predict method to calculate RSS
list_predict <- function(mod, ND) {
  mapply(function(m, n) predict(m, newdata = n, type = 'lp'),
         m = mod, n = ND)
}

# in case you want to fit the model with the 'amt' function for iSSAs
list_issa <- function(resp, expl, DT) {
  list(list(fit_clogit(reformulate(expl, resp),
                       model = T, data = DT)))
}

# calculate the difference between h1 and h2 for RSS
#' @param ls list column
#' @param val constant value
dif_list <- function(ls, val) {
  list(lapply(ls, function(l) l - val))
}

# get the list of coefficient values from you model
calc_coef <- function(model) {
  list(lapply(model, coef))
}

# get the list of names of coefficient values from you model
calc_coef_names <- function(model) {
  list(lapply(model, function(m) names(coef(m))))
}

# calculate AIC etc for each individual's list of models
calc_aictab <- function(models, names) {
  aictab(models, names)
}

# calculate evidence ratios for top two models for each individual
calc_evidence <- function(aictab) {
  list(lapply(aictab, evidence))
}

# calculate log-likelihood of each model
calc_loglik <- function(model) {
  lapply(model, logLik)
}



### Model ----
# Setup model name, explanatory and response variables
# Cross join individuals and bricks of interest
setup <- CJ(
  snail = unique(dat$snail),
  model = 'p1',
  response = 'case_',
  explanatory = 'log_sl + cos_ta + ToD_start:log_sl + Temperature:log_sl + log(edgedist_end + 1):Stage + log(brickdist_end + 1):Stage + log_sl:Stage + cos_ta:Stage + strata(step_id_)'
)


# Which individuals should be dropped?
# this chould be a list of individuals to include if you want
p1bad <- c("O12b", "O14a", "O22b", "O24b", "O31a", "P21a", "P22b", "P23b", "P24b")
setup[model == 'p1', bad := snail %in% p1bad]


# Run only on *good* individuals
setup[!(bad), mod := 
        list_models(response, explanatory,
                    dat[snail == .BY[[1]]]),
      by = .(snail)]

setup[!(bad), issa := 
        list_issa(response, explanatory,
                  dat[snail == .BY[[1]]]),
      by = .(snail)]

# calc coefs 

setup[!(bad), coef := calc_coef(mod),
      by = .(snail)]


setup[!(bad), var := calc_coef_names(mod),
      by = .(snail)]



# Setup new data
# you need new data for each variable in your model

# if you want RSS based off pop means, calculate them before
meansl <- mean(dat$log_sl, na.rm = T)
meanta <- mean(dat$cos_ta, na.rm = T)
meantemp <- mean(dat$Temperature, na.rm = T)
meanedge <- mean(dat$edgedist_end, na.rm = T)
maxedge <- max(dat$edgedist_end, na.rm = T)
meanbrick <- mean(dat$brickdist_end, na.rm = T)
maxbrick <- max(dat$brickdist_end, na.rm = T)

# h2 new data -- based off mean values
setup[!(bad), h2dat :=
        list(list(dat[snail == .BY[[1]],
                      .(log_sl = meansl,  ## if you want RSS based on indiv values use mean(log_sl, na.rm = T) here and for any other variable
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)), # keep factors consistent
                        Temperature = meantemp,
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = meanedge,
                        brickdist_end = meanbrick,
                        step_id_ = step_id_[1])])),
      by = .(snail)]

# h2 will be the same for each variable of interest, but you will vary 1 variable at a time for h1

# h1 edge new data
setup[!(bad), h1edgedat :=
        list(list(dat[snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = seq(0, maxedge, length.out = 100), # the one habitat variable your vary
                        brickdist_end = meanbrick,
                        step_id_ = step_id_[1])])),
      by = .(snail)]

# h1 brick new data 
setup[!(bad), h1brickdat :=
        list(list(dat[snail == .BY[[1]],
                      .(log_sl = meansl,
                        cos_ta = meanta,
                        ToD_start = factor('day', levels = levels(ToD_start)),
                        Temperature = meantemp,
                        Stage = factor('B', levels = levels(Stage)),
                        edgedist_end = meanedge,
                        brickdist_end = seq(0, maxbrick, length.out = 100),
                        step_id_ = step_id_[1])])),
      by = .(snail)]

# you would do this for however many habitats you want RSS for

# Predict based on new data
# get h2 values
setup[!(bad), h2 := list_predict(mod, h2dat),
      by = .(snail)]

# get h1 values
# edge selection
setup[!(bad), h1edge := list(list(list_predict(mod, h1edgedat))),
      by = .(snail)]
# range of habitat values you're looking at (x for your graph)
setup[!(bad), xedge := list(list(dat[snail == .BY[[1]],
               .(edgedist_end = seq(0, maxedge, length.out = 100))])),
      by = .(snail)]

# brick selection
setup[!(bad), h1brick := list(list(list_predict(mod, h1brickdat))),
      by = .(snail)]
# range of habitat values you're looking at (x for your graph)
setup[!(bad), xbrick := list(list(dat[snail == .BY[[1]],
                                     .(brickdist_end = seq(0, maxbrick, length.out = 100))])),
      by = .(snail)]

# select just the values you need for RSS
rss <- setup[!(bad),.(snail, h1edge, h1brick, h2,
                              xedge, xbrick)]

# now actually calculate the RSS itself (h1-h2)
#edge
rss[, rssedge := dif_list(h1edge, h2),
    by = .(snail)]
rss[, stepedge := list(list(seq(0,max(unlist(xedge)), length.out = 100))),
    by = .( brick)]

# now unlist everything so its a normal data frame
# I like it in long form, so I bind each variable of interest under the last
rss.long <- rss[, .(rss = unlist(rssedge), x = unlist(xedge), step = unlist(stepedge), var = 'edgedist', BA = 'before', model = 'p1'),
                by = .(snail)]
rss.long <- rbind(rss.long, rss[, .(rss = unlist(rssbrick), x = unlist(xbrick), step = unlist(stepbrick), var = 'brickdist', BA = 'before', model = 'p1'),
                                by = .(snail)])

# tada

p1.rss <- copy(rss.long)