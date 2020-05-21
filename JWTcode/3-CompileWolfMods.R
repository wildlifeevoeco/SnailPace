

libs <- c('data.table', 'stringr', 'ggplot2')
lapply(libs, require, character.only=TRUE)


numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

### for core model

core_covs_list <- list.files(path="output/models/core/",  recursive=FALSE, include.dirs=TRUE)

core_covs <- data.table()
for (file in core_covs_list){
  temp_dataset <- fread(paste('output/models/core/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  core_covs <- rbind(core_covs, temp_dataset, fill=TRUE)
}
### for pack model

cons_covs_list <- list.files(path="output/models/cons/",  recursive=FALSE, include.dirs=TRUE)

cons_covs <- data.table()
for (file in cons_covs_list){
  temp_dataset <- fread(paste('output/models/cons/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  cons_covs <- rbind(cons_covs, temp_dataset, fill=TRUE)
}

### for prey model

prey_covs_list <- list.files(path="output/models/prey/",  recursive=FALSE, include.dirs=TRUE)

prey_covs <- data.table()
for (file in prey_covs_list){
  temp_dataset <- fread(paste('output/models/prey/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  prey_covs <- rbind(prey_covs, temp_dataset, fill=TRUE)
}

##for human model
human_covs_list <- list.files(path="output/models/human/",  recursive=FALSE, include.dirs=TRUE)

human_covs <- data.table()
for (file in human_covs_list){
  temp_dataset <- fread(paste('output/models/human/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  human_covs <- rbind(human_covs, temp_dataset, fill=TRUE)
}

##for move model

move_covs_list <- list.files(path="output/models/move/",  recursive=FALSE, include.dirs=TRUE)

move_covs <- data.table()
for (file in move_covs_list){
  temp_dataset <- fread(paste('output/models/move/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  move_covs <- rbind(move_covs, temp_dataset, fill=TRUE)
}


### for full model

full_covs_list <- list.files(path="output/models/full/",  recursive=FALSE, include.dirs=TRUE)

full_covs <- data.table()
for (file in full_covs_list){
  temp_dataset <- fread(paste('output/models/full/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  full_covs <- rbind(full_covs, temp_dataset, fill=TRUE)
}


core_covs[,'model':= as.character('core')]
cons_covs[,'model':= as.character('cons')]
prey_covs[,'model':= as.character('prey')]
human_covs[,'model':= as.character('human')]
move_covs[,'model':= as.character('move')]
full_covs[,'model':= as.character('full')]


mods = list(core_covs, cons_covs, prey_covs, human_covs, move_covs, full_covs)
all_covs = rbindlist(mods)

###move model sl and ta distribution parameters
move_distr_par_list <-list.files(path="output/models/distr_params/",  recursive=FALSE, include.dirs=TRUE)

move_distr_par <- data.table()
for (file in move_distr_par_list){
  temp_dataset <- fread(paste('output/models/distr_params/', file, sep=''))
  temp_dataset <- cbind(rep(numextract(file), length=nrow(temp_dataset)), temp_dataset)
  temp_dataset <- cbind(rep(substrRight(file, 2), length=nrow(temp_dataset)), temp_dataset)
  colnames(temp_dataset)[1] <- 'pack'
  colnames(temp_dataset)[2] <- 'wolf'
  move_distr_par <- rbind(move_distr_par, temp_dataset, fill=TRUE)
}

###
saveRDS(core_covs, 'output/models/allwolves/core_mod.rds')
saveRDS(cons_covs, 'output/models/allwolves/cons_mod.rds')
saveRDS(prey_covs, 'output/models/allwolves/prey_mod.rds')
saveRDS(human_covs, 'output/models/allwolves/human_mod.rds')
saveRDS(move_covs, 'output/models/allwolves/move_mod.rds')
saveRDS(full_covs, 'output/models/allwolves/full_mod.rds')
saveRDS(all_covs, 'output/models/allwolves/all_mod.rds')

saveRDS(move_distr_par, 'output/models/allwolves/move_distr_par.rds')


write.csv(core_covs,"output/models/allwolves/core_mod.csv")
write.csv(cons_covs,"output/models/allwolves/cons_mod.csv")
write.csv(prey_covs,"output/models/allwolves/prey_mod.csv")
write.csv(move_covs,"output/models/allwolves/move_mod.csv")
write.csv(full_covs,"output/models/allwolves/full_mod.csv")
write.csv(all_covs,"output/models/allwolves/all_mod.csv")

write.csv(move_distr_par, 'output/models/allwolves/move_distr_par.csv')

