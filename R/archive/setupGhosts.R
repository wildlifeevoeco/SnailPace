require(targets)
require(data.table)
raw <- 'Data/raw/' # folder I store raw data here
derived <- 'Data/derived/' #this is the folder where I'll put my new data after I extract the covariates, as we'll be doing here

tar_load(stepID)

stepID[,uniqueN(brickedge1_end), by = id]

stepID2 <- stepID[case_== TRUE | !is.na(brickedge1_end)]
stepID2[,iter:=1:.N, by = .(id, step_id_)]
stepID.10 <- stepID2[iter <=11]

dat.raw <- fread(paste0(raw, 'SnailDataUTM.csv'))
meta <- dat.raw[,.(id = Snail, treatment = Treatment, stage = Stage, 
                   temp = Temperature, precip = Precipitation, 
                   t1_ = as.POSIXct(paste(Date, paste(Time, "00", sep=":"), sep = ' '), 
                                    tz = 'UTC', "%Y-%m-%d %H:%M:%S"))]

steps <- merge(stepID.10, meta, by = c('id', 't1_'))



steps$treatment <- ifelse(steps$treatment=="4", "3", steps$treatment)


bricktreatments <- function(data, treat){
  data.sub <- data[treatment==treat]
  
  brick <- paste("brickedge", treat, sep="")
  
  data.sub[,"brickdist_start"] <- as.data.frame(data.sub)[,paste(brick, "start", sep="_")]
  data.sub[,"brickdist_end"] <- as.data.frame(data.sub)[,paste(brick, "end", sep="_")]
  data.sub[,"ghostbricks"] <- treat
  
  output <- data.sub[,.(burst_, step_id_, case_, id,  x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, ta_, tod_start_,
                        edgedist_start, edgedist_end, brickdist_start, brickdist_end, ghostbricks, treatment, stage,
                        temp, precip, indiv_step_id)]
  return(output)
}

treat1 <- bricktreatments(steps, "1")
treat2 <- bricktreatments(steps, "2")
treat3 <- bricktreatments(steps, "3")

controltreats <- function(data){
  data.sub <- data[treatment=="C"]
  
  
  data.c <- data.sub
  data.c[,"brickdist_start"] <- NA
  data.c[,"brickdist_end"] <- NA
  data.c[,"ghostbricks"] <- "C"
  
  data.g1 <- data.sub
  brick.g1 <-"brickedge1"
  data.g1[,"brickdist_start"] <- as.data.frame(data.sub)[,paste(brick.g1, "start", sep="_")]
  data.g1[,"brickdist_end"] <- as.data.frame(data.sub)[,paste(brick.g1, "end", sep="_")]
  data.g1[,"ghostbricks"] <- "g1"
  
  data.g2 <- data.sub
  brick.g2 <-"brickedge2"
  data.g2[,"brickdist_start"] <- as.data.frame(data.sub)[,paste(brick.g2, "start", sep="_")]
  data.g2[,"brickdist_end"] <- as.data.frame(data.sub)[,paste(brick.g2, "end", sep="_")]
  data.g2[,"ghostbricks"] <- "g2"
  
  data.g3 <- data.sub
  brick.g3 <-"brickedge3"
  data.g3[,"brickdist_start"] <- as.data.frame(data.sub)[,paste(brick.g3, "start", sep="_")]
  data.g3[,"brickdist_end"] <- as.data.frame(data.sub)[,paste(brick.g3, "end", sep="_")]
  data.g3[,"ghostbricks"] <- "g3"
  
  data.merge <- rbind(data.c, data.g1, data.g2, data.g3)
  
  output <- data.merge[,.(burst_, step_id_, case_, id,  x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, ta_, tod_start_,
                          edgedist_start, edgedist_end, brickdist_start, brickdist_end, ghostbricks, treatment, stage,
                          temp, precip, indiv_step_id)]
  return(output)}

control <- controltreats(steps)

steps.ghosts <- rbind(treat1, treat2, treat3, control)
saveRDS(steps.ghosts, paste0(derived,'snailRandSteps.RDS'))


