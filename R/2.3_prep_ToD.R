### Data - Time of Day ###


### Packages ----
libs <- c('data.table', 'dplyr', 'lubridate', 'tidyr')
lapply(libs, require, character.only = TRUE)




### Input data ----
raw <- 'Data/raw/'
derived <- 'Data/derived/'


# Read in Sunset/Sunrise times ####
#in the csv, make sure you have these columns, sometimes the website does weird formatting: Date, `Civil Twilight Start`, Sunrise, Sunset, `Civil Twilight End`
ss2019 <- fread(paste0(raw,"SunriseSunset.csv"), header=TRUE) # read in csv of times
ss2019[,'Year'] <- 2019 # need to add year so it will be the right format to merge later
ss2019[,'dateyear'] <- paste(ss2019$Year, ss2019$Date, sep = '-') # building right format yyyy-mm-dd
day <- ss2019[,.(Date = dateyear, Sunrise = Sunrise, Sunset = Sunset)] #renaming so it's easier
day[,'Date'] <- as.POSIXct(day$Date, tz = 'UTC', "%Y-%m-%d") #formatting (dates are stupid in R)
day[,'SunriseDate'] <- as.POSIXct(paste(day$Date, day$Sunrise, sep = ' '), tz = 'UTC', "%Y-%m-%d %H:%M")
day[,'SunsetDate'] <- as.POSIXct(paste(day$Date, day$Sunset, sep = ' '), tz = 'UTC', "%Y-%m-%d %H:%M")

# Save RDS
#saveRDS(day, '~/snails/Data/derived/sunsetsunrise2019.Rds')

# Read in prepped Sunset/Sunrise times
day.snails <- readRDS('~/snails/Data/derived/sunsetsunrise2019.Rds')

# Read in SSA data
ssa.snails <- readRDS("Data/derived/ssa-hr.Rds")
ssa.snails[,"date.snails"] <- as.POSIXct(format(ssa.snails$t1_, "%Y-%m-%d"), tz = 'UTC', "%Y-%m-%d")
# Merge based on Date
full.snails <- merge(ssa.snails, day.snails, by.x = 'date.snails', by.y = 'Date', all.x = T)

# Specify phase -- day between sunrise and sunset, otherwise night
full.snails[,'ToD_start'] <- ifelse((full.snails$SunriseDate < full.snails$t1_ & full.snails$t1_ < (full.snails$SunsetDate)),
                                  'day', 'night')
#dat <- fread(paste0(raw, 'SnailDataUTM.csv'))
full.snails <- setDT(full.snails)

# dat <- dat[,.(Snail, Date, Time)]
# dat$datetime <- paste(dat$Date, dat$Time)
# making sure my newly created datetime is the right str
#dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 



#merged.snails <-merge(full.snails, dat[,.(datetime)], by.x=c('snail','t2_'), by.y=c('Snail','datetime'), all.x=T)

# get rid of now unneeded sunrise/sunset columns -- yours will probably be something like this in the end
ssa.snails2019 <- setDT(full.snails)[,.(burst_, step_id_, case_, snail,  x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, log_sl, ta_, cos_ta, ToD_start,
                              temp, precip, edgedist_start, edgedist_end, brickedge1_start, brickedge1_end, brickedge2_start, 
                              brickedge2_end, brickedge3_start, brickedge3_end, treatment, stage)]

# 
# saveRDS(ssa.snails2019, '~/snails/Data/derived/ssaAll_snails2019.Rds')
# 
# data <- readRDS('~/snails/Data/derived/ssaAll_snails2019.Rds')

data <- ssa.snails2019 %>% 
         rename(
            Temperature = temp,
            Precipitation = precip,
            Stage = stage,
            Treatment = treatment
             )

treat = "1"
data$Treatment <- ifelse(data$Treatment=="4", "3", data$Treatment)


bricktreatments <- function(data, treat){
  data.sub <- data[Treatment==treat]
  
  brick <- paste("brickedge", treat, sep="")
  
  data.sub[,"brickdist_start"] <- as.data.frame(data.sub)[,paste(brick, "start", sep="_")]
  data.sub[,"brickdist_end"] <- as.data.frame(data.sub)[,paste(brick, "end", sep="_")]
  data.sub[,"ghostbricks"] <- treat
  
  output <- data.sub[,.(burst_, step_id_, case_, snail,  x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, log_sl, ta_, cos_ta, ToD_start,
                        Temperature, Precipitation,edgedist_start, edgedist_end, brickdist_start, brickdist_end, ghostbricks, Treatment, Stage)]
  return(output)}

treat1 <- bricktreatments(data, "1")
treat2 <- bricktreatments(data, "2")
treat3 <- bricktreatments(data, "3")

controltreats <- function(data){
  data.sub <- data[Treatment=="C"]
 
  
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
  
  output <- data.merge[,.(burst_, step_id_, case_, snail,  x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, log_sl, ta_, cos_ta, ToD_start,
                          Temperature, Precipitation,edgedist_start, edgedist_end, brickdist_start, brickdist_end, ghostbricks, Treatment, Stage)]
  return(output)}

control <- controltreats(data)

ssa.30ghosts <- rbind(treat1, treat2, treat3, control)
saveRDS(ssa.30ghosts, 'Data/derived/ssa-hr-ghosts.Rds')
