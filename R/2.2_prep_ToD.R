### Data - Time of Day ###


### Packages ----
libs <- c('data.table', 'dplyr', 'lubridate', 'tidyr')
lapply(libs, require, character.only = TRUE)




### Input data ----
raw <- '~/snails/Data/raw/'
derived <- '~/snails/Data/derived/'


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
saveRDS(day, '~/snails/Data/derived/sunsetsunrise2019.Rds')

# Read in prepped Sunset/Sunrise times
day.snails <- readRDS('~/snails/Data/derived/sunsetsunrise2019.Rds')

# Read in SSA data
ssa.snails <- readRDS("~/snails/Data/derived/ssa30.Rds")
ssa.snails[,"date.snails"] <- as.POSIXct(format(ssa.snails$t1_, "%Y-%m-%d"), tz = 'UTC', "%Y-%m-%d")
# Merge based on Date
full.snails <- merge(ssa.snails, day.snails, by.x = 'date.snails', by.y = 'Date', all.x = T)

# Specify phase -- day between sunrise and sunset, otherwise night
full.snails[,'ToD_start'] <- ifelse((full.snails$SunriseDate < full.snails$t1_ & full.snails$t1_ < (full.snails$SunsetDate)),
                                  'day', 'night')
dat <- fread(paste0(raw, 'SnailDataUTM.csv'))
full.snails <- setDT(full.snails)

dat <- dat[,.(Date, Snail, Treatment, Time, Temperature, Precipitation, Stage)]
dat$datetime <- paste(dat$Date, dat$Time)
# making sure my newly created datetime is the right str
dat$datetime <- as.POSIXct(dat$datetime, tz = 'UTC', "%Y-%m-%d %H:%M") 



merged.snails <-merge(full.snails, dat[,.(Snail, datetime, Temperature, Precipitation, Treatment, Stage)], by.x=c('snail','t2_'), by.y= c('Snail', 'datetime'), all.x=T)

# get rid of now unneeded sunrise/sunset columns -- yours will probably be something like this in the end
ssa.snails2019 <- setDT(merged.snails)[,.(burst_, step_id_, case_, snail,  x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, log_sl, ta_, cos_ta, ToD_start,
                              Temperature, Precipitation,edgedist_start, edgedist_end, brickedge1_start, brickedge1_end, brickedge2_start, 
                              brickedge2_end, brickedge3_start, brickedge3_end, Treatment, Stage)]


saveRDS(ssa.snails2019, '~/snails/Data/derived/ssaAll_snails2019.Rds')
