### Data - Time of Day ====
# Julie Turner
# Started: July 3 2019




### Packages ----
libs <- c('data.table', 'dplyr', 'lubridate', 'tidyr')
lapply(libs, require, character.only = TRUE)




### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'


# Read in Sunset/Sunrise times RMNP ####
#in the csv, make sure you have these columns, sometimes the website does weird formatting: Date, `Civil Twilight Start`, Sunrise, Sunset, `Civil Twilight End`
ss2016 <- fread(paste0(raw,"sunset_sunrise2016.csv"), header=TRUE) # read in csv of times
ss2016[,'year'] <- 2016 # need to add year so it will be the right format to merge later
day[,'dateyear'] <- paste(day$year, day$Date, sep = '-') # building right format yyyy-mm-dd
day <- day[, .(Date = dateyear, TwiStart = `Civil Twilight Start`, Sunrise = Sunrise, Sunset = Sunset, TwiEnd = `Civil Twilight End`)] #renaming so it's easier
day[,'Date'] <- as.POSIXct(day$Date, tz = 'UTC', "%Y-%m-%d") #formatting (dates are stupid in R)
day[,'TwiStartDate'] <- as.POSIXct(paste(day$Date, day$TwiStart, sep = ' '), tz = 'UTC', "%Y-%m-%d %H:%M") # making datetimes to be linked with those fromm steps
day[,'SunriseDate'] <- as.POSIXct(paste(day$Date, day$Sunrise, sep = ' '), tz = 'UTC', "%Y-%m-%d %H:%M")
day[,'SunsetDate'] <- as.POSIXct(paste(day$Date, day$Sunset, sep = ' '), tz = 'UTC', "%Y-%m-%d %H:%M")
day[,'TwiEndDate'] <- as.POSIXct(paste(day$Date, day$TwiEnd, sep = ' '), tz = 'UTC', "%Y-%m-%d %H:%M")

### you probably won't need this, but you need to be sure that all your times are in the same time zone. I think yours are, mine weren't, so this is me putting everything in GMT. 
# the env canada sunrise sunset times also don't account for daylight savings time, so make sure all your times are in standard newfoundland time
# day[,'gmtTwiStartDate'] <- day$TwiStartDate + hours(6)
# day[,'gmtSunriseDate'] <- day$SunriseDate + hours(6)
# day[,'gmtSunsetDate'] <- day$SunsetDate + hours(6)
# day[,'gmtTwiEndDate'] <- day$TwiEndDate + hours(6)
# day[,'gmtDate'] <-  as.POSIXct(format(day$gmtTwiStartDate, "%Y-%m-%d"), tz = 'UTC', "%Y-%m-%d")


# save data so you don't have to do this everytime
# saveRDS(day, 'data/derived-data/sunsetsunriseRMNP_2016-2017.Rds')


# Read in prepped Sunset/Sunrise times
day.RMNP <- readRDS('data/derived-data/sunsetsunriseRMNP_2016-2017.Rds')

# Read in SSA data
ssa.RMNP <- readRDS("data/derived-data/ssaAll.Rds")

# Merge based on Date
full.RMNP <- merge(ssa.RMNP, day.RMNP, by.x = 't1_', by.y = 'Date', all.x = T)


# Specify phase -- day between sunrise and sunset, otherwise night
full.RMNP[,'ToD_start'] <- ifelse((full.RMNP$SunriseDate < full.RMNP$t1_ & full.RMNP$t1_ < (full.RMNP$SunsetDate)),
                                  'day', 'night')


# get rid of now unneeded sunrise/sunset columns -- yours will probably be something like this in the end
ssa.wolf.RMNP <- full.RMNP[,.(burst_, step_id_, case_, x1_, y1_, x2_, y2_, t1_, t2_, dt_, sl_, log_sl, ta_, cos_ta, ToD_start,
                                temp, precip, distBrick_end, distEdge_end)]


saveRDS(ssa.wolf.RMNP, 'data/derived-data/ssaAllCov_RMNP.Rds')


