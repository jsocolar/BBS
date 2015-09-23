## Code for analysis of BBS Data

library(RCurl)

##### Part I: Merging data from the 50 states #####
# This script is written in a way that lets us run it from either of our  
# computers without modification. Instead of reading in locally
# stored files (which will inevitably have different filenames on our different
# machines), I read in the files directly from the github website.
# This requires some tinkering because read.csv doesn't know how to interact
# with https.

# IMPORTANT: we break this script if we remove or rename files stored in
# <https://github.com/jsocolar/BBS/tree/master/State-level%20data%20compilation>

states <- c('Alabama', 'Alaska', 'Arizona', 'Arkansa', 'Califor', 'Colorad', 'Connect', 'Delawar', 'Florida', 'Georgia', 'Idaho', 'Illinoi', 'Indiana', 'Iowa', 'Kansas', 'Kentuck', 'Louisia', 'Maine', 'Marylan', 'Massach', 'Michiga', 'Minneso', 'Mississ', 'Missour', 'Montana', 'Nebrask', 'Nevada', 'NHampsh', 'NJersey', 'NMexico', 'NYork', 'NCaroli', 'NDakota', 'Ohio', 'Oklahom', 'Oregon', 'Pennsyl', 'RhodeIs', 'SCaroli', 'SDakota', 'Tenness', 'Texas', 'Utah', 'Vermont', 'Virgini', 'Washing', 'W_Virgi', 'Wiscons', 'Wyoming')
state_data <- list()
for(i in 1:length(states)){
  sURL <- getURL(paste('https://raw.githubusercontent.com/jsocolar/BBS/master/State-level%20data%20compilation/', states[i], '.csv', sep=""))
  state_data[[i]] <- as.data.frame(read.csv(text = sURL))
}

for(i in 1:length(states)){
  colnames(state_data[[i]])[c(5, 6, 12, 13)] <- 
    c('year', 'AOU', 'Frequency', 'Abundance')
  # Note that column 12 is not total abundance, but rather the total
  # number of points on which the species is recorded.  Column 13 is the 
  # abundance.
}

all_states <- state_data[[1]]
for(i in 2:length(states)){
  all_states <- rbind(all_states, state_data[[i]])
}

## Now some data cleaning:
all_clean <- all_states[which(all_states$RPID == 101),]
# This limits us to just the first run in a year, and only standard run protocols
# see <ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/RunProtocolID.txt>

## Now we bring in more route covariates
wURL <- getURL('https://raw.githubusercontent.com/jsocolar/BBS/master/weather.csv')
weather <- as.data.frame(read.csv(text = wURL))
dfr <- as.data.frame(matrix(data=NA, nrow=dim(all_clean)[1], ncol=dim(weather)[2]-5))
cc <- c(1, 7:dim(weather)[2])
colnames(dfr) <- colnames(weather)[cc]
all_data <- cbind(all_clean, dfr)

# ptm <- proc.time()
# for(i in 1:500){
#  all_data[i,14:29] <-
#           weather[which(weather$countrynum==all_data$countrynum[i] & 
#                           weather$statenum==all_data$statenum[i] & 
#                           weather$Route==all_data$Route[i] & 
#                           weather$RPID==all_data$RPID[i] & 
#                           weather$Year==all_data$year[i]), cc]
#}
#proc.time()-ptm
# NOTE, the above code took 318 seconds to complete, meaning that looping to 
# i ==  5000000 would take a month.  Need to find a much more efficient way
# to do this...

rweather <- weather[which(weather$countrynum==840),]
rweather <- rweather[which(rweather$RPID==101),]
# Only one value of these columns exists in all_data, so we don't have to worry
# about these

# Give each combo of statenum, Route, and year a unique combination. Inspiration
# for doing this came from Gödel numbering :)
rweather$uniqueID <- rweather$statenum/100 + rweather$Route + 1000*rweather$Year
all_data$uniqueID <- all_data$statenum/100 + all_data$Route + 1000*all_data$year

all_data <- all_data[ , c(1:13, 30)]

full <- merge(all_data, rweather, by = "uniqueID")
good_data <- full[which(full$RunType==1), ]

##### Part II: Extracting the lags #####
# For each route/species combination, let n be the number of corresponding 
# lines in good_data.  Then there will be n*(n-1)/2 lags associated with that
# route/species combination.

# Give each route/species combo a unique identifier
summary(good_data$Route.x)
summary(good_data$AOU)
summary(good_data$statenum.x)
good_data$routespec <- good_data$AOU + good_data$Route.x/1000 + good_data$statenum.x*1000000
routespecs <- unique(good_data$routespec)
length(routespecs)

num.lags <- 0
ptm <- proc.time()
for(i in 1:length(routespecs)){
  n <- length(which(good_data$routespec==routespecs[i]))
  num.lags <- c(num.lags, n*(n-1)/2)
}
proc.time() - ptm
# Elapsed time on above for-loop was 18194 seconds.