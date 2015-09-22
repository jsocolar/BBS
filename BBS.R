## Code for analysis of BBS Data

library(RCurl)

##### Part I: Merging data from the 50 states #####
# This script is written in a way that lets us run it without 
# modification from either of our computers. Instead of reading in locally
# stored files, I read the files directly in from the github website.
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

