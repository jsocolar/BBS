## Parallelization of the lags list

#### LOAD good_data #########

#### Then, we trim good_data ###

small_data_lag=good_data[,c('statenum.x','Route.x','year','AOU','Abundance','ObsN')]
colnames(small_data_lag)[1:2]=c('statenum','Route')
rm('good_data')


### add routespecs to our dataframe
small_data_lag$routespec <- small_data_lag$AOU+small_data_lag$Route/1000+small_data_lag$statenum/1000000
routespecs <- unique(small_data_lag$routespec)
length(routespecs)


#########################
# We will parallelize the code so that each core will run a state and produce a list that contains the statenum
# and a list of the lags for all species/route combos within the state. For this, we need a function that extracts
# the species-route combo for each state


######################
#Now, we'll break up the data by state and associate num.lags with each state's dataframe
StateNumbers <- unique(small_data_lag$statenum)
StateList <- list()
tt=0
for (i in 1:length(StateNumbers)){
  IndexSet <- which(small_data_lag$statenum==StateNumbers[i])
  dummyFrame <- small_data_lag[IndexSet,]
  # This will be our data frame to put into StateList, once we add the lags
  
  # Now we stitch on the lags
  dumLags=rep(NA,length(dummyFrame$AOU)) # dummy variable for lags
  ind=which(is.element(routespecs,dummyFrame$routespec)) #find which route-specs are in our state dataset.
  for (ll in 1:length(ind)){ #for each unique routespec in our dataset...
    
    dumLags[which(dummyFrame$routespec==routespecs[ind[ll]])]=num.lags[ind[ll]]
    # associate it with its num.lags
  }
  dummyFrame$numlags=dumLags #Now our dummy data frame is complete!
  StateList[[i]]=dummyFrame 
}


#Now we'll stitch together StateList$numlags so that each routespec corresponds to its numlags
StateList[[i]]$numlags=num.lags[(tt+1):(tt+length(IndexSet))]   #We will store numlags with StateList
tt=tt+length(IndexSet)


# Clean up the workspace & Save the StateList
rm('small_data_lag','num.lags','LogicalSet','tt','routespecs')
save('StateList','StateNumbers',file='StateList')


########################### Workhorse Function ############################

getLags <- function(StateData){
  # extract the lags for all species within all routes
  routespecs=unique(StateData$routespec)
  num.lags=StateData$numlags
  lagslist=list()
  for (i in 1:length(routespecs)){  
    # For a given route-species combo
    if(num.lags[i] != 0){           
      # if the species is found more than once
      lagsi <- as.data.frame(matrix(data=NA, nrow=num.lags[i], ncol=9)) 
      # We create a matrix to store the lags
      colnames(lagsi) <- c("state", "route", "year1", "year2", "obs1", "obs2", "species", "count1", "count2")
      p <- which(StateData$routespec==routespecs[i])
      # p is the index set for all rows containing this route-species combo.
      # i.e. it is the number of years this species was found on this route.
      n <- length(p)
      tt <- 0
      for(k in 1:(n-1)){
        
        for(j in (k+1):n){
          tt <- tt+1
          lagsi[tt, ] <- c(StateData$statenum[p[k]], 
                           StateData$Route[p[k]], 
                           StateData$year[p[k]], StateData$year[p[j]], 
                           StateData$ObsN[p[k]], StateData$ObsN[p[j]], 
                           StateData$AOU[p[k]], 
                           StateData$Abundance[p[k]], StateData$Abundance[p[j]])
        }
      }
    }
    else{lagsi <- NA}
    lagslist[[i]]=lagsi #store this data frame in a list
  }
  return(lagslist)
}


####################### Parallelization ###############################

library(parallel)

nnset=list(1:7,8:14,15:21,22:49)
nameList=c('1-7','8-14','15-21','22-49')

for (k in 1:length(nnset)){
  numWorkers=7
  cl=makeCluster(numWorkers,type="PSOCK")
  system.time({
  LagsList=parLapply(cl,StateList[nnset[[k]]],getLags)
  })
  stopCluster(cl)
  save('LagsList',file=paste('Lags.from.StatesList.22-49',namesList[k],sep=''))
}


######################### Merging ########################################

rm(list=ls())
nameList=c('1-7','8-14','15-21','22-49')
nnset=list(1:7,8:14,15:21,22:49)
LagsList_Complete=vector(mode='list',length=49)
for (nn in 1:length(nameList)){
  load(paste('Lags.from.StatesList.',nameList[nn],sep=''))
  tt=0
  for (kk in nnset[[nn]]){
    tt=tt+1
    LagsList_Complete[[kk]]=LagsList[[tt]]
  }
}
  
save('LagsList_Complete',file="Complete_Lag_List")


################ Demo for how to use LagsList_Complete ###################

# Each element of LagsList_Complete corresponds to a state
state49=LagsList_Complete[[49]]

# Each element of the list state49 contains lags for a unique route-spec combo. To find this route, just type
ROUTE=state49[[5]]$route[1]
STATE=state49[[5]]$species[1]

# Or, we can just pull out
lagsi=state49[[5]]
lagsi$year1
lagsi$year2

# etc. !!!