##### Formatting all the states to be bound to previous fifty_r.csv files
##### And then binding them!!


setwd("Princeton/Socolar - Washburne/BBS and Land Use Changes/Data/BBS Data and Documents/Pre 1997/States")
States=list.files()
N=length(States)

X=read.csv(States[1])
colnames(X)[c(5,6,12)]=c('year','AOU','Abundance')
D=X[,c(1:6,12)]


for (i in 2:N){
Y=read.csv(States[i])
colnames(Y)[c(5,6,12)]=c('year','AOU','Abundance')
D=rbind(D,Y[,c(1:6,12)])
}