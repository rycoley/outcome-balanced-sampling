

####################### functions to source() ########################


###### sboot() ########## USE FOR FULL SAMPLE/1:99 MODEL ONLY
#this function performs a stratified bootstrap, saves information that can be used to tell ranger the bootstrap samples each tree should be built on

#ouput
#this function returns a vector, indi, that is the same length as the number of rows in your data
#each entry in indi indicates how many times a record (row in the dataset) should appear in the bootstrap sample.

#inputs
#data is whatever dataframe you are taking the bootstrap sample from, going to estimate RF with
#outcome is the outcome you want to stratify sampling on

sboot <- function(data, outcome){
  indi <- rep(0,nrow(data))
  ind1 <- which(data[,outcome]==1)
  ind0 <- which(data[,outcome]==0)
  booti <- data.table(ind=c(sample(ind1, replace=TRUE), sample(ind0, replace=TRUE)))
  booti <- booti[, .(freq=.N), by=ind] # make a frequency summary table
  indi[booti$ind] <- booti$freq
  return(indi) ## frequency of each observation to be chosen
}
#use this with the argument inbag in the ranger function


###### sbootprop() ########## USE FOR BALANCED SAMPLING (1:1, 1:2, 1:5 ONLY)
#updates to sboot() by Ziyi
#ratio is added to funciton inputs, which can help change the sample size ratio of minority and majority groups in the bootstrap
#ratio is the sample size of majority group over minority group (e.g. majority: minority=3:1, then ratio=3)
##########WE WILL USE ratio=1 FOR 1:1, ratio=2 FOR 1:2, ratio=5 FOR 1:5 ##########
#minsize (sample size for outcome=1), majsize(sample size for outcome=0)

sbootprop <- function(data, outcome, ratio){
  indi <- rep(0,nrow(data))
  ind1 <- which(data[,outcome]==1)
  ind0 <- which(data[,outcome]==0)
  minsize <-length(ind1)
  majsize <-ratio*minsize
  #print(minsize)
  #print(majsize)
  booti <- data.table(ind=c(sample(ind1, minsize,replace=TRUE), sample(ind0, majsize, replace=TRUE)))
  booti <- booti[, .(freq=.N), by=ind] # make a frequency summary table
  indi[booti$ind] <- booti$freq
  return(indi) ## frequency of each observation to be chosen
}