##April 26, 2023
##Shared function for balanced sampling project
##Script will generate training set, testing set, and validation set

#inputs
#sim_replication is the simulation iteration for i in 1:500 (for 500 replications). 
#we use this to set the seed so that datasets generated for all simulations will be the same
#n_train, n_test, n_val are the number of visits for the training, testing, and validation sets, respectively
#event_rate is the event rate for the simulation
#n_val_rep is the number of validation sets we will sample (and average across)


#outputs
#dat_train and dat_test, training and testing sets
#dat_val: all remaining visits in the home dataset (not in the training or testing set), with indicator variables for whether each visit should be included in 1:n_val_rep validation sets
# e.g., colnames val1, val2, ...val10 for n_val_rep=10
# 
# generate_sim_data <- function(sim_replication, n_train=100000, n_test=100000, n_val=1000000, event_rate=0.01, n_val_rep=1){
#   #set starting seed based on simulation iteration
#   set.seed(sim_replication)
# 
#   #load home dataset
#   # load("G:/CTRHS/RED_suicide_prevention/PROGRAMMING/Data/No_PHI/RED_sim_data_full.RData")
#   
#   #remove columns that we don't use for model estimation or validation
#   dat_s <- dat_s[,!names(dat_s)%in%c("PERSON_ID","VISIT_SEQ", "DAYS_SINCE_PREV")]
#   
#   #stratify data on event status
#   dat1 <- dat_s[dat_s$EVENT90==1,]  ##visits with an event
#   dat0 <- dat_s[dat_s$EVENT90==0,]  ##visits without an event
#   rm(dat_s)
#   
#   #randomly re-arrange visits, to sample training, testing, validation
#   dat1 <- dat1[sample(x=1:nrow(dat1), size=nrow(dat1), replace=F),]
#   dat0 <- dat0[sample(x=1:nrow(dat0), size=nrow(dat0), replace=F),]
#   
#   dat_train <- rbind(dat1[1:(n_train*event_rate),], dat0[1:(n_train*(1-event_rate)),])
#   dat_test <- rbind(dat1[(n_train*event_rate+1):(n_train*event_rate+n_test*event_rate),], 
#                      dat0[(n_train*(1-event_rate)+1):(n_train*(1-event_rate)+n_test*(1-event_rate)),])
# 
#   #for validation set, we will take all remaining visits and sample 1 million visits n_val_rep times, for n_val_rep validation sets
#   dat1_val <- dat1[(n_train*event_rate+n_test*event_rate+1):nrow(dat1),]
#   dat0_val <- dat0[(n_train*(1-event_rate)+n_test*(1-event_rate)+1):nrow(dat0),]
#   rm(dat0,dat1)
#   
#   ncol_dat1_val = ncol(dat1_val)
#   ncol_dat0_val = ncol(dat0_val)
#   nrow_dat1_val = nrow(dat1_val)
#   nrow_dat0_val = nrow(dat0_val)
#   
#   for(rep in 1:n_val_rep){
#     dat1_val[, ncol_dat1_val+1] = 0
#     dat1_val[sample(x=1:nrow_dat1_val, size=n_val*event_rate, replace=F ), ncol_dat1_val+1] = 1
# 
#     dat0_val[, ncol_dat0_val+1] = 0
#     dat0_val[sample(x=1:nrow_dat0_val, size=n_val*(1-event_rate), replace=F ), ncol_dat0_val+1] = 1}
#   
#   dat_val <- rbind(dat1_val, dat0_val)
#   names(dat_val)[tail(1:ncol(dat_val), n_val_rep)] <- paste0("val", c(1:n_val_rep))
#   rm(val1_s, val0_s, dat1_val, dat0_val, rep)
#   
#   return(list(dat_train=dat_train, dat_test=dat_test, dat_val=dat_val))
# }


generate_sim_data <- function(sim_replication, n_train=100000, n_test=100000, n_val=1000000, event_rate=0.01, n_val_rep=10){
  #set starting seed based on simulation iteration
  set.seed(sim_replication)
  
  #load home dataset
  # load("G:/CTRHS/RED_suicide_prevention/PROGRAMMING/Data/No_PHI/RED_sim_data_full.RData")
  
  #remove columns that we don't use for model estimation or validation
  dat_s <- dat_s[,!names(dat_s)%in%c("PERSON_ID","VISIT_SEQ", "DAYS_SINCE_PREV")]
  
  # Create categorical race variable for later use in stratified analysis
  # Assumes incoming dummies are mutually exclusive.
  racevars = c("raceAsian","raceBlack","raceHP","raceIN","raceMUOT",
               "raceUN","hispanic")
  
  # Initialize race categorical variable
  race_cat = as.integer(rep(NA, nrow(dat_s)))
  
  # Code White based on all others
  race_cat[apply(dat_s[, racevars], 1, sum) == 0] <- 0
  
  # Cope non-white categories 
  sapply(1:length(racevars), function(i) {
    race_cat[dat_s[, racevars[i]]==1] <<- i
  })
  
  # Add to main dataset so that race_cat is included in test, train and valid splits
  dat_s$race_cat <- race_cat
  
  #browser()
  
  #stratify data on event status
  dat1 <- dat_s[dat_s$EVENT90==1,]  ##visits with an event
  dat0 <- dat_s[dat_s$EVENT90==0,]  ##visits without an event
  rm(dat_s)
  
  #randomly re-arrange visits, to sample training, testing, validation
  dat1 <- dat1[sample(x=1:dim(dat1)[1], size=dim(dat1)[1], replace=F),]
  dat0 <- dat0[sample(x=1:dim(dat0)[1], size=dim(dat0)[1], replace=F),]
  
  dat_train <- rbind(dat1[1:(n_train*event_rate),], dat0[1:(n_train*(1-event_rate)),])
  dat_test <- rbind(dat1[(n_train*event_rate+1):(n_train*event_rate+n_test*event_rate),],
                    dat0[(n_train*(1-event_rate)+1):(n_train*(1-event_rate)+n_test*(1-event_rate)),])
  
  #for validation set, we will take all remaining visits and sample 1 million visits n_val_rep times, for n_val_rep validation sets
  dat1_val <- dat1[(n_train*event_rate+n_test*event_rate+1):dim(dat1)[1],]
  dat0_val <- dat0[(n_train*(1-event_rate)+n_test*(1-event_rate)+1):dim(dat0)[1],]
  rm(dat0,dat1)
  
  for(rep in 1:n_val_rep){
    val1_s <- sample(x=1:dim(dat1_val)[1], size=n_val*event_rate, replace=F )
    dat1_val[val1_s, dim(dat1_val)[2]+1] <- 1
    dat1_val[is.na(dat1_val[,dim(dat1_val)[2]]),dim(dat1_val)[2]] <- 0
    
    val0_s <- sample(x=1:dim(dat0_val)[1], size=n_val*(1-event_rate), replace=F )
    dat0_val[val0_s, dim(dat0_val)[2]+1] <- 1
    dat0_val[is.na(dat0_val[,dim(dat0_val)[2]]),dim(dat0_val)[2]] <- 0
    }

  dat_val <- rbind(dat1_val, dat0_val)
  names(dat_val)[(dim(dat_val)[2]-(n_val_rep-1)):dim(dat_val)[2]] <- paste0("val", c(1:n_val_rep))
  rm(val1_s, val0_s, dat1_val, dat0_val, rep)
  
  race_cat_train = dat_train$race_cat
  race_cat_test = dat_test$race_cat
  race_cat_val = dat_val$race_cat
  
  dat_train$race_cat <- NULL
  dat_test$race_cat <- NULL
  dat_val$race_cat <- NULL
  
  return(list(dat_train=dat_train, dat_test=dat_test, dat_val=dat_val, 
              race_cat_train=race_cat_train, race_cat_test=race_cat_test,
              race_cat_val=race_cat_val))
  
}

