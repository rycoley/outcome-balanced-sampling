rm(list=ls())
gc()
library("torch")
library("bit64")
library("tidyverse")
# library("xgboost")
# library("ranger")
library("ROCR")
library("doParallel")
library("foreach")
library("data.table")
library("fastDummies")
library("tidyverse", lib.loc="C:/Users/h778841/Anaconda3/envs/r_env/Lib/R/library")
library("janitor")
# Load home dataset 
load("G:/CTRHS/RED_suicide_prevention/PROGRAMMING/Data/No_PHI/RED_sim_data_full.RData")

# Load functions
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/generate_sim_data.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/calibration_functions.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/performance_functions.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/simulation_function_nn.R")
# source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/simulation_function_rf.R")
# source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/tibble_to_XGB.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/fit_nn_on_generate_sim_data.R")
# source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/fit_rf_on_generate_sim_data.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sboot_edits.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/format_simulations.R")


### Data prep, recode race as mutually exclusive giving hispanic priority.
dat_s$VISIT_TYPE[dat_s$VISIT_TYPE=="MH0"] = 0
dat_s$VISIT_TYPE[dat_s$VISIT_TYPE=="PC"] = 1
dat_s$VISIT_TYPE = as.integer(dat_s$VISIT_TYPE)

# Create categorical variable for performance in racial/ethnic subgroups
racevars = c("raceAsian","raceBlack","raceHP","raceIN","raceMUOT",
             "raceUN","hispanic")
race_cat = as.integer(rep(NA, nrow(dat_s)))
race_cat[apply(dat_s[, racevars], 1, sum) == 0] <- 0

# Recode race categories
sapply(1:length(racevars), function(i) {
  race_cat[dat_s[, racevars[i]]==1] <<- i
})

# Generate dummies, remove old race variables and add to new dummies to home data set
race_cat_dummy <- fastDummies::dummy_cols(race_cat, remove_most_frequent_dummy = TRUE,
                                          remove_selected_columns = TRUE)
colnames(race_cat_dummy) <- racevars

lapply(racevars, function(x) {
  dat_s[,x] <<- NULL 
})

dat_s <- cbind(dat_s, race_cat_dummy)

rm(list=c("race_cat", "race_cat_dummy"))
gc()

### End data prep.

### Set simulation parameters
n_val_rep = 500          # Number of validation set replicates
event_rate = 0.01       # overall event rate
n_sim_train = 100000    # training set sample size
n_sim_test = 100000    # test set sample size
n_sim_val = 1000000     # validation set sample size
qtile_probs_test = c(0.75, 0.90, 0.95, 0.99) # Which probabilities to get testing quantiles
num_cols_valid <- ncol(dat_s) - 3 + n_val_rep # Number of columns in the validation dataset (minus PERSON_ID, VISIT_SEQ and EVENT90)
nboot = 1000 # Number of bootstrap iterations to perform
nodes <- detectCores()-1 # Number of nodes to use for parallel bootstrap
sim_replicate <- 500
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/NN/control/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/control/sim_"
n_epochs <- 49  # Number of passes through the entire data
batch_size  <- 1000     # Batch size for unbalanced sampling
mini_n_controls  <- 1000     # Mini-batch size controls 
mini_n_cases <- 1000      # Mini-batch size cases 

### End simulation parameters.

#############################
#############################
# Neural Nets
#############################
#############################

## Neural


# 1:1 Subsample 
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/NN/1_1/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/1_1/sim_"
n_epochs <- 2  # Boosting rate (0,1]
batch_size  <- NULL 
mini_n_controls  <- 1000   
mini_n_cases <- 1000    
total.runtime <- system.time({
  for(iteration in 1:sim_replicate){
    red.sim.nn(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                   qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results,
                   n_epochs, batch_size, w_decay = 0.0001, mini_n_controls, mini_n_cases)
  }
  
})

rate_difference = 1
nn_1_1 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "NN", digits = 5, folder_path = fname.results) 



# 1:2 Subsample 
rm(total.runtime)
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/NN/1_2/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/1_2/sim_"
n_epochs <- 3 
batch_size  <- NULL 
mini_n_controls  <- 1000   
mini_n_cases <- 500   

total.runtime <- system.time({
  for(iteration in 372:sim_replicate){
    red.sim.nn(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
               qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results,
               n_epochs, batch_size, w_decay = 0.0001, mini_n_controls, mini_n_cases)
  }
  
})
rate_difference = 2
nn_1_2 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "NN", digits = 5, folder_path = fname.results) 

# 1:5 Subsample
rm(total.runtime)
# fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/NN/1_5/sim_"
# fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/1_5/sim_"

fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/rob/boot_indices/NN/1_5/sim_"
fname.results <-"G:/CTRHS/RED_suicide_prevention/ANALYSIS/rob/sim_rep_results/NN/1_5/sim_"
n_epochs <- 6  
batch_size  <- NULL 
mini_n_controls  <- 1000   
mini_n_cases <- 200 
nodes <- 32 # Number of nodes to use for parallel bootstrap


total.runtime <- system.time({
  for(iteration in 416:sim_replicate){
    red.sim.nn(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
               qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results,
               n_epochs, batch_size, w_decay = 0.0001, mini_n_controls, mini_n_cases)
    gc()
  }
  
})
rate_difference = 5
nn_1_5 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "NN", digits = 5, folder_path = fname.results) 

# Full Sample
rm(total.runtime)
# fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/NN/control/sim_"
# fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/control/sim_"

fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/rob/boot_indices/NN/control/sim_"
fname.results <-"G:/CTRHS/RED_suicide_prevention/ANALYSIS/rob/sim_rep_results/NN/control/sim_"
n_epochs <- 49  
batch_size  <- 1000 
mini_n_controls  <- NULL   
mini_n_cases <- NULL 
nodes <- 23 # Number of nodes to use for parallel bootstrap

total.runtime <- system.time({
  for(iteration in 4:sim_replicate){
    red.sim.nn(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
               qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results,
               n_epochs, batch_size, w_decay = 0.0001, mini_n_controls, mini_n_cases)
    gc()
  }
  
})
rate_difference <- NA
nn_control <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "NN", digits = 5, folder_path = fname.results)


nn_1_1_val_cal = marg_val_calib(nn_1_1)
nn_1_2_val_cal = marg_val_calib(nn_1_2)
nn_1_5_val_cal = marg_val_calib(nn_1_5)
nn_control_val_cal = marg_val_calib(nn_control)

write.table(cbind(nn_1_1_val_cal, nn_1_2_val_cal, nn_1_5_val_cal, nn_control_val_cal), "clipboard", sep = "\t")

nn_results <- rbind(cbind(nn_1_1$results_table, sample= "1_1", model="nn"), 
                    cbind(nn_1_2$results_table, sample= "1_2", model="nn"), 
                    cbind(nn_1_5$results_table, sample= "1_5", model="nn"), 
                    cbind(nn_control$results_table, sample= "1_c", model="nn"))

write.csv(nn_results, 
          file = "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/nn_final_results.csv")

# Manuscript tables

get_res1 <- function(datlist, strata, metric) {
  do.call(bind_rows, lapply(strata, function(s) {
    do.call(bind_rows, lapply(metric, function(m){
      if(m%in%c("AUC", "AUCPR", "Brier")) {
        subset(get(datlist)[["results_table"]], strata==s & metric==m & pctile=="75%")
      } else {
        subset(get(datlist)[["results_table"]], strata==s & metric==m)[4:1,]
      }
    }))
  }))
}

metric <-  names(table(nn_1_1$results_table$metric))
strata <- names(table(nn_1_1$results_table$strata))

results_nn<- do.call(bind_rows, lapply(paste0("nn_", c("1_1", "1_2", "1_5", "control")), function(v) {
  temp = get_res1(v, strata, metric)
  temp$model = "NN"
  temp$sample = substr(v, 4, 100)
  temp
}))

saveRDS(results_nn, 
        file = "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/nn_paper_results.RDS")
results_nn = readRDS("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/NN/nn_paper_results.RDS")
results_rf = readRDS("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/RF/rf_paper_results.RDS")
results_xgb = readRDS("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/xgb_paper_results.RDS")

write.table(results_xgb, "clipboard-64000", sep="\t")
write.table(results_nn, "clipboard-64000", sep="\t")
write.table(results_rf, "clipboard-64000", sep="\t")
