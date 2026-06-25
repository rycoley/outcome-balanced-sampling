rm(list=ls())
gc()
library("bit64")
library("tidyverse")
library("xgboost")
library("ranger")
library("ROCR")
library("doParallel")
library("foreach")
library("data.table")
library("fastDummies")

# Load home dataset 
load("G:/CTRHS/RED_suicide_prevention/PROGRAMMING/Data/No_PHI/RED_sim_data_full.RData")

# Load functions
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/generate_sim_data.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/calibration_functions.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/performance_functions.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/simulation_function_xgb.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/simulation_function_rf.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/tibble_to_XGB.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/fit_xgboost_on_generate_sim_data.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/fit_rf_on_generate_sim_data.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sboot_edits.R")
source("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/format_simulations.R")


### Data prep, recode race as mutually exclusive giving hispanic priority.
original_data = dat_s
dat_s$VISIT_TYPE[original_data$VISIT_TYPE=="MH0"] = 0
dat_s$VISIT_TYPE[original_data$VISIT_TYPE=="PC"] = 1
rm(original_data)
# dat_s$VISIT_TYPE = as.integer(dat_s$VISIT_TYPE)

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
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/XGB/control/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/control/sim_"
boosting_rate <- 0.01  # Boosting rate (0,1]
tree_depth  <- 3      # Boosted tree depth [1,K]
eval_met  <- "auc"    # XGBoost supported evaluation metric (string) 
sub_count <- "all"      # Number of positive cases observed in model 

#############################
#############################
# XGBoost
#############################
#############################

# 1:1 Subsample
rm(total.runtime)
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/XGB/1_1/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/1_1/sim_"
rate_difference <- 1
sub_count <- 200
rounds <- 1292

total.runtime <- system.time({
  for(iteration in 1:sim_replicate){
    red.sim.xgb(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results, rate_difference,
                boosting_rate, tree_depth, eval_met, sub_count, rounds)
  }
  
})

xgb_1_1 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "XGBoost", digits = 5, folder_path = fname.results) 
xgb_1_1_val_cal = marg_val_calib(xgb_1_1)



# 1:2 Subsample
rm(total.runtime)
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/XGB/1_2/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/1_2/sim_"
rate_difference <- 2
sub_count <- 200
rounds <- 1280

total.runtime <- system.time({
  for(iteration in 1:sim_replicate){
    red.sim.xgb(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results, rate_difference,
                boosting_rate, tree_depth, eval_met, sub_count, rounds)
  }
  
})

xgb_1_2 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "XGBoost", digits = 5, folder_path = fname.results) 
xgb_1_2_val_cal = marg_val_calib(xgb_1_2)


# 1:5 Subsample
rm(total.runtime)
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/XGB/1_5/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/1_5/sim_"
rate_difference <- 5
sub_count <- 200
rounds <- 1200

total.runtime <- system.time({
  for(iteration in 1:sim_replicate){
    red.sim.xgb(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results, rate_difference,
                boosting_rate, tree_depth, eval_met, sub_count, rounds)
  }
  
})
sim_replicate = length(list.files("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/1_5/"))
xgb_1_5 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "XGBoost", digits = 5, folder_path = fname.results) 
xgb_1_5_val_cal = marg_val_calib(xgb_1_5)



## Control
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/XGB/control/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/control/sim_"
rate_difference <- NA    # number of non-events relevant to cases (1:5=5; 1:2=2); NA otherwise
rounds  <- 1618 
sub_count <- "all"


total.runtime <- system.time({
  for(iteration in 1:sim_replicate){
    red.sim.xgb(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results, rate_difference,
                boosting_rate, tree_depth, eval_met, sub_count, rounds)
  }
  
})
sim_replicate = length(list.files("G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/control/"))
xgb_control <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "XGBoost", digits = 5, folder_path = fname.results)
xgb_control_val_cal = marg_val_calib(xgb_control)
# 
xgb_1_1_val_cal = marg_val_calib(xgb_1_1)
xgb_1_2_val_cal = marg_val_calib(xgb_1_2)
xgb_1_5_val_cal = marg_val_calib(xgb_1_5)
xgb_control_val_cal = marg_val_calib(xgb_control)
# 
write.table(cbind(xgb_1_1_val_cal, xgb_1_2_val_cal, xgb_1_5_val_cal, xgb_control_val_cal), "clipboard", sep = "\t")


# Tables for manuscript
xgb_results <- rbind(cbind(xgb_1_1$results_table, sample= "1_1", model="xgb"), 
                     cbind(xgb_1_2$results_table, sample= "1_2", model="xgb"), 
cbind(xgb_1_5$results_table, sample= "1_5", model="xgb"), 
cbind(xgb_control$results_table, sample= "1_c", model="xgb"))

write.csv(xgb_results, 
          file = "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/xgb_final_results.csv")

metric <-  names(table(xgb_1_1$results_table$metric))
strata <- names(table(xgb_1_1$results_table$strata))

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

results_xgb <- do.call(bind_rows, lapply(paste0("xgb_", c("1_1", "1_2", "1_5", "control")), function(v) {
  temp = get_res1(v, strata, metric)
  temp$model = "XGB"
  temp$sample = substr(v, 5, 100)
  temp
}))

saveRDS(results_xgb, 
        file = "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/xgb_paper_results.RDS")


## 1:99 Subsample
rm(total.runtime)
fname.boot <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/boot_indices/XGB/1_99/sim_"
fname.results <- "G:/CTRHS/RED_suicide_prevention/ANALYSIS/freddy/outcome_sampling/sim_rep_results/XGB/1_99/sim_"
rounds <- 1560
sub_count <- 200

total.runtime <- system.time({
  for(iteration in 1:sim_replicate){
    red.sim.xgb(sim_replicate=iteration, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results = fname.results, rate_difference,
                boosting_rate, tree_depth, eval_met, sub_count, rounds)
  }
  
})

xgb_1_99 <- format_simulations(max_iter = sim_replicate, rate = rate_difference, model = "XGBoost", digits = 5, folder_path = fname.results) 


