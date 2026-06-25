### RED Suicide XGB Simulation Function
### Generates simulated data, fits model to training data,
### calibrates predictions on testing set, recalibrates on test and validation datasets,
### summarizes calibration, estimates overall and by strata performance in test
### data, compares performance estimates to "truth" estimates in validation set.
### Arguments:
###   dat_s       Analytic data
###   n_val_rep   Number of validation set replicates
###   event_rate  Overall event rate
###   n_sim_train Training set sample size
###   n_sim_test  Test set sample size
###   n_sim_val   Validation set sample size
###   qtile_probs_test = c(0.75, 0.90, 0.95, 0.99); Which probabilities to get training quantiles for
###   num_cols_valid = ncol(dat_s) - 3 + n_val_rep; Number of columns in the validation dataset (minus PERSON_ID, VISIT_SEQ and EVENT90)
###   nboot = 1000;             Number of bootstrap iterations to perform
###   nodes <- detectCores()-1; Number of nodes to use for parallel bootstrap
###   sim_replicate             Simulation counter and seed, should be incremented after each run.
###   fname.boot                Character, path and first part of file name, sim rep and .Rdata
###                             are appended internally.
###   fname.results             Character, path and first of file name, sim rep, and .Rdata of prediction results
###   Model Params:
###    rate_difference Number of non-events relative to cases (1:1 = 1; 1:2 = 2; 1:5=5)
###    boosting_rate   Boosting rate (0,1]
###    tree_depth      Boosted tree depth [1,K]
###    eval_met        XGBoost supported evaluation metric (string) 
###    sub_count       Number of positive cases observed in model 
###                    subsampling [1,K]. If using all possible cases let 
###                    sub_count = "all"
###    rounds          Number of boosting rounds [1,K]


red.sim.xgb <- function(sim_replicate, dat_s, n_val_rep, event_rate, n_sim_train, n_sim_test, n_sim_val, 
                    qtile_probs_test, num_cols_valid, nboot, nodes, fname.boot, fname.results,
                    rate_difference, boosting_rate, tree_depth, eval_met, sub_count, rounds) {
  ### Generate data
  datgen.runtime <- system.time({
    sim_data = generate_sim_data(sim_replication=sim_replicate, n_train=n_sim_train, n_test=n_sim_test, 
                                  n_val=n_sim_val, event_rate=event_rate, 
                                  n_val_rep=n_val_rep)
  })
  
  # Categorical (numeric) strata variable and labels for unique values
  # Coding begins at zero and labels should be in ascending numeric order
  strata_var_test <- sim_data$race_cat_test
  strata_var_train <- sim_data$race_cat_train
  
  strata.labels <- c("raceWhite", "raceAsian","raceBlack",
                     "raceHP","raceIN","raceMUOT","raceUN",
                     "hispanic")
  
  # Categorical (numeric) strata variable and labels for unique values
  # Coding begins at zero and labels should be in ascending numeric order
  # Assumes labels are the same as above
  strata_var_valid <- sim_data$race_cat_val
  
  # Flags indicating validation replicates
  validation.set.indicators <- sim_data$dat_val[, 
                                                (ncol(sim_data$dat_val)-(n_val_rep - 1)):(num_cols_valid)]
  
  ### Fit model and get predictions
  modelfit.time <- system.time({
  xgboost_predictions <- fit_xgboost(sim_data$dat_train, sim_data$dat_test, sim_data$dat_val, 
                                     sim_data$race_cat_test, sim_data$race_cat_train, sim_data$race_cat_val,
                                     "EVENT90", rate_difference,
                                     boosting_rate, tree_depth, eval_met, sub_count, rounds)
})
  # predictions on the training set
  preds_train_sample <- xgboost_predictions$preds_train_sample
  # predictions on the test set
  preds_test_sample <- xgboost_predictions$preds_test_sample
  # predictions on the validation set
  preds_val_sample <- xgboost_predictions$preds_val_sample

  
  train_y <- xgboost_predictions$train_y
  test_y <- xgboost_predictions$test_y
  valid_y <- xgboost_predictions$valid_y
  train_x <- xgboost_predictions$train_x
  
  ## End generate model predictions
  
  # Calibrate predictions
  
  # Fit model calibration model in testing data
  test_preds_calibr_model <- get_calibration_model(preds_test_sample, test_y)
  # test_preds_calibr_model_strata <- lapply(sort(unique(strata_var_test)), function(x) {
  #   get_calibration_model(preds_test_sample[strata_var_test==x], test_y[strata_var_test==x])
  # }
  # )
  # names(test_preds_calibr_model_strata) <- c("raceWhite", racevars)
  
  # Generate calibrated predictions 
  # train_preds_calibrated = calibrate_predictions(train_preds_calibr_model, preds_train_sample)
  test_preds_calibrated = calibrate_predictions(test_preds_calibr_model, preds_test_sample)
  valid_preds_calibrated = calibrate_predictions(test_preds_calibr_model, preds_val_sample)
  
  # Check calibration in test and validation sets overall and by strata
  test_recalibr_check = get_calibration_model(test_preds_calibrated, test_y)
  test_recalibr_check_strata <- lapply(sort(unique(strata_var_test)), function(x) {
    get_calibration_model(test_preds_calibrated[strata_var_test==x], test_y[strata_var_test==x])
  }
  )
  names(test_recalibr_check_strata) <- c("raceWhite", racevars)
  
 # valid_recalibr_check = get_calibration_model(valid_preds_calibrated, valid_y)
  valid_recalibr_check = parallel.validation.recalibration.model(validation.set.indicators, 
                                                                          valid_preds_calibrated, 
                                                                          valid_y,
                                                                          nodes)
  # valid_recalibr_check_strata <- lapply(sort(unique(strata_var_test)), function(x) {
  #   get_calibration_model(valid_preds_calibrated[strata_var_valid==x], valid_y[strata_var_valid==x])
  # }
  # )
  # names(valid_recalibr_check_strata) <- c("raceWhite", racevars)
  
  # Training sample quantiles of uncalibrated predictions
  qtiles_test_sample = quantile(preds_test_sample, probs = qtile_probs_test)
  
  ### End model and predictions
  
  ### Point Estimates
  overall.point.estimates <- get.overall.point.est(pred=preds_test_sample,
                                                   calib_pred=test_preds_calibrated,
                                                   outcome=test_y,
                                                   train.pred=preds_train_sample,
                                                   qtile.probs = qtile_probs_test)
  # Strata-specific point estimates                     
  strata.point.estimates <- get.strata.point.est(strata=strata_var_test, 
                                                 strata.labels=strata.labels,
                                                 pred=preds_test_sample,
                                                 calib_pred=test_preds_calibrated,
                                                 outcome=test_y,
                                                 train.pred=preds_train_sample,
                                                 qtile.probs=qtile_probs_test)
  
  point.estimates <- rbind(overall.point.estimates, strata.point.estimates)
  # point.estimates <- cbind("dataset"="test", point.estimates)
  
  
  # Point estimates in the validation set
  
  
  valdata.runtime <- system.time({
    validation.point.estimates <- parallel.get.validation.point.est(flags=validation.set.indicators, 
                                                                    pred=preds_val_sample,
                                                                    calib_pred = valid_preds_calibrated,
                                                                    outcome=valid_y, 
                                                                    train.pred=preds_train_sample,
                                                                    qtile.probs=qtile_probs_test, 
                                                                    strata.labels=strata.labels,
                                                                    strata = strata_var_valid,
                                                                    nodes = nodes )
  })
  
  # point.estimates <- rbind(point.estimates, 
  #                          cbind("dataset"="validation", validation.point.estimates))
  
  
  ### Marginal Calibration relative to fixed event rate.
  marginal_mean_test_calib = mean(test_preds_calibrated) - event_rate
  marginal_mean_valid_calib = mean(valid_preds_calibrated) - event_rate
  
  
  ### Perform bootstrap iterations for confidence intervals
  boot.runtime <- system.time({
    
    # Initiate storage for bootstrap indices
    boot.indx <- lapply(1:nboot, function(x) sample(1:length(test_preds_calibrated), replace = TRUE))
    save(boot.indx, file=paste0(fname.boot, sim_replicate, ".RData"))
    
    cl <- makeCluster(nodes)
    registerDoParallel(cl)
    boot.reps <- foreach(i=boot.indx, .export = c("perf.boot")) %dopar% {
      # Bootstrap sample
      perf.boot(pred = preds_test_sample[i], calib_pred = test_preds_calibrated[i], outcome = test_y[i],
                train.qtiles = qtiles_test_sample, overall=TRUE, strata=strata_var_test[i],
                strata.labels=c("raceWhite", "raceAsian","raceBlack",
                                "raceHP","raceIN","raceMUOT","raceUN",
                                "hispanic"))
    }
    stopCluster(cl)
    gc()  
    
  })
  
  ### Compute bootstrap confidence limits
  overall.boot.estimates <- get.overall.boot.est(boot.reps, qtiles_test_sample)
  
  strata.boot.estimates <- get.boot.est.strata(boot.reps, qtile=qtiles_test_sample, nboot) 
  
  boot.estimates <- rbind(overall.boot.estimates, strata.boot.estimates)
  
  boot.means <- subset(boot.estimates, Estimate=="mean")
  
  boot.mean.valid.bias <- boot.means[,c("Accuracy","Sens","Spec","PPV", "NPV",
                                        "Fscore","AUC", "AUCPR","Brier")] -
    validation.point.estimates[, c("Accuracy","Sens","Spec","PPV", "NPV",
                                   "Fscore","AUC", "AUCPR","Brier")]
  
  
  merge.boot.validation <- merge(boot.estimates, validation.point.estimates, 
                                 by=c("Pctile", "strata"), 
                                 suffixes = c("boot","validation"),
                                 sort = FALSE)
  
  # Compute and assemble coverage for all the estimates in this simulation
  boot.confints <- subset(merge.boot.validation, Estimate!="mean")
  boot.converage.list <- lapply(paste0(c("Accuracy","Sens","Spec","PPV", "NPV",
                                         "Fscore","AUC","AUCPR", "Brier")), function(x){
                                           
                                           t(sapply(seq(1, nrow(boot.confints), by=2), function(y) {
                                             cover_name = paste0(x, "_cover95")
                                             temp <- cbind("Measure"=x, boot.confints[y, c("Pctile", "strata")], 
                                                           as.integer(boot.confints[y, 
                                                                                    paste0(x, 
                                                                                           "validation")] >= boot.confints[y, paste0(x, "boot")] &
                                                                        boot.confints[y, paste0(x, "validation")] <= boot.confints[y+1, paste0(x, "boot")]))
                                             colnames(temp)[4] <- cover_name
                                             temp
                                           }))
                                         })
  # Data set containing coverage for this sim bye estimate, risk quantile and group.
  boot.confint.coverage <- do.call(rbind, boot.converage.list)
  boot.confint.coverage <- boot.confint.coverage %>% as.data.frame.array() %>% unnest(cols = c("Measure","Pctile", "strata", "Accuracy_cover95")) %>% as.data.frame()
  
  
  results_list <- list("point.estimates"=point.estimates, 
                       "validation.point.estimates"=validation.point.estimates,
                       "marginal_mean_test_calib"=marginal_mean_test_calib,
                       "marginal_mean_valid_calib"=marginal_mean_valid_calib, 
                       "boot.estimates"=boot.estimates, 
                       "test_preds_calibr_model"=test_preds_calibr_model,
                       "test_recalibr_check"=test_recalibr_check,
                       # "test_recalibr_check_strata"=test_recalibr_check_strata,
                       "valid_recalibr_check"=valid_recalibr_check,
                       # "valid_recalibr_check_strata"=valid_recalibr_check_strata,
                       "bias"=cbind(point.estimates[, c(1:3)], 
                                    as.matrix(point.estimates[,4:13]) - 
                                      as.matrix(validation.point.estimates[,4:13])),
                       "datgen.runtime"=datgen.runtime, "valdata.runtime"=valdata.runtime, 
                       "boot.runtime"=boot.runtime, "modelfit.runtime" = modelfit.time,
                       "coverage"=boot.confint.coverage)
  
  saveRDS(results_list, file=paste0(fname.results, sim_replicate, ".rds"))
  
  rm(results_list, point.estimates,validation.point.estimates,marginal_mean_test_calib,marginal_mean_valid_calib, xgboost_predictions,
     boot.estimates,test_preds_calibr_model,test_recalibr_check, 
     preds_train_sample, preds_test_sample, preds_val_sample, train_y, test_y, valid_y, train_x,
     #  test_recalibr_check_strata,
     
     valid_recalibr_check,
     
     #valid_recalibr_check_strata,
     datgen.runtime, valdata.runtime, boot.runtime, modelfit.time, boot.confint.coverage)
  
}
