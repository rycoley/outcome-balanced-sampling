#logit function
logit<-function(x){return(log(x/(1-x)))}

#inverse logit function
expit <- function(x){return(exp(x)/(1+exp(x)))}

#estimate calibration model given set predictions and outcome
#purpose in training data is to estimate and save a model to recalibrate predictions
#purpose in testing and validation data is to evaluate coefficients on recalibrated predictions (to see that they are in fact recalibrated)
#pred: predictions to evaluate calibration for
#in training data, use "uncalibrated" predictions, output from prediction model. 
#in testing or validation data, use recalibrated predictions
#out: outcomes from the data (binary)
get_calibration_model <- function(pred, out){
  lp <- logit(pred)
  calib_mod <- glm(out~lp, family="binomial")
  return(calib_mod$coefficients)}
#output is a vector of length two with intercept and slope


#function to calibrate predictions for new dataset (testing or validation set)
#calib_mod_coefficients: output from get_calibration_model
#pred_new: fitted values in test or validation set, predictions to be calibrated
calibrate_predictions <- function(calib_mod_coefficients, pred_new){
  recalib_pred <- expit(calib_mod_coefficients[1] + 
                          calib_mod_coefficients[2]*logit(pred_new))
  return(as.vector(recalib_pred))}
#output is a vector the same length as pred_new with calibrated predictions with values between 0 and 1

#function to estimate calibration models and average results across validation 
#replicates, given validation set indicators (flags) and the number of cores 
#available for parallelization.
parallel.validation.recalibration.model <- function(flags, # matrix of flags indicating resampling 
                                                    calib_pred, # recalibrated validation predictions
                                                    outcome, #outcome data from validation set 
                                                    nodes = nodes # number of workers to use for parallelization
) 
{
  
  cl <- makeCluster(nodes)
  registerDoParallel(cl)
  point.valid.avg.all <- foreach(rep=1:ncol(flags), .export = c("get_calibration_model", "logit", "expit")) %dopar% {
    # Bootstrap sample
    
    temp.res <- get_calibration_model(calib_pred[flags[, rep]==1], outcome[flags[, rep]==1])
    return(temp.res) 
    
  }
  # registerDoSEQ()
  parallel::stopCluster(cl = cl)
  
  # Returns data.frame if no strata-specific estimates.
  point.valid.avg <- Reduce("+", point.valid.avg.all)/length(point.valid.avg.all)
  return(point.valid.avg)
}

marg_val_calib = function(results) {
  
  temp = rbind(c(mean(results$master_list$marginal_mean_valid_calib$marginal_mean_valid_calib, na.rm=TRUE),
                 sd(results$master_list$marginal_mean_valid_calib$marginal_mean_valid_calib, na.rm=TRUE)),
               cbind(as.matrix(by(results$master_list$valid_recalibr_check$valid_recalibr_check,
                                  results$master_list$valid_recalibr_check$term, mean, na.rm=TRUE)),
                     as.matrix(by(results$master_list$valid_recalibr_check$valid_recalibr_check, 
                                  results$master_list$valid_recalibr_check$term, sd, na.rm=TRUE))),
               cbind(as.matrix(by(results$master_list$valid_recalibr_check$valid_recalibr_check - rep(c(0,1), 
                    times=length(results$master_list$valid_recalibr_check$valid_recalibr_check)/2), 
                    results$master_list$valid_recalibr_check$term, mean, na.rm=TRUE)),
                     as.matrix(by(results$master_list$valid_recalibr_check$valid_recalibr_check - rep(c(0,1), 
                    times=length(results$master_list$valid_recalibr_check$valid_recalibr_check)/2), 
                                  results$master_list$valid_recalibr_check$term, sd, na.rm=TRUE))))
  rownames(temp)[1] = "Marginal Mean Calibration"
  rownames(temp)[c(4,5)] = c("Intercep Bias", "Slope Bias")
  colnames(temp) = c("Mean", "SD")
  temp
}