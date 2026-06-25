#############################################################################################################
### This function was written by Rod Walker, August 2019
### It relies on the CRAN packages data.table and ROCR, and the function "prediction" within ROCR
### It calls the commands - library(data.table) and library(ROCR) - to make sure the pacakges are installed
### Value: data.frame/data.table.
#############################################################################################################

#### perf.results - function to provide calibration and performance tables and metrics
####

perf.results <- function(pred, # predicted probability of outcome=1 for each observation in the test set
                         calib_pred,  # recalibrated predicted probability of outcome=1 for each observation in the test set
                         outcome, # numeric code for each unique outcome in the test set (non-events need to be coded as NA)
                         train.pred, # predicted probability of outcome=1 for each observation in the training set,
                         qtile.probs, # vector of percentiles to use for calibration/performance strata
                         strata=NULL, # Numeric categorical variableStrata to measure performance withing. Skips if NULL (default)
                         level.labels=NULL # Character labels for strata increasing order of numeric coding
) {    
  ## data.table version 1.12.2
  library(data.table)
  ## ROCR version 1.0.7
  library(ROCR)
  train.qtiles <- quantile(train.pred, qtile.probs)
  # create ROCR prediction object and table of values (needed to construct performance table and plots)
  # prediction is a function in the ROCR package
  
  obj.rocr <- prediction(pred[!is.na(pred)], outcome[!is.na(outcome)])
  tab.rocr <- data.table(RiskCutpoint=obj.rocr@cutoffs[[1]],
                         Accuracy=performance(obj.rocr,"acc")@y.values[[1]],
                         Sens=performance(obj.rocr,"sens")@y.values[[1]],
                         Spec=performance(obj.rocr,"spec")@y.values[[1]],
                         PPV=performance(obj.rocr,"ppv")@y.values[[1]],
                         NPV=performance(obj.rocr,"npv")@y.values[[1]],
                         Fscore=performance(obj.rocr,"f")@y.values[[1]])
  
  # create performance table
  perf.tab <- rbindlist(lapply(train.qtiles, function(x){tail(tab.rocr[RiskCutpoint>=x], 1)}))
  perf.tab$RiskCutpoint <- train.qtiles
  # perf.tab <- data.table(Pctile=names(pctile), perf.tab)
  
  # compute auc
  auc.rocr <- performance(obj.rocr,"auc")
  
  # compute aucpr
  aucpr.rocr <- performance(obj.rocr,"aucpr")
  
  # compute Brier Score (MSE in our binary case)
  brier.sc <- mean((calib_pred-outcome)^2, na.rm = TRUE)
  
  # Calibration
  boot_mean_calibr = mean(calib_pred, na.rm = TRUE) - mean(outcome, na.rm = TRUE)
  
  # return calibration table, preformance table, and AUC 
  overall.res <-   cbind(perf.tab, 
                         "AUC"=auc.rocr@y.values[[1]],
                         "AUCPR"=aucpr.rocr@y.values[[1]],
                         "Brier"=brier.sc, 
                         "Bootstrap Calibration"=boot_mean_calibr)
  
  return(overall.res)
  
}
####
#### end perf.results.id function


#############################################################################################################
### This function was written by Robert Wellman partially using code developed by
### Rod Walker in August 2019.
### It relies on the CRAN packages data.table and ROCR, and the function "prediction" within ROCR
### It calls the commands - library(data.table) and library(ROCR) - to make sure the pacakges are installed
###
### Function can produce either overall performance, strata-specific performance or both.
### overall=TRUE included overall performance, Strata non-null included strata specific estimates.
### If overall=FALSE and strata != NULL then only stratified results are returned.
###
### Value
### A data frame if overall only, otherwise a list of dimension 2
#############################################################################################################

perf.boot <- function(pred, # predicted probability of outcome=1 for each observation in the test set
                      calib_pred,  # recalibrated predicted probability of outcome=1 for each observation in the test set
                      outcome, # 0/1 numeric outcome for each observation in the test set
                      train.qtiles=c(0.95), # cutpoints in training data that correspond to desired percentiles
                      overall=TRUE, # Logical should overall performance be calculated
                      strata=NULL, # Numeric categorical variable coding strata, starts at 0.
                      strata.labels=NULL # Character labels matching the ordered unique numeric values.
                      
) {    
  
  ## data.table version 1.12.2
  library(data.table)
  ## ROCR version 1.0.7
  library(ROCR)
  
  # Stop if arguments don't make sense.
  if(is.null(strata) & overall==FALSE) {
    stop("Overall argument must not be FALSE when strata arg is non-null. \n Nothing is returned.")
  }
  
  # Compute overall performance if requested.
  if(overall==TRUE) {
    
    # create ROCR prediction object and table of values (needed to construct performance table)
    # prediction is a function in the ROCR package
    obj.rocr <- prediction(pred[!is.na(pred)], outcome[!is.na(outcome)])
    tab.rocr <- data.table(RiskCutpoint=obj.rocr@cutoffs[[1]],
                           Accuracy=performance(obj.rocr,"acc")@y.values[[1]],
                           Sens=performance(obj.rocr,"sens")@y.values[[1]],
                           Spec=performance(obj.rocr,"spec")@y.values[[1]],
                           PPV=performance(obj.rocr,"ppv")@y.values[[1]],
                           NPV=performance(obj.rocr,"npv")@y.values[[1]],
                           Fscore=performance(obj.rocr,"f")@y.values[[1]])
    
    # create performance table
    perf.tab <- rbindlist(lapply(train.qtiles, function(x){tail(tab.rocr[RiskCutpoint>=x], 1)}))
    # perf.tab$RiskCutpoint <- NULL
    perf.tab <- data.table(Pctile=names(train.qtiles), perf.tab)
    
    # compute auc
    auc.rocr <- performance(obj.rocr,"auc")
    
    # Compute aucpr
    aucpr.rocr <- performance(obj.rocr, "aucpr")
    
    # compute Brier Score (MSE in our binary case)
    brier.sc <- mean((calib_pred-outcome)^2, na.rm=TRUE)
    
    # Calibration
    boot_mean_calibr = mean(calib_pred) - mean(outcome)
    
    overall.res <-   cbind(perf.tab, 
                           "AUC"=auc.rocr@y.values[[1]],
                           "AUCPR"=aucpr.rocr@y.values[[1]],
                           "Brier"=brier.sc, 
                           "Bootstrap Calibration"=boot_mean_calibr)
    
  } 
  
  # Strata specific estimates if strata variable if requested (variable passed).
  if(!is.null(strata)) {
    
    # ordered numeric strata values
    strata_values <- sort(unique(strata))
    
    # Iterate over strata, compute performance
    res.strata <- lapply(strata_values, function(cat) {
      
      strata.indx <- strata==cat
      
      # If strata has no outcomes then return NA, otherwise continue.
      if(sum(outcome[strata.indx])==0) {
        temp.perf.matrix <- matrix(NA, nrow = length(train.qtiles), ncol = 7)
        colnames(temp.perf.matrix) <- c("RiskCutpoint","Accuracy","Sens","Spec","PPV","NPV","Fscore")
        temp.perf.matrix <- as.data.frame(temp.perf.matrix)
        temp.perf.matrix <- cbind("Pctile"=names(train.qtiles), temp.perf.matrix)
        return(list("Performance"=temp.perf.matrix,
                    "AUC"=NA,"AUCPR"=NA, "Brier Score"=NA, 
                    "Bootstrap Calibration"=NA))
      }
      
      obj.rocr <- prediction(na.omit(pred[strata.indx]), na.omit(outcome[strata.indx]))
      tab.rocr <- data.table(RiskCutpoint=obj.rocr@cutoffs[[1]],
                             Accuracy=performance(obj.rocr,"acc")@y.values[[1]],
                             Sens=performance(obj.rocr,"sens")@y.values[[1]],
                             Spec=performance(obj.rocr,"spec")@y.values[[1]],
                             PPV=performance(obj.rocr,"ppv")@y.values[[1]],
                             NPV=performance(obj.rocr,"npv")@y.values[[1]],
                             Fscore=performance(obj.rocr,"f")@y.values[[1]])
      
      # create performance table
      perf.tab <- rbindlist(lapply(train.qtiles, function(x){tail(tab.rocr[RiskCutpoint>=x], 1)}))
      
      perf.tab <- data.table(Pctile=names(train.qtiles), perf.tab)
      
      # compute auc
      auc.rocr <- performance(obj.rocr,"auc")
      
      # compute aucpr
      aucpr.rocr <- performance(obj.rocr,"aucpr")
      
      # compute Brier Score (MSE in our binary case)
      brier.sc <- mean((calib_pred[strata.indx]-outcome[strata.indx])^2, na.rm=TRUE)
      
      # Calibration
      boot_mean_calibr = mean(calib_pred[strata.indx], na.rm=TRUE) - mean(outcome[strata.indx], na.rm=TRUE)
      
      return(list("Performance"=perf.tab,
                  "AUC"=auc.rocr@y.values[[1]],"AUCPR"=aucpr.rocr@y.values[[1]], "Brier Score"=brier.sc, 
                  "Bootstrap Calibration"=boot_mean_calibr))
      
    })
    # End loop over strata
    
    # Parse results for presentation.
    boot.performance <- do.call(rbind, lapply(res.strata, function(x) {
      x$Performance
    }))
    
    class(boot.performance) <- "data.frame"
    rownames(boot.performance) <- paste0(rep(strata.labels[strata_values + 1], 
                                             each=length(train.qtiles)),
                                         rownames(boot.performance))
    
    boot.performance$strata <- rep(strata.labels[strata_values + 1], 
                                   each=length(train.qtiles))
    
    boot.performance <- boot.performance[, c( "strata", "Pctile",
                                              "Accuracy", "Sens",
                                              "Spec", "PPV", "NPV",
                                              "Fscore")]
    # Bootstrap calibration: mean prediction - mean in sample
    strata.calibr <- sapply(res.strata, function(x) {
      x$`Bootstrap Calibration`
    })
    names(strata.calibr) <- strata.labels[strata_values + 1]
    #AUC
    strata.auc <- sapply(res.strata, function(x) {
      x$`AUC`
    })
    names(strata.auc) <- strata.labels[strata_values + 1]
    #AUCPR
    strata.aucpr <- sapply(res.strata, function(x) {
      x$`AUCPR`
    })
    names(strata.aucpr) <- strata.labels[strata_values + 1]
    # Brier
    strata.brier <- sapply(res.strata, function(x) {
      x$`Brier Score`
    })
    names(strata.brier) <- strata.labels[strata_values + 1]
    # Compile
    strata.res <- cbind(boot.performance, 
                        "AUC"=rep(strata.auc, each=length(train.qtiles)),
                        "AUCPR"=rep(strata.aucpr, each=length(train.qtiles)),
                        "Brier"=rep(strata.brier, each=length(train.qtiles)),
                        "Bootstrap Calibration"=rep(strata.calibr, each=length(train.qtiles)))
    
    
    # names(res.list) <- c("Bootstrap Calibration","Performance","AUC", "Brier Score")
    if(overall==TRUE) {
      return(list("Overall"=overall.res,
                  "By Strata"=strata.res))
    } else {
      return("By Strata"=strata.res)
    }
  }
  if(!is.null(overall.res)) {
    return("Overall"=overall.res)
  } else {
    return(NULL)
  }
}

####
#### end perf.boot function

### Overall point estimates
get.overall.point.est <- function(pred, # preds in test set 
                                  calib_pred, # recalibrated predictions from test set
                                  outcome, # outcome in test set 
                                  train.pred, # Predictions in training set
                                  qtile.probs # Probabilities at which quantiles are desired
) 
{
  point.test.performance <- perf.results(pred=pred, 
                                         calib_pred=calib_pred, 
                                         outcome=outcome, 
                                         train.pred=train.pred, 
                                         qtile.probs = qtile.probs)
  
  point.test.performance <- cbind("Pctile"=paste0(100*qtile.probs, "%"), "strata"="overall", point.test.performance)
  
  # Returns data.frame
  return(point.test.performance)
  
}


### Point estimates for strata.
get.strata.point.est <- function(strata, # Numeric categorical variable coding strata, starts at 0.
                                 strata.labels, # Character labels matching the ordered unique numeric values.
                                 pred, # Test set predictions
                                 calib_pred, # recalibrate predictions from test set
                                 outcome, # Test outcomes
                                 train.pred, # Training set predictions
                                 qtile.probs # Probabilities at which to takes quantiles (e.g., .95, .975)
){

  # Point estimates in racial subgroups, test set
  point.test.performance.strata <- lapply(1:length(strata.labels)-1, function(grp){
    
    strata.indx <- strata==grp
    
    if(sum(outcome[strata.indx], na.rm = TRUE)==0) {
      temp.perf.matrix <- matrix(NA, nrow = length(qtile.probs), ncol = 11)
      colnames(temp.perf.matrix) <- c("RiskCutpoint","Accuracy","Sens","Spec","PPV","NPV","Fscore", "AUC", "AUCPR", "Brier", "Bootstrap Calibration")
      temp.perf.matrix <- as.data.frame(temp.perf.matrix)
      temp.perf.matrix <- cbind("Pctile"=paste0(100*qtile.probs, "%"), temp.perf.matrix)
      temp <- temp.perf.matrix
    } else{
      temp <- perf.results(pred=pred[strata.indx], 
                           calib_pred=calib_pred[strata.indx], 
                           outcome=outcome[strata.indx], 
                           train.pred=train.pred, 
                           qtile.probs=qtile.probs 
      )
      temp <- cbind("Pctile"=paste0(100*qtile.probs, "%"), temp)
    }
  })
  
  names(point.test.performance.strata) <- strata.labels
  point.test.performance.strata <- do.call(rbind, point.test.performance.strata)
  
  class(point.test.performance.strata) <- "data.frame"
  point.test.performance.strata$strata <- rep(strata.labels, each=length(qtile.probs))
  
  # Returns data.frame
  point.test.performance.strata[, c("Pctile", "strata", "RiskCutpoint",
                                    names(point.test.performance.strata)[-c(1:2, 
                                                                            ncol(point.test.performance.strata))])]
  
}

### Validation set point estimates
get.validation.point.est <- function(flags, # matrix of flags indicating re sampling 
                                     pred, #predictions from the validation set
                                     calib_pred, # recalibrate predictions from test set
                                     outcome, #outcome from validation set 
                                     train.pred, #training set predictions 
                                     qtile.probs=c(0.75,0.90,0.95,0.99),#probabilities for quantile()
                                     strata=NULL, # numeric categorical variable start at 0
                                     strata.labels=NULL # text labels for the unique values of strata
) 
{
  
  # Loop over sample validation sets
  point.valid.avg.all <- lapply(1:ncol(flags), function(rep){
    
    # Overall estimates
    temp.res <- perf.results(pred=pred[flags[, rep]==1], 
                             calib_pred=calib_pred[flags[, rep]==1],
                             outcome=outcome[flags[, rep]==1], 
                             train.pred=train.pred, 
                             qtile.probs=qtile.probs) 
    
    class(temp.res) <- "data.frame"
    
    # Strata-specific estimates
    if(!is.null(strata) & !is.null(strata.labels)) {

      temp.res.strata <- get.strata.point.est(strata=strata[flags[, rep]==1], 
                                              strata.labels=strata.labels,
                                              pred=pred[flags[, rep]==1],
                                              calib_pred=calib_pred[flags[, rep]==1],
                                              outcome=outcome[flags[, rep]==1], 
                                              train.pred=train.pred, 
                                              qtile.probs=qtile.probs)
      
      class(temp.res) <- "data.frame"
      class(temp.res.strata) <- "data.frame"
      temp.res.strata$Pctile <- NULL
      temp.res.strata$strata <- NULL
      
      return(list(temp.res, temp.res.strata))
    }
    
    class(temp.res) <- "data.frame"
    return(list(temp.res))
  })
  
  
  if(!is.null(strata) & !is.null(strata.labels)) {
    point.valid.avg <- lapply(1:length(point.valid.avg.all), function(x) return(point.valid.avg.all[[x]][[1]]))
    point.valid.avg.strata <- lapply(1:length(point.valid.avg.all), function(x) return(point.valid.avg.all[[x]][[2]]))
    point.valid.avg <- Reduce("+", point.valid.avg)/length(point.valid.avg)
    point.valid.avg.strata <- Reduce("+", point.valid.avg.strata)/length(point.valid.avg.strata)
    
    # Returns list of dimension 2 if strata-specific estimates included
    return(rbind(cbind("Pctile"=paste0(100*qtile.probs, "%"), "strata"="overall", point.valid.avg),
                cbind("Pctile"=paste0(100*qtile.probs, "%"),"strata"=rep(strata.labels, each=length(qtile.probs)), point.valid.avg.strata)))
  }
  
  # Returns data.frame if no strata-specific estimates.
  point.valid.avg <- Reduce("+", point.valid.avg.all)/length(point.valid.avg.all)
  return(cbind("Pctile"=paste0(100*qtile.probs, "%"), "strata"="overall", point.valid.avg))
}


### Parallelized validation set point estimates
parallel.get.validation.point.est <- function(flags, # matrix of flags indicating re sampling 
                                              pred, #predictions from the validation set
                                              calib_pred, # recalibrate predictions from test set
                                              outcome, #outcome from validation set 
                                              train.pred, #training set predictions 
                                              qtile.probs=c(0.95), #probabilities for quantile()
                                              strata=NULL, # numeric categorical variable start at 0
                                              strata.labels=NULL, # text labels for the unique values of strata
                                              nodes = nodes # number of workers to use for parallelization
) 
{
  
  cl <- makeCluster(nodes)
  registerDoParallel(cl)
  point.valid.avg.all <- foreach(rep=1:ncol(flags), .export=c("perf.results", "get.strata.point.est")) %dopar% {
    # Bootstrap sample
    temp.res <-  perf.results(pred=pred[flags[, rep]==1], 
                              calib_pred=calib_pred[flags[, rep]==1],
                              outcome=outcome[flags[, rep]==1], 
                              train.pred=train.pred, 
                              qtile.probs=qtile.probs) 
    class(temp.res) <- "data.frame"
    
    if(!is.null(strata) & !is.null(strata.labels)) {
      
      temp.res.strata <-  get.strata.point.est(strata=strata[flags[, rep]==1], 
                                               strata.labels=strata.labels,
                                               pred=pred[flags[, rep]==1],
                                               calib_pred=calib_pred[flags[, rep]==1],
                                               outcome=outcome[flags[, rep]==1], 
                                               train.pred=train.pred, 
                                               qtile.probs=qtile.probs)
      
      
      class(temp.res) <- "data.frame"
      class(temp.res.strata) <- "data.frame"
      temp.res.strata$Pctile <- NULL
      temp.res.strata$strata <- NULL
      
      return(list(temp.res, temp.res.strata))
    } else{ 
      return(temp.res) 
    }
  }
  parallel::stopCluster(cl = cl)
  
  
  
  if(!is.null(strata) & !is.null(strata.labels)) {
    point.valid.avg <- lapply(1:length(point.valid.avg.all), function(x) return(point.valid.avg.all[[x]][[1]]))
    point.valid.avg.strata <- lapply(1:length(point.valid.avg.all), function(x) return(point.valid.avg.all[[x]][[2]]))
    point.valid.avg <- Reduce("+", point.valid.avg)/length(point.valid.avg)
    point.valid.avg.strata <- Reduce("+", point.valid.avg.strata)/length(point.valid.avg.strata)
    
    # Returns list of dimension 2 if strata-specific estimates included
    return(rbind(cbind("Pctile"=paste0(100*qtile.probs, "%"), "strata"="overall", point.valid.avg),
                 cbind("Pctile"=paste0(100*qtile.probs, "%"),"strata"=rep(strata.labels, each=length(qtile.probs)), point.valid.avg.strata)))
  }
  
  # Returns data.frame if no strata-specific estimates.
  point.valid.avg <- Reduce("+", point.valid.avg.all)/length(point.valid.avg.all)
  return(cbind("Pctile"=paste0(100*qtile.probs, "%"), "strata"="overall", point.valid.avg))
}

###############################################################################
### getoverall.boot.est - function to extract overall bootstrap results
### args: 
### boot.reps   List with results from bootstrapping, multiple instances of perf.boot 
### qtiles      Training sample quantiles.
###############################################################################
get.overall.boot.est <- function(boot.reps, qtiles) {
  
  overall.boot <- do.call(rbind, 
                          lapply(1:(nboot), function(x){
                            boot.reps[[x]]$Overall
                          }))   
  
  temp.summary <-  lapply(1:length(qtiles), function(x) {
    
    temp.indx <- overall.boot$Pctile==names(qtiles)[x] 
    temp.res <- apply(overall.boot[temp.indx,-c(1,2)], 2, function(z) {
      c("Mean"=mean(z, na.rm=TRUE), quantile(z, probs = c(0.025, 0.975),
                                             na.rm=TRUE))
    })
    
    temp.res <- as.data.frame(temp.res, check.names =TRUE)
    cbind("Estimate"=c("mean", "p025", "p975"), 
          "Pctile"=names(qtiles)[[x]], "strata"="overall",
          "RiskCutpoint"= qtiles[[x]], temp.res)
  })
  
  temp.summary <- do.call(rbind, temp.summary)
  rownames(temp.summary) <- NULL
  
  return(temp.summary)
}

###############################################################################
### getboot.est.strata - function to extract overall bootstrap results
### args: 
### boot.reps   List with results from bootstrapping, multiple instances of perf.boot .
### qtiles      Training sample quantiles.
### nboot       Number of iterations performed.
###############################################################################
get.boot.est.strata <- function(boot.reps, qtile, nboot) {
  
  boot.results <- do.call(rbind, lapply(1:(nboot), function(x){
    boot.reps[[x]]$`By Strata`
  }))
  
  strata.levels <- unique(boot.results$strata)
  
  summarize.boot <- mapply(function(x,y) {
    
    temp.indx <- boot.results$Pctile==x & boot.results$strata==y
    temp.res <- apply(boot.results[temp.indx,-c(1,2)], 2, function(z) {
      c("Mean"=mean(z, na.rm=TRUE), quantile(z, probs = c(0.025, 0.975),
                                             na.rm=TRUE))
    })
    
    temp.res <- as.data.frame(temp.res, check.names=TRUE)
    temp.res <- cbind("Estimate"=c("mean", "p025", "p975"),"Pctile"=x,"strata"=y, temp.res)
    rownames(temp.res) <- NULL
    temp.res
  }, rep(names(qtile), times=length(strata.levels)), rep(strata.levels, each=length(qtile)),
  SIMPLIFY = FALSE)
  
  temp.res <- do.call(rbind, summarize.boot)
  rownames(temp.res) <- NULL
  
  temp.res$RiskCutpoint <- rep(qtile, each = 3, times = length(strata.levels))
  # Returns data.frame
  temp.res[, c(names(temp.res)[1:3], "RiskCutpoint", names(temp.res)[-c(1:3, ncol(temp.res))])]
}