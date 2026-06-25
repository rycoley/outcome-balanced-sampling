### RED Suicide Random Forest Model Fitting
### Fits Random Forest model upon simulated training dataset, then computes and 
### formats predictions on testing and validation datasets. Returns a list 
### object containing initial data, outcome prediction probabilities, 
### sample specific race indicators, and observed outcomes.  
### Arguments:
###   train             Training data (tibble or data.frame)
###   test              Testing data (tibble or data.frame)
###   valid             Validation data (tibble or data.frame)
###   test_race         Indicators of race/ethnicity from training data
###   train_race        Indicators of race/ethnicity from testing data
###   valid_race        Indicators of race/ethnicity from validation data
###   outcome_variable  Variable of interest (chr string)
###   params:
###     mtry_input      Number of columns to sample at each split (mtry)
###     node_size       Minimum terminal node sample size in a tree
###     n_tree          Number of trees in the forest 
###     outcome_diff    Number of non-events relative to cases (1:1 = 1; 1:2 = 2; 1:5=5)


fit_rf <- function(train, test, valid, 
                   test_race, train_race, valid_race,
                   outcome_variable, outcome_diff,
                   mtry_input, node_size, n_tree){
  
  training_data <- train %>% model.matrix(~0+., data=.)
  testing_data <- test %>% model.matrix(~0+., data=.)
  valid_data <- valid %>% select(colnames(train)) %>% model.matrix(~0+., data=.)
  
  if(is.na(outcome_diff)){
     ## Index Definition
      list.f <- vector(mode = "list", length = n_tree) 
      for(i in 1:length(list.f)){
        list.f[[i]] <- sboot(training_data, "EVENT90")}
      } else{
    ## Index Definition
    list.f <- vector(mode = "list", length = n_tree) 
    for(i in 1:length(list.f)){
      list.f[[i]] <- sbootprop(training_data, outcome_variable, ratio=outcome_diff)}
  }
  
  ## Fitting
  RF_mod <- ranger(dependent.variable.name = outcome_variable,  #specify outcome to predict
                   data = training_data,  #specify predctor data (subset all training data to exclude held-out fold and select only columns with predictors)
                   num.trees = n_tree,      #specify number of trees
                   mtry=mtry_input,     #specify number of predictors to sample at each split (mtry)
                   importance="none",  #don't calculate variable importance measures
                   write.forest=TRUE,  #save the random forest object
                   probability=TRUE,  #calculate probabilities for terminal nodes (instead of a classificaiton tree that uses majority voting)
                   min.node.size= node_size,   #specify minimum node size
                   respect.unordered.factors="partition",   #for categorical variables, consider all grouping of categories (the alternative is that a categorical variable is turning into a number factor and treated like a continuous variable)
                   oob.error=TRUE,   #do not calculate out of bag statistics
                   save.memory=FALSE,   # do not use memory saving options (can't recall why, but they don't work for some part of what we're doing here)
                   inbag=list.f
                   #num.threads = 7 # DO NOT use this option, it does not make things run faster
      )
  
  ## Extract outcome level positions to avoid level swapping
  outcome_level_position <- which(RF_mod$forest$class.values==1)
  ## Prediction
  preds_train <- predict(RF_mod, data = training_data)$predictions[,outcome_level_position]
  preds_test <- predict(RF_mod, data = testing_data)$predictions[,outcome_level_position]
  preds_val <- predict(RF_mod, data = valid_data)$predictions[,outcome_level_position]
  
  # train_inbag_index <- which(preds_train==0)
  # test_inbag_index <- which(preds_test!=0)
  # valid_inbag_index  <- which(preds_val!=0)
  
  preds_train <- replace(preds_train, preds_train==0, min(preds_train[c(which(preds_train!=0))]))
  preds_test <-  replace(preds_test, preds_test==0, min(preds_test[c(which(preds_test!=0))]))
  preds_val <-  replace(preds_val, preds_val==0, min(preds_val[c(which(preds_val!=0))]))
  

  ## Output Packaging
  return(list("preds_test_sample" = preds_test,
              "preds_train_sample" = preds_train,
              "preds_val_sample" = preds_val,
              "test_x" = test  %>% select(-`outcome_variable`) %>% model.matrix(~0+., data=.),
              "test_y" = test %>% pull(`outcome_variable`) %>% as.numeric(),
              "train_x" = train %>% select(-`outcome_variable`) %>% model.matrix(~0+., data=.),
              "train_y" = train %>% pull(`outcome_variable`) %>% as.numeric(),
              "valid_x" = valid[,colnames(train)] %>% select(-`outcome_variable`) %>% model.matrix(~0+., data=.),
              "valid_y" = valid[,colnames(train)] %>% pull(`outcome_variable`) %>% as.numeric(),
              "race_test" = test_race,
              "race_train" = train_race,
              "valid_race" = valid_race))}
    
  