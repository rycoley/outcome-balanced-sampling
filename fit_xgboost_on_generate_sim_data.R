### RED Suicide XGBoost Model Fitting
### Fits XGBoost model upon simulated training dataset, then computes and 
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
###     learning_rate   Boosting rate (0,1]
###     depth           Boosted tree depth [1,K]
###     metric          XGBoost supported evaluation metric (string) 
###     sub_count       Number of positive cases observed in model 
###                     subsampling [1,K]. If using all possible cases let 
###                     sub_count = "all"
###     iterations      Number of boosting iterations [1,K]
###     outcome_diff    Number of non-events relative to cases (1:1 = 1; 1:2 = 2; 1:5=5)



fit_xgboost <- function(train, test, valid, 
                        test_race, train_race, valid_race,
                        outcome_variable, outcome_diff,
                        learning_rate, depth, metric, sub_count, iterations){
  
  if(is.na(outcome_diff)){
    
    if(is.numeric(sub_count)==T){
      ## Data Preparation
      
      training_data <- tibble_to_XGB(input_data = train, outcome="EVENT90")
#      training_2 <- tibble_to_XGB_2(input_data = train, outcome=outcome_variable)
      
      testing_data <- tibble_to_XGB(input_data = test, outcome="EVENT90")
      valid_data <- tibble_to_XGB(input_data =  valid[, colnames(train)], outcome="EVENT90")
      
      ## Parameter Definition
      event_vector <-  train %>% pull(`outcome_variable`) %>% as.numeric()
      subsample_prop <- sub_count/sum(!!event_vector)
      param_list <- list(eta=learning_rate, max_depth=depth,
                         eval_metric=metric, subsample = subsample_prop)
      
      ## Fitting
      xgb_model <- xgboost::xgboost(data = training_data, params = param_list,
                                    nrounds=iterations, 
                                    objective = "binary:logistic", verbose=0)
      
      ## Prediction
      preds_train <- training_data %>% predict(xgb_model, .)
      preds_test <- testing_data %>% predict(xgb_model, .)
      preds_val <- valid_data %>% predict(xgb_model, .)
      
      ## Output Packaging
      return(list("preds_test_sample" = preds_test,
                  "preds_train_sample" = preds_train,
                  "preds_val_sample" = preds_val,
                  "test_x" = test %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                  "test_y" = test %>% pull(`outcome_variable`) %>% as.numeric(),
                  "train_x" = train %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                  "train_y" = train %>% pull(`outcome_variable`) %>% as.numeric(),
                  "valid_x" = valid %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                  "valid_y" = valid[,colnames(train)] %>% pull(`outcome_variable`) %>% as.numeric(),
                  "race_test" = test_race,
                  "race_train" = train_race,
                  "valid_race" = valid_race))
      } else{
      ## Data Preparation
      training_data <- tibble_to_XGB(input_data = train, outcome=outcome_variable)
      testing_data <- tibble_to_XGB(input_data = test, outcome=outcome_variable)
      valid_data <- tibble_to_XGB(input_data =  valid[, colnames(train)], outcome=outcome_variable)
      
      ## Parameter Definition
      param_list <- list(eta=learning_rate, max_depth=depth,
                         eval_metric=metric, subsample = 1)
      
      ## Fitting
      xgb_model <- xgboost::xgboost(data = training_data, params = param_list,
                                    nrounds=iterations, 
                                    objective = "binary:logistic", verbose=0)
      
      ## Prediction
      preds_train <- training_data %>% predict(xgb_model, .)
      preds_test <- testing_data %>% predict(xgb_model, .)
      preds_val <- valid_data %>% predict(xgb_model, .)
      
      ## Output Packaging
      return(list("preds_test_sample" = preds_test,
                  "preds_train_sample" = preds_train,
                  "preds_val_sample" = preds_val,
                  "test_x" = test %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                  "test_y" = test %>% pull(`outcome_variable`) %>% as.numeric(),
                  "train_x" = train %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                  "train_y" = train %>% pull(`outcome_variable`) %>% as.numeric(),
                  "valid_x" = valid %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                  "valid_y" = valid[,colnames(train)] %>% pull(`outcome_variable`) %>% as.numeric(),
                  "race_test" = test_race,
                  "race_train" = train_race,
                  "valid_race" = valid_race))}
    
  } else{
    majority_count <- round(train %>% filter(!!as.symbol(outcome_variable) == 0) %>% nrow()/outcome_diff, 0)
    train_resamp <- bind_rows(train %>% filter(!!as.symbol(outcome_variable) == 1) %>% sample_n(., size=majority_count, replace = TRUE),
                              train %>% filter(!!as.symbol(outcome_variable) == 0))
    
    if(is.numeric(sub_count)==T){
    ## Data Preparation
    
    training_data <- tibble_to_XGB(input_data = train_resamp, outcome=outcome_variable)
    testing_data <- tibble_to_XGB(input_data = test, outcome=outcome_variable)
    valid_data <- tibble_to_XGB(input_data =  valid[, colnames(train)], outcome=outcome_variable)
    
    ## Parameter Definition
    event_vector <-  train_resamp %>% pull(`outcome_variable`) %>% as.numeric()
    subsample_prop <- sub_count/sum(!!event_vector)
    param_list <- list(eta=learning_rate, max_depth=depth,
                       eval_metric=metric, subsample = subsample_prop)
    
    ## Fitting
    xgb_model <- xgboost::xgboost(data = training_data, params = param_list,
                                  nrounds=iterations, 
                                  objective = "binary:logistic", verbose=0)
    
    ## Prediction
    preds_train <- tibble_to_XGB(input_data = train, outcome=outcome_variable) %>% predict(xgb_model, .)
    preds_test <- testing_data %>% predict(xgb_model, .)
    preds_val <- valid_data %>% predict(xgb_model, .)
    
    ## Output Packaging
    return(list("preds_test_sample" = preds_test,
                "preds_train_sample" = preds_train,
                "preds_val_sample" = preds_val,
                "test_x" = test %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                "test_y" = test %>% pull(`outcome_variable`) %>% as.numeric(),
                "train_x" = train %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                "train_y" = train %>% pull(`outcome_variable`) %>% as.numeric(),
                "valid_x" = valid %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                "valid_y" = valid[,colnames(train)] %>% pull(`outcome_variable`) %>% as.numeric(),
                "race_test" = test_race,
                "race_train" = train_race,
                "valid_race" = valid_race))
    } else{
    ## Data Preparation
    training_data <- tibble_to_XGB(input_data = train_resamp, outcome=outcome_variable)
    testing_data <- tibble_to_XGB(input_data = test, outcome=outcome_variable)
    valid_data <- tibble_to_XGB(input_data =  valid[, colnames(train)], outcome=outcome_variable)
    
    ## Parameter Definition
    param_list <- list(eta=learning_rate, max_depth=depth,
                       eval_metric=metric, subsample = 1)
    
    ## Fitting
    xgb_model <- xgboost::xgboost(data = training_data, params = param_list,
                                  nrounds=iterations, 
                                  objective = "binary:logistic", verbose=0)
    
    ## Prediction
    preds_train <- tibble_to_XGB(input_data = train, outcome=outcome_variable) %>% predict(xgb_model, .)
    preds_test <- testing_data %>% predict(xgb_model, .)
    preds_val <- valid_data %>% predict(xgb_model, .)
    
    ## Output Packaging
    return(list("preds_test_sample" = preds_test,
                "preds_train_sample" = preds_train,
                "preds_val_sample" = preds_val,
                "test_x" = test %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                "test_y" = test %>% pull(`outcome_variable`) %>% as.numeric(),
                "train_x" = train %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                "train_y" = train %>% pull(`outcome_variable`) %>% as.numeric(),
                "valid_x" = valid %>% select(-all_of(`outcome_variable`)) %>% model.matrix(~0+., data=.),
                "valid_y" = valid[,colnames(train)] %>% pull(`outcome_variable`) %>% as.numeric(),
                "race_test" = test_race,
                "race_train" = train_race,
                "valid_race" = valid_race))
    }}}
