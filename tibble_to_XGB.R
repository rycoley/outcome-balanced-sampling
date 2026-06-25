### RED Suicide XGBoost-Specific Data Preparation
### Transforms prepared datasets into XGBoost-accepted matrices for model 
### fitting.
### Arguments:
###   input_data  Data for model fitting (data.frame or tibble)
###   outcome     Outcome variable (chr string)
###   

tibble_to_XGB <- function(input_data, outcome){
  label_outcome <- input_data %>% pull(`outcome`)
  data_outcome <- input_data %>% select(-all_of(`outcome`)) %>% model.matrix(~0+., data=.)
  return(xgboost::xgb.DMatrix(label = label_outcome, data = data_outcome))
}
