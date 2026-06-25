## Partitions the indices from 1:n into k groups of [nearly] equal size.
### Group assignments are random

partition_indices <- function(n, k){
  browser()
  perm_inds <- sample(n,replace = FALSE)
  
  base_size <- n %/% k
  remainder <- n %% k
  sizes <- rep(base_size, k) + c(rep(1, remainder), rep(0, k - remainder))
  ends <- cumsum(sizes)
  starts <- c(1, ends[-k] + 1)
  return(lapply(1:k, function(i) perm_inds[starts[i]:ends[i]]))
}

## Takes feature and outcome data, and splits it into case and control data
### This is actually a bit goofy as I don't actually need to store the y data! (it's just a vector of 1s and a vector of 0s)

separate.data <- function(X,y){
  ind_cases <- which(y == 1)
  ind_controls <- which(y == 0)
  
  X_cases <- X[ind_cases,]
  X_controls <- X[ind_controls,]
  
  y_cases <- y[ind_cases,,drop=F]
  y_controls <- y[ind_controls,,drop=F]
  
  return(list(X_cases = X_cases,
              X_controls = X_controls,
              y_cases = y_cases,
              y_controls = y_controls))
}

### Takes a vector of indices for controls and a vector of indices for cases
#### and creates a feature matrix (which includes controls, and cases corresponding to those indices
#### and forms outcome vector of 0s and 1s

form.mini.batch <- function(inds_use_controls, inds_use_cases, dat.sep){
  browser()
  x_batch <- torch_tensor(rbind(dat.sep$X_controls[inds_use_controls,], dat.sep$X_cases[inds_use_cases,]))
  y_batch <- torch_tensor(rbind(dat.sep$y_controls[inds_use_controls,,drop=F], dat.sep$y_cases[inds_use_cases,,drop=F]),
                          dtype = torch_float())
  return(list(x_vals = x_batch, y_vals = y_batch))
}

### 


fit_nn = function(train, test, valid, test_race, train_race, 
                  valid_race, n_epochs, batch_size=NULL, lr, w_decay = 0.0001) {
  
  X_train = torch_tensor(as.matrix(train[,-1]))
  y_train = torch_tensor(as.matrix(train[, "EVENT90", drop = F]),
                         dtype = torch_float())
  
  X_test = torch_tensor(as.matrix(test[,-1]))
  y_test <- torch_tensor(as.matrix(test[, "EVENT90", drop = F]),
                         dtype = torch_float())
  X_valid = torch_tensor(as.matrix(valid[,-1]))
  y_valid <- torch_tensor(as.matrix(valid[, "EVENT90", drop = F]),
                          dtype = torch_float())
  
  model = nn_sequential(
    nn_linear(in_features = ncol(X_train), out_features = 16),
    nn_relu(),
    nn_linear(in_features = 16, out_features = 16),
    nn_relu(),
    nn_linear(in_features = 16, out_features = 16),
    nn_relu(),
    nn_linear(in_features = 16, out_features = 1),
    nn_sigmoid()
  )
  
  num_data_points <- X_train$size(1)
  num_batches <- floor(num_data_points/batch_size)
  
  optimizer = optim_adam(model$parameters, weight_decay = w_decay, amsgrad = TRUE)
  modelruntime = system.time({ 

    # Loop for model fit over epochs.
    model_loss = NULL
    auc_test_by_epoch = NULL
    auc_train_by_epoch = NULL
    for(epoch in 1L:n_epochs) {
      begin = Sys.time()
      # Permute data
      permute <- torch_randperm(num_data_points) + 1L
      x <- X_train[permute]
      y <- y_train[permute]
      
      # Manually loop through the batches
      for(batch_idx in 1L:num_batches){
        
        # Index for walking through batches.
        index <- (batch_size*(batch_idx-1L) + 1L):(batch_idx*batch_size)
        
        # Forward pass
        optimizer$zero_grad()
        
        # Predictions.
        output <- model(x[index])
        
        # Calculate loss.
        loss <- nnf_binary_cross_entropy(output, y[index])
        
        # Backward pass.
        loss$backward()
        
        # Update parameters.
        optimizer$step()
        
      }
      
      # Calculate an store loss.
      model_loss <- c(model_loss, loss$item())
      
      # Store parameters from each epoch.
      # model_params = cbind(model_params, do.call(c, sapply(model$parameters, as_array)))
      
      # Predictions
      preds_test = as_array(model(X_test))
      preds_train = as_array(model(X_train))
      preds_val = as_array(model(X_valid))
      auc_test_by_epoch = c(auc_test_by_epoch, Metrics::auc(as_array(y_test), as_array(model(X_test)))) 
      auc_train_by_epoch = c(auc_train_by_epoch,  Metrics::auc(as_array(y_train), as_array(model(X_train))))
    }
  })
  
  ## Output Packaging
  return(list("preds_test_sample" = preds_test,
              "preds_train_sample" = preds_train,
              "preds_val_sample" = preds_val,
              "test_x" = as.matrix(test[,-1]),
              "test_y" = as.matrix(test[, "EVENT90", drop = F]),
              "train_x" = as.matrix(train[,-1]),
              "train_y" = as.matrix(train[, "EVENT90", drop = F]),
              "valid_x" = as.matrix(valid[,-1]),
              "valid_y" = as.matrix(valid[, "EVENT90", drop = F]),
              "race_test" = test_race,
              "race_train" = train_race,
              "valid_race" = valid_race))
}

fit_nn_mini = function(train, test, valid, test_race, train_race, 
                  valid_race, n_epochs, mini_n_controls=NULL, mini_n_cases=NULL, lr, w_decay = 0.0001) {

  X = as.matrix(train[,-1])
  y = as.matrix(train[, "EVENT90", drop = F])
  X_train = torch_tensor(X)
  y_train = torch_tensor(y,
                         dtype = torch_float())
  
  X_test = torch_tensor(as.matrix(test[,-1]))
  y_test <- torch_tensor(as.matrix(test[, "EVENT90", drop = F]),
                         dtype = torch_float())
  X_valid = torch_tensor(as.matrix(valid[,-1]))
  y_valid <- torch_tensor(as.matrix(valid[, "EVENT90", drop = F]),
                          dtype = torch_float())
  browser()
  ## Separates the data into cases and controls
  dat.sep.train = separate.data(X,y)
  # dat.sep.test = separate.data(X_test, y_test)
  
  num_batches_controls = floor(nrow(dat.sep.train$X_controls)/mini_n_controls) # number of groups to partition controls into
  num_batches_cases = floor(nrow(dat.sep.train$X_cases)/mini_n_cases) # number of groups to partition cases into

  model = nn_sequential(
    nn_linear(in_features = ncol(X_train), out_features = 16),
    nn_relu(),
    nn_linear(in_features = 16, out_features = 16),
    nn_relu(),
    nn_linear(in_features = 16, out_features = 16),
    nn_relu(),
    nn_linear(in_features = 16, out_features = 1),
    nn_sigmoid()
  )
  
  optimizer = optim_adam(model$parameters, weight_decay = w_decay, amsgrad = TRUE)
  
  modelruntime = system.time({ 
    
    # Loop for model fit over epochs. 
    for (epoch in 1:n_epochs) {
      
      model$train()
      
      mini_batch_labels_controls <- partition_indices(nrow(dat.sep.train$X_controls), num_batches_controls)
      mini_batch_labels_cases <- partition_indices(nrow(dat.sep.train$X_cases), num_batches_cases)
      
      for(i in 1:num_batches_controls){

        ## Grabbing the appropriate batch
        cases_i = ((i-1) %% num_batches_cases) + 1
        mini_batch <- form.mini.batch(mini_batch_labels_controls[[i]],
                                      mini_batch_labels_cases[[cases_i]],
                                      dat.sep.train)
        
        # Forward pass
        optimizer$zero_grad()
        # Predictions
        output <- model(mini_batch$x_vals)
        # Calculate loss
        loss <- nnf_binary_cross_entropy(output, mini_batch$y_vals)
        # Backward pass
        loss$backward()
        # Update parameters
        optimizer$step()
        
      }
    }
    # Predictions
    preds_test = as_array(model(X_test))
    preds_train = as_array(model(X_train))
    preds_val = as_array(model(X_valid))
  })

  ## Output Packaging
  return(list("preds_test_sample" = preds_test,
              "preds_train_sample" = preds_train,
              "preds_val_sample" = preds_val,
              "test_x" = as.matrix(test[,-1]),
              "test_y" = as.matrix(test[, "EVENT90", drop = F]),
              "train_x" = as.matrix(train[,-1]),
              "train_y" = as.matrix(train[, "EVENT90", drop = F]),
              "valid_x" = as.matrix(valid[,-1]),
              "valid_y" = as.matrix(valid[, "EVENT90", drop = F]),
              "race_test" = test_race,
              "race_train" = train_race,
              "valid_race" = valid_race))
}

