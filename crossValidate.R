crossvalidate <- function(data, k, model, dependent, random = FALSE){
  # data is the training set with the ".folds" column
  # k is the number of folds we have
  # model is a string describing a linear regression model formula
  # dependent is a string with the name of the score column we want to predict
  # random is a logical; do we have random effects in the model?
  
  # Initialize empty list for recording performances
  performances <- c()
  
  # One iteration per fold
  for (fold in 1:k){
    
    # Create training set for this iteration
    # Subset all the datapoints where .folds does not match the current fold
    training_set <- data[data$.folds != fold,]
    
    # Create test set for this iteration
    # Subset all the datapoints where .folds matches the current fold
    testing_set <- data[data$.folds == fold,]
    
    ## Train model
    
    # If there is a random effect,
    # use lmer() to train model
    # else use lm()
    
    if (isTRUE(random)){
      
      # Train linear mixed effects model on training set
      model <-  lmer(model, training_set, REML=FALSE)
      
    } else {
      
      # Train linear model on training set
      model <-  lm(model, training_set)
      
    }
    
    ## Test model
    
    # Predict the dependent variable in the testing_set with the trained model
    predicted <- predict(model, testing_set, allow.new.levels=TRUE)
    
    # Get the Root Mean Square Error between the predicted and the observed
    RMSE <- rmse(predicted, testing_set[[dependent]])
    
    # Add the RMSE to the performance list
    performances[fold] <- RMSE
    
    
  }
  
  # Return the mean of the recorded RMSEs
  return(c('RMSE' = mean(performances)))
  
}