# LIBRARIES --------------------------------------------------------------------
library(car)
library(caret)
library(pROC)
library(forcats)

# FUNCTIONS --------------------------------------------------------------------

# Learning function: f'_learn
learn <- function(data) {
  # Logistic regression
  model <- glm(Attrition_Flag ~ .,
               data = data,
               family = binomial(link = "logit"))
  return(model)
}

# Prediction function: f'_predict
predict_prime <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response") > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "response")
  return(predicted_probs)
}

# Function to assess the model later
assess <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response") > 0.5
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == 0) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second(model, data))
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(0, length(actual)))
  dummy_classifier_auc <- auc(dummy_classifier_roc)
  
  # AIC
  aic <- AIC(model)
  
  # BIC
  bic <- BIC(model)
  
  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    auc = auc,
    dummy_classifier_auc = dummy_classifier_auc,
    fpr = fpr,
    fnr = fnr,
    aic = aic,
    bic = bic
  )
  
  # Print results
  cat("----------------------------------------\n")
  print(results$confusion_matrix)
  cat("----------------------------------------\n")
  cat("Accuracy:", round(results$accuracy * 100, 2), "%\n")
  cat("Dummy classifier accuracy:",
      round(results$dummy_classifier_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("AUC:", round(results$auc * 100, 2), "%\n")
  cat("Dummy classifier AUC:",
      round(results$dummy_classifier_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("FPR:", round(results$fpr * 100, 2), "%\n")
  cat("FNR:", round(results$fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("AIC:", results$aic, "\n")
  cat("BIC:", results$bic, "\n")
  cat("----------------------------------------\n")
  
  return(results)
}

# k-fold cross validation function: f'_cv
cv <- function(data, k = 10) {
  # Create k equally size folds
  folds <- createFolds(data$Attrition_Flag, k = k)
  
  # Initialize vectors
  accuracy <- rep(0, k)
  auc <- rep(0, k)
  fpr <- rep(0, k)
  fnr <- rep(0, k)
  aic <- rep(0, k)
  bic <- rep(0, k)
  
  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    # Train the model on the training set
    model <- learn(train)
    
    # Predict on the testing set
    predicted <- predict_prime(model, test)
    predicted_probs <- predict_second(model, test)
    
    # Positive, negatives and false cases
    p <- sum(predicted == 1)
    n <- sum(predicted == 0)
    fp <- sum(predicted == 1 & test$Attrition_Flag == 0)
    fn <- sum(predicted == 0 & test$Attrition_Flag == 1)
    
    # Compute the accuracy, Auc, FPR, FNR, AIC and BIC
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    roc <- roc(test$Attrition_Flag, predicted_probs)
    auc[i] <- auc(roc)
    fpr[i] <- fp / n
    fnr[i] <- fn / p
    aic[i] <- AIC(model)
    bic[i] <- BIC(model)
  }
  
  # Compute the average accuracy, AUC, FPR, FNR, AIC and BIC
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  average_aic <- mean(aic)
  average_bic <- mean(bic)
  
  # Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
  sd_accuracy <- sd(accuracy)
  sd_auc <- sd(auc)
  sd_fpr <- sd(fpr)
  sd_fnr <- sd(fnr)
  sd_aic <- sd(aic)
  sd_bic <- sd(bic)
  
  # Print the average accuracy, AIC and BIC with their standard deviations
  cat("----------------------------------------\n")
  cat("Average accuracy:", round(average_accuracy * 100, 2), "+/-",
      round(sd_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average AUC:", round(average_auc * 100, 2), "+/-",
      round(sd_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average FPR:", round(average_fpr * 100, 2), "+/-",
      round(sd_fpr * 100, 2), "%\n")
  cat("Average FNR:", round(average_fnr * 100, 2), "+/-",
      round(sd_fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average AIC:", average_aic, "+/-", sd_aic, "\n")
  cat("Average BIC:", average_bic, "+/-", sd_bic, "\n")
  cat("----------------------------------------\n")
}
