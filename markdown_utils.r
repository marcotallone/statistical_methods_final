# LIBRARIES --------------------------------------------------------------------
options(rgl.useNULL = TRUE) #don't remove this!
library(car)
library(caret)
library(pROC)
library(forcats)
library(adabag)
library(randomForest)
library(MASS)
library(splines)
library(mgcv)
library(glmnet)
library(tidyverse)
library(recipes)
library(ROSE)

# For the plots
library(ggplot2)
library(ggExtra)
library(ggforce)
library(patchwork)
library(RColorBrewer)
library(dplyr)
library(corrr)

# PLOTS ------------------------------------------------------------------------

plot_continuous <- function(dataset, variable, title, xlab, bins_width = 1) {
  p1 <- ggplot(dataset, aes(x = {{ variable }},
                            fill = as.factor(Attrition_Flag))) +
    geom_histogram(binwidth = bins_width, color = "#FFFFFF") +
    labs(title = paste("Histogram of", title), x = xlab, y = "Count") +
    scale_fill_manual(values = c("royalblue", "#FF5733"),
                      name = "Attrition Flag:",
                      labels = c("Existing", "Attrited")) +
    theme(legend.position = c(.85, .85),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          aspect.ratio = 1)

  p2 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
    geom_sina(aes(group = Attrition_Flag), alpha = .25) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p3 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_boxplot(fill = "gray80", alpha = .5,
                 outlier.size = 4, outlier.alpha = .75) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p4 <- ggplot(dataset, aes(x = {{ variable }})) +
    stat_ecdf(geom = "step", color = "royalblue") +
    labs(title = paste("Cumulative Distribution of", title),
         x = xlab, y = "Cumulative Distribution") +
    theme(legend.position = "none", aspect.ratio = 1)

  (p1 / p4 / p2 / p3) + plot_layout(ncol = 2)
}

plot_discrete <- function(dataset, variable, title, xlab) {
  p1 <- ggplot(dataset, aes(x = as.factor({{ variable }}),
                            fill = as.factor(Attrition_Flag))) +
    geom_bar(color = "#FFFFFF") +
    labs(title = paste("Barplot of", title), x = xlab, y = "Count") +
    scale_fill_manual(values = c("royalblue", "#FF5733"),
                      name = "Attrition Flag:",
                      labels = c("Existing", "Attrited")) +
    theme(legend.position = c(.85, .85),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          aspect.ratio = 1)

  p2 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
    geom_sina(aes(group = Attrition_Flag), alpha = .25) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p3 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_boxplot(fill = "gray80", alpha = .5,
                 outlier.size = 4, outlier.alpha = .75) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p4 <- ggplot(dataset, aes(x = {{ variable }})) +
    stat_ecdf(geom = "step", color = "royalblue") +
    labs(title = paste("Cumulative Distribution of", title),
         x = xlab, y = "Cumulative Distribution") +
    theme(legend.position = "none", aspect.ratio = 1)

  (p1 / p4 / p2 / p3) + plot_layout(ncol = 2)
}

plot_categorical <- function(dataset, variable, xlab) {

  # Compute the position of labels
  class_prop <- dataset %>%
    count({{variable}}) %>%
    mutate(prop = n / sum(n) * 100,
           ypos = cumsum(prop) - 0.5 * prop)

  # Create the pie chart
  pie <- ggplot(class_prop, aes(x = "", y = prop, fill = {{variable}})) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "top",
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal", aspect.ratio = 1) +
    # geom_text(aes(y = ypos, label = paste(round(prop, 1), "%")),
    #           color = "white", size = 6) +
    scale_fill_brewer(palette = "Set2",
                      name = xlab)

  box <- ggplot(dataset, aes(x = as.factor({{variable}}),
                             fill = as.factor(Attrition_Flag))) +
    geom_bar(color = "#FFFFFF") +
    labs(x = xlab, y = "Count") +
    scale_fill_manual(values = c("royalblue", "#FF5733"),
                      name = "Attrition Flag:",
                      labels = c("Existing", "Attrited")) +
    theme(legend.position = "top",
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal",
          legend.title = element_text(size = 10),
          aspect.ratio = 1)

  (pie / box) + plot_layout(ncol = 2)
}

# LOGISTIC REGRESSION ----------------------------------------------------------
# LEARN / PREDICT FUNCTIONS ----------------------------------------------------

# learn_logistic function: f'_learn_logistic
learn_logistic <- function(data) {
  # Logistic regression
  model <- glm(Attrition_Flag ~ .,
               data = data,
               family = binomial(link = "logit"))
  return(model)
}

# Prediction function: f'_predict
predict_prime_logistic <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response") > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second_logistic <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "response")
  return(predicted_probs)
}

# ASSESSMENT FUNCTION ----------------------------------------------------------

# Function to assess the model later
assess_logistic <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response") > 0.5
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")

  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

  # Dummy classifier accuracy
  maj_class <- names(table(data$Attrition_Flag))[which.max(table(data$Attrition_Flag))]
  dummy_classifier_accuracy <- sum(actual == maj_class) / length(actual)

  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])

  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])

  # ROC curve and AUC
  roc <- roc(actual, predict_second_logistic(model, data))
  auc <- auc(roc)

  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual,
                              rep(as.numeric(maj_class), length(actual)))
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
cv_logistic <- function(data, k = 10) {
  # Create k equally size folds
  set.seed(0)
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
    model <- learn_logistic(train)

    # Predict on the testing set
    predicted <- predict_prime_logistic(model, test)
    predicted_probs <- predict_second_logistic(model, test)

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

# PENALIZED REGRESSION ----------------------------------------------------------
# LEARN / PREDICT FUNCTIONS ----------------------------------------------------
learn_penalized_regression<- function(data, response="Attrition_Flag",lasso=FALSE) {
  # Separate X and y
  # X needs to one-hot-encode factor vars...
  X <- makeX(data %>% select(!response))
  y <- data %>% select(response) %>% as.matrix()
  # Fit ridge regression model
  model <- cv.glmnet(X, y, family = "binomial", alpha = as.numeric(lasso))
  
  return(model)
}
# Prediction function: f'_predict
predict_prime_penalized_regression <- function(model, data, response="Attrition_Flag",tau = 0.5) {
  X <- makeX(data %>% select(!response))
  # Predictions
  predicted <- predict(model, newx = X, type = "response",s = "lambda.min") > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second_penalized_regression <- function(model, data,response="Attrition_Flag") {
  # Predicted probabilities
  X <- makeX(data %>% select(!response))
  predicted_probs <- predict(model, newx = X, type = "response",s = "lambda.min")
  return(predicted_probs)
}
# ASSESSMENT FUNCTION ----------------------------------------------------------
cv_penalized_regression <- function(data, k = 10,response="Attrition_Flag",lasso=FALSE) {
  # Create k equally size folds
  set.seed(0)
  folds <- createFolds(data[,response], k = k)
  
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
    model <- learn_penalized_regression(data = train,response=response,lasso)
    
    # Predict on the testing set
    predicted <- as.numeric(predict_prime_penalized_regression(model,test,response))
    predicted_probs <- as.numeric(predict_second_penalized_regression(model,test,response))
    
    # Positive, negatives and false cases
    p <- sum(predicted == 1)
    n <- sum(predicted == 0)
    fp <- sum(predicted == 1 & test[,response] == 0)
    fn <- sum(predicted == 0 & test[,response] == 1)
    
    # Compute the accuracy, Auc, FPR, FNR, AIC and BIC
    accuracy[i] <- sum(predicted == test[,response]) / nrow(test)
    roc <- roc(test[,response], predicted_probs)
    auc[i] <- auc(roc)
    fpr[i] <- fp / n
    fnr[i] <- fn / p
  }
  
  # Compute the average accuracy, AUC, FPR, FNR, AIC and BIC
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  
  # Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
  sd_accuracy <- sd(accuracy)
  sd_auc <- sd(auc)
  sd_fpr <- sd(fpr)
  sd_fnr <- sd(fnr)
  
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
}


# -----------------------------------------------------------------------------
#GAM/SPLINES------------------------------------------------------------------
# LEARN / PREDICT FUNCTIONS ---------------------------------------------------
learn_gam <- function(data) {
  #spline with gam
  model<-gam(Attrition_Flag ~ s(Customer_Age)
             +Gender
             +Marital_Status 
             +Total_Relationship_Count
             +Months_Inactive_12_mon
             +Contacts_Count_12_mon
             +s(Total_Revolving_Bal)
             +s(Total_Amt_Chng_Q4_Q1)
             +s(Total_Trans_Amt)
             +s(Total_Trans_Ct)
             +s(Total_Ct_Chng_Q4_Q1)
             ,family = binomial(link = "logit"), data=data)
  return(model)
}

# Prediction function: f'_predict
predict_prime_gam <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response") > tau
  return(predicted)
}

predict_second_gam <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "response")
  return(predicted_probs)
}
#ASSESSMENT-------------

# Function to assess the model later
assess_gam <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response") > 0.5
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  maj_class= names(table(data$Attrition_Flag))[which.max(table(data$Attrition_Flag))]
  dummy_classifier_accuracy <- sum(actual == maj_class) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second_gam(model, data))
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(as.numeric(maj_class), length(actual)))
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
cv_gam <- function(data, k = 10) {
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
    

    model <- learn_gam(train)

    
    # Predict on the testing set
    predicted <- predict_prime_gam(model, test)
    predicted_probs <- predict_second_gam(model, test)
    
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



# ENSEMBLE LEARNING -----------------------------------------------------------
# LEARN / PREDICT FUNCTIONS ---------------------------------------------------

# ADA BOOST --------------------------------------------------------------------

learn_boost <- function(data) {
  model <- boosting(Attrition_Flag ~ .,
                    data = data,
                    boos=TRUE)
  return(model)
}

# Prediction function: f'_predict
predict_prime_boost <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response")$prob[,2] > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second_boost <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "response")$prob[,2]
  return(predicted_probs)
}

# Function to assess the model later
assess_boost <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response")$prob[,2] > 0.5
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == FALSE) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second_boost(model, data))
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(0, length(actual)))
  dummy_classifier_auc <- auc(dummy_classifier_roc)
  
  # Extract the variable selection information for each boosting iteration
  var_selection <- model$importance
  
  # Sort the variable importance in descending order
  sorted_variable_importance <- sort(var_selection, decreasing = TRUE)
  
  # Convert variable importance to a data frame
  variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                       Mean_Gini_Decrease = sorted_variable_importance,
                                       row.names = NULL)
  
  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    auc = auc,
    dummy_classifier_auc = dummy_classifier_auc,
    fpr = fpr,
    fnr = fnr,
    variable_importance = variable_importance_df
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
  cat("Variable importance:\n")
  print(results$variable_importance)
  cat("----------------------------------------\n")
  #cat("AIC:", results$aic, "\n")
  #cat("BIC:", results$bic, "\n")
  #cat("----------------------------------------\n")
  
  return(results)
}

# k-fold cross validation function: f'_cv
cv_boost <- function(data, k = 10) {
  # Create k equally sized folds
  set.seed(0)
  folds <- createFolds(data$Attrition_Flag, k = k)
  
  # Initialize lists to store evaluation metrics and variable importance
  accuracy <- vector("numeric", k)
  auc <- vector("numeric", k)
  fpr <- vector("numeric", k)
  fnr <- vector("numeric", k)
  mean_decrease_list <- vector("list", k)
  
  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    # Train the model on the training set
    model <- learn_boost(train)
    
    # Predict on the testing set
    predicted <- predict_prime_boost(model, test)
    predicted_probs <- predict_second_boost(model, test)
    
    # Compute evaluation metrics
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    roc <- roc(test$Attrition_Flag, predicted_probs)
    auc[i] <- auc(roc)
    fp <- sum(predicted == TRUE & test$Attrition_Flag == FALSE)
    fn <- sum(predicted == FALSE & test$Attrition_Flag == TRUE)
    fpr[i] <- fp / sum(test$Attrition_Flag == FALSE)
    fnr[i] <- fn / sum(test$Attrition_Flag == TRUE)
    
    # Extract the variable selection information for each boosting iteration
    var_selection <- model$importance
    
    # Sort the variable importance in descending order
    sorted_variable_importance <- sort(var_selection, decreasing = TRUE)
    
    # Store the sorted variable importance in the list
    mean_decrease_list[[i]] <- sorted_variable_importance
  }
  
  # Compute average evaluation metrics
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  
  # Compute average variable importance
  # Convert the list to a matrix to compute the average mean decrease
  mean_decrease_matrix <- do.call(rbind, mean_decrease_list)
  
  # Compute the average mean decrease across all iterations
  average_mean_decrease <- colMeans(mean_decrease_matrix)
  sd_mean_decrease <- apply(mean_decrease_matrix, 2, sd)
  
  
  sorted_variable_importance <- sort(average_mean_decrease, decreasing = TRUE)
  
  # Convert variable importance to a data frame
  variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                       Mean_Gini_Decrease = sorted_variable_importance,
                                       Std_Dev = sd_mean_decrease,
                                       row.names = NULL)
  
  # Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
  sd_accuracy <- sd(accuracy)
  sd_auc <- sd(auc)
  sd_fpr <- sd(fpr)
  sd_fnr <- sd(fnr)
  
  # Print average evaluation metrics and variable importance
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
  cat("Average variable importance ranking:\n")
  print(variable_importance_df)
  cat("----------------------------------------\n")
  
  results <- list(
    accuracy = average_accuracy,
    auc = average_auc,
    fpr = average_fpr,
    fnr = average_fnr,
    variable_importance = variable_importance_df
  )
  
  return(results)
}

# RANDOM FOREST ----------------------------------------------------------------
# Learning function
learn_rf <- function(data) {
  # Logistic regression
  model <- randomForest(Attrition_Flag ~ ., data = data, ntree = 500,
                        seed=123, importance = TRUE)
  return(model)
}

# Prediction function: f'_predict
predict_prime_rf <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "prob")[, 2] > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second_rf <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "prob")[, 2]
  return(predicted_probs)
}

assess_rf <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response")
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == FALSE) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second_rf(model, data))
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(0, length(actual)))
  dummy_classifier_auc <- auc(dummy_classifier_roc)
  
  # Extract variable importance
  variable_importance <- importance(model, type = 2)
  
  # Create a data frame for variable names and importance values
  variable_importance_df <- data.frame(
    Variable = row.names(variable_importance),
    Mean_Gini_Decrease = as.numeric(variable_importance),
    row.names = NULL
  )
  
  # Sort the data frame by Importance in descending order
  variable_importance_df <- variable_importance_df[order(-variable_importance_df$Mean_Gini_Decrease), ]
  
  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    auc = auc,
    dummy_classifier_auc = dummy_classifier_auc,
    fpr = fpr,
    fnr = fnr,
    variable_importance = variable_importance_df
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
  cat("Variable importance:\n")
  print(results$variable_importance)
  cat("----------------------------------------\n")
  
  
  return(results)
}

# k-fold cross validation function: f'_cv
cv_rf <- function(data, k = 10) {
  # Create k equally size folds
  folds <- createFolds(data$Attrition_Flag, k = k)
  
  # Initialize vectors
  accuracy <- rep(0, k)
  auc <- rep(0, k)
  fpr <- rep(0, k)
  fnr <- rep(0, k)
  mean_decrease_list <- vector("list", k)
  
  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    # Train the model on the training set
    model <- learn_rf(train)
    
    # Predict on the testing set
    predicted <- predict_prime_rf(model, test)
    predicted_probs <- predict_second_rf(model, test)
    
    # Positive, negatives and false cases
    p <- sum(predicted == TRUE)
    n <- sum(predicted == FALSE)
    fp <- sum(predicted == TRUE & test$Attrition_Flag == FALSE)
    fn <- sum(predicted == FALSE & test$Attrition_Flag == TRUE)
    
    # Compute the accuracy, Auc, FPR, FNR, AIC and BIC
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    roc <- roc(test$Attrition_Flag, predicted_probs)
    auc[i] <- auc(roc)
    fpr[i] <- fp / n
    fnr[i] <- fn / p
    mean_decrease_list[[i]] <- importance(model, type = 2)
  }
  
  # Compute the average accuracy, AUC, FPR, FNR, AIC and BIC
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  
  # Compute average variable importance
  # Convert the list to a matrix to compute the average mean decrease
  mean_decrease_matrix <- do.call(cbind, mean_decrease_list)
  
  # Compute the average mean decrease across all iterations
  average_mean_decrease <- rowMeans(mean_decrease_matrix)
  sd_mean_decrease <- apply(mean_decrease_matrix, 1, sd)
  
  sorted_variable_importance <- sort(average_mean_decrease, decreasing = TRUE)
  
  # Convert variable importance to a data frame
  variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                       Mean_Gini_Decrease = sorted_variable_importance,
                                       Std_Dev = sd_mean_decrease,
                                       row.names = NULL)
  
  # Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
  sd_accuracy <- sd(accuracy)
  sd_auc <- sd(auc)
  sd_fpr <- sd(fpr)
  sd_fnr <- sd(fnr)
  
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
  cat("Average variable importance ranking:\n")
  print(variable_importance_df)
  cat("----------------------------------------\n")
  
  results <- list(
    accuracy = average_accuracy,
    auc = average_auc,
    fpr = average_fpr,
    fnr = average_fnr,
    variable_importance = variable_importance_df
  )
  
  return(results)
}

# -----------------------------------------------------------------------------
