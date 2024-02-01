#--------------------------------LIBRARIES--------------------------------------
library(glmnet)
library(tidyverse)
library(caret)
library(recipes) #for one-hot encoding
library(ROSE)
#--------------------------------FUNCTIONS--------------------------------------
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

#--------------------PRE-PROCESSING AND FEATURE ENGINEERING---------------------

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Load the dataset and pre-process it
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")
bank <- bank[, -c(1, 3, 5, 6, 9, 10, 14, 16, 17, 21, 22, 23)]
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)
# Convert all categorical variables to factors and reorder the levels
bank$Gender <- as.factor(bank$Gender)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Marital_Status <- forcats::fct_relevel(bank$Marital_Status,
                                            "Unknown",
                                            "Single",
                                            "Married",
                                            "Divorced")
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Income_Category <- forcats::fct_relevel(bank$Income_Category,
                                             "Unknown",
                                             "Less than $40K",
                                             "$40K - $60K",
                                             "$60K - $80K",
                                             "$80K - $120K",
                                             "$120K +")
# Override the Total_Trans_Amt variable with its log !!!
bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)
# Standardization (optional) all columns except response and categorical
bank[, -c(1, 2, 3, 4)] <- scale(bank[, -c(1, 2, 3, 4)])

#------------------------------APPLYING ROSE------------------------------------
bank_balanced<- ROSE(Attrition_Flag~.,data=bank,seed = 123)$data

#------------------------------RIDGE MODEL--------------------------------------
cv_penalized_regression(data = bank,k=10,response = "Attrition_Flag",lasso=F)

#------------------------------LASSO MODEL--------------------------------------
cv_penalized_regression(data = bank,k=10,response = "Attrition_Flag",lasso=T)
