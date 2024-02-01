library(MASS)
library(splines)
library(pROC)
library(mgcv)
library(car)
library(caret)
library(forcats)
##PREPROCESSING ---------------------------

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset and pre-process it
bank <- read.csv("../../datasets/BankChurners.csv", sep = ",")
# ⚠️ Remove the last two columns as suggested in the README
bank <- bank[, -c(1,22, 23)]

# ⚠️ Convert the Attrition_Flag column to a binary variable:
# - FALSE: Existing Customer
# - TRUE: Attrited Customer
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)

# Convert all categorical variables to factors
bank$Gender <- as.factor(bank$Gender)

bank$Income_Category <- fct_collapse(bank$Income_Category,
                                     "Less than 120K" = c("Unknown",
                                                          "Less than $40K",
                                                          "$40K - $60K",
                                                          "$60K - $80K",
                                                          "$80K - $120K"),
                                     "More than 120K" = c("$120K +"))

# Chnanging the levels of Marital_Status in either married or not married
bank$Marital_Status <- fct_collapse(bank$Marital_Status,
                                    "Married" = c("Married"),
                                    "Not Married" = c("Divorced",
                                                      "Single",
                                                      "Unknown"))
# Converting Months_Inactive_12_mon to a factor
bank$Months_Inactive_12_mon <- as.factor(bank$Months_Inactive_12_mon)
levels(bank$Months_Inactive_12_mon) <- c("0", "1", "2", "3", "4", "5", "6+")

# Joining together the levels after 4 months
bank$Months_Inactive_12_mon <- fct_collapse(bank$Months_Inactive_12_mon,
                                            "4+" = c("4", "5", "6+"))

bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)
bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)

#-------------------------------------------------------------------------------
#                               APPLYING ROSE
bank_balanced<- ROSE(Attrition_Flag~.,data=bank,seed = 123)$data
#-------------------------------------------------------------------------------

#FUNCTIONS

# Learning function: f'_learn
learn <- function(data) {
  #spline with gam
            model<-gam(Attrition_Flag ~ s(Customer_Age)+Gender
                       +Dependent_count
            +Education_Level+Marital_Status
            +Income_Category+Card_Category+Months_on_book 
            +Total_Relationship_Count+Months_Inactive_12_mon
            +Contacts_Count_12_mon+Credit_Limit+s(Total_Revolving_Bal)
            +Avg_Open_To_Buy+s(Total_Amt_Chng_Q4_Q1)
            +s(Total_Trans_Amt)+s(Total_Trans_Ct)+s(Total_Ct_Chng_Q4_Q1)
            +s(Avg_Utilization_Ratio),
            family = binomial(link = "logit"),data=data)
            return(model)
}
# Learning function: f'_learn using the dataset as: bank <- bank[, -c(1, 3, 5, 6, 9, 10, 14, 16, 17, 21, 22, 23)]
learn_less_vars <- function(data) {
  #spline with gam
  model<-gam(Attrition_Flag ~ Gender
             +Marital_Status
             +Income_Category
             +Total_Relationship_Count+Months_Inactive_12_mon
             +Contacts_Count_12_mon+s(Total_Revolving_Bal)
             +s(Total_Trans_Amt)+s(Total_Trans_Ct)+s(Total_Ct_Chng_Q4_Q1),
             family = binomial(link = "logit"),data=data)
  return(model)
}


# Prediction function: f'_predict
predict_prime <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response") > tau
  return(predicted)
}


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
  maj_class= names(table(data$Attrition_Flag))[which.max(table(data$Attrition_Flag))]
  dummy_classifier_accuracy <- sum(actual == maj_class) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second(model, data))
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
cv <- function(data, k = 10, flag) {
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
    if (flag==TRUE){
      model <- learn(train)
      cat("model with all variables")}
    if (flag==FALSE){
      model<-learn_less_vars(train)
      cat("model with less variables")}
    
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

##MODELS---------------------------------------------------------------

#considering edf in the summary I change as additive the variables that has edf<2
gamfit<-learn(bank_balanced)
summary(gamfit)
#considering less variables
gamfit_less_vars<-learn_less_vars(bank_balanced)
summary(gamfit_less_vars)
##ASSESSING--------------------------------------------------------

cat("Results on whole dataset (using 'more' vars):\n")
results <- assess(gamfit, bank_balanced)
results_less_vars  <-assess(gamfit_less_vars,bank_balanced)

anova(gamfit, test = "Chisq")
anova(gamfit_less_vars, test="Chisq")
cat("Results on 10-fold cross validation:\n")
cv(bank_balanced,flag =  TRUE)
cv(bank_balanced, flag = FALSE)

