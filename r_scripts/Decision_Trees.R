#install.packages("caret")
# LOADING AND PREPROCESSING ----------------------------------------------------

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#setwd("D:/Vishal")

# Load the dataset from the datasets/ folder
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")

#bank <- read.csv("D:/Vishal/BankChurners.csv", sep = ",")

# ?????? Remove the last two columns as suggested in the README. Remove the first ID column.
bank <- bank[, -c(1, 22, 23)]



# ?????? Convert the Attrition_Flag column to a binary variable:
# - 0: Existing Customer
# - 1: Attrited Customer
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)

# Convert all categorical variables to factors
bank$Attrition_Flag <- as.factor(bank$Attrition_Flag)
bank$Gender <- as.factor(bank$Gender)
bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)


#Classification tree

#Libraries ---------------------------------------------------------------

library(MASS)
library(rpart)
library(rpart.plot)
library(caret)


#create a classification tree

fit <- rpart(Attrition_Flag ~ ., method="class", data = bank)

#plotcp(fit) # plots error rate by cp=complexity parameter for pruning

rpart.plot(fit, extra=1, digits=4, box.palette="auto")

predictions <- predict(fit, type = "class")

head(predictions)

levels(predictions)
levels(bank$Attrition_Flag)

#Performance indices:

# Calculate accuracy
accuracy <- confusionMatrix(predictions, bank$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision <- confusionMatrix(predictions, bank$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall <- confusionMatrix(predictions, bank$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity <- confusionMatrix(predictions, bank$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score <- confusionMatrix(predictions, bank$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
library(pROC)
roc <- roc(bank$Attrition_Flag, as.numeric(predictions))
auc_roc <- auc(roc)

original_performance <- c(Accuracy = accuracy, Precision = precision, Recall = recall, 
                          Specificity = specificity, F1_Score = f1_score, AUC_ROC = auc_roc)

original_performance

# Now, let's consider the significant variables from glm based on p-value


fit1 <- rpart(Attrition_Flag ~ Gender + Marital_Status + Income_Category + Total_Relationship_Count + 
                Months_Inactive_12_mon + Contacts_Count_12_mon + Total_Revolving_Bal + Total_Trans_Amt + 
                Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, method="class", data = bank)

rpart.plot(fit1, extra=1, digits=4, box.palette="auto")

predictions_rm <- predict(fit1, type = "class")

#Performance indices:

# Calculate accuracy
accuracy_rm <- confusionMatrix(predictions_rm, bank$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_rm <- confusionMatrix(predictions_rm, bank$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_rm <- confusionMatrix(predictions_rm, bank$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_rm <- confusionMatrix(predictions_rm, bank$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_rm <- confusionMatrix(predictions_rm, bank$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
roc_rm <- roc(bank$Attrition_Flag, as.numeric(predictions_rm))
auc_roc_rm <- auc(roc_rm)

rm_performance <- c(Accuracy = accuracy_rm, Precision = precision_rm, Recall = recall_rm, 
                    Specificity = specificity_rm, F1_Score = f1_score_rm, AUC_ROC = auc_roc_rm)

rm_performance


#k-fold cross validation


ctrl <- trainControl(method = "cv",  # Use k-fold cross-validation
                     number = 10)     # Number of folds (e.g., 10-fold)


# Perform k-fold cross-validation
set.seed(123)  # Set seed for reproducibility
cv <- train(Attrition_Flag ~ Gender + Marital_Status + Income_Category + Total_Relationship_Count + 
              Months_Inactive_12_mon + Contacts_Count_12_mon + Total_Revolving_Bal + Total_Trans_Amt + 
              Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, data = bank, method = "rpart", trControl = ctrl)      

# View the cross-validation results
print(cv)

predictions_kfold <- predict(cv, type = "raw")

#Performance indices:

# Calculate accuracy
accuracy_kfold <- confusionMatrix(predictions_kfold, bank$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_kfold <- confusionMatrix(predictions_kfold, bank$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_kfold <- confusionMatrix(predictions_kfold, bank$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_kfold <- confusionMatrix(predictions_kfold, bank$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_kfold <- confusionMatrix(predictions_kfold, bank$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
roc_kfold <- roc(bank$Attrition_Flag, as.numeric(predictions_kfold))
auc_roc_kfold <- auc(roc_kfold)

kfold_performance <- c(Accuracy = accuracy_kfold, Precision = precision_kfold, Recall = recall_kfold, 
                       Specificity = specificity_kfold, F1_Score = f1_score_kfold, AUC_ROC = auc_roc_kfold)

kfold_performance
#Accuracy for reduced model without performing kfold is better. So, next we try tuning a tree of reduced model. 

#Hyperparameter tuning

set.seed(123)
trainIndex <- createDataPartition(bank$Attrition_Flag, p = 0.7, list = FALSE)
train_data <- bank[trainIndex, ]
validation_data <- bank[-trainIndex, ]

# Define a grid of hyperparameters to tune over
cp_grid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))

# Set up the tuning process
ctrl <- trainControl(method = "cv", number = 10)  

# Perform grid search
fit_tune <- train(Attrition_Flag ~ Gender + Marital_Status + Income_Category + Total_Relationship_Count + 
                    Months_Inactive_12_mon + Contacts_Count_12_mon + Total_Revolving_Bal + Total_Trans_Amt + 
                    Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, method = "rpart", data = train_data, trControl = ctrl,              
                  tuneGrid = cp_grid)            
# Evaluate performance
print(fit_tune)

# Choose the best model
best_model <- fit_tune$finalModel

best_model

rpart.plot(best_model, extra=1, digits=4, box.palette="auto")

# Print the summary of the decision tree
summary(best_model)

# Access the split nodes starting from the root
split_nodes <- best_model$splits[, "variable"]
unique_split_nodes <- unique(split_nodes)

split_variables <- unique(best_model$variable)

# Print the unique split nodes
print(unique_split_nodes)



# Retrain the model
final_model <- rpart(Attrition_Flag ~ Gender + Marital_Status + Income_Category + Total_Relationship_Count + 
                       Months_Inactive_12_mon + Contacts_Count_12_mon + Total_Revolving_Bal + Total_Trans_Amt + 
                       Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, method="class", data = bank, cp = best_model$cp)


predictions_tune <- predict(final_model, type = "class")

levels(predictions_tune)
table(predictions_tune)

dim(data.frame(predictions_tune))

d <- data.frame(predictions_tune)

#Performance indices:

# Calculate accuracy
accuracy_tune <- confusionMatrix(predictions_tune, bank$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_tune <- confusionMatrix(predictions_tune, bank$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_tune <- confusionMatrix(predictions_tune, bank$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_tune <- confusionMatrix(predictions_tune, bank$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_tune <- confusionMatrix(predictions_tune, bank$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
roc_tune <- roc(bank$Attrition_Flag, as.numeric(predictions_tune))
auc_roc_tune <- auc(roc_tune)

tune_tree_performance <- c(Accuracy = accuracy_tune, Precision = precision_tune, Recall = recall_tune, 
                           Specificity = specificity_tune, F1_Score = f1_score_tune, AUC_ROC = auc_roc_tune)

tune_tree_performance
