#install.packages("caret")
# LOADING AND PREPROCESSING ----------------------------------------------------
print(dirname(rstudioapi::getSourceEditorContext()$path))
# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#setwd("D:/Vishal")

# Load the dataset from the datasets/ folder
bank <- read.csv("./datasets/BankChurners.csv", sep = ",")

#Remove the last two columns as suggested in the README. Remove the first ID column.
bank <- bank[, -c(1, 22, 23)]

# Convert the Attrition_Flag column to a binary variable:
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
library(pROC)
library(car) # for checking vif for multicollinearity. 
library(ggplot2)

# Perform partition on entire data

set.seed(123)
trainIndex <- createDataPartition(bank$Attrition_Flag, p = 0.8, list = FALSE)
train_data <- bank[trainIndex, ]
validation_data <- bank[-trainIndex, ]

# Classification tree for dummy classifier

# Dummy classifier: Predict the majority class for all instances
majority_class <- levels(train_data$Attrition_Flag)[which.max(table(train_data$Attrition_Flag))]
dummy_predictions <- rep(majority_class, nrow(train_data))


# Build and print the confusion matrix
conf_matrix <- table(train_data$Attrition_Flag, dummy_predictions)
print(conf_matrix)

# Plot the dummy tree (for visualization purposes)
dummy_tree <- rpart(Attrition_Flag ~ dummy_predictions, data = train_data, method = "class")
rpart.plot(dummy_tree, extra=1, digits=4, box.palette="auto")


#create a classification tree considering all predictors and response = Attrition_Flag
#Full tree

fit <- rpart(Attrition_Flag ~ ., method="class", data = train_data)

rpart.plot(fit, extra=1, digits=4, box.palette="auto")

predictions <- predict(fit, type = "class", newdata = validation_data)

head(predictions)

levels(predictions)
levels(bank$Attrition_Flag)

#Performance indices:

# Calculate accuracy
accuracy <- confusionMatrix(predictions, validation_data$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision <- confusionMatrix(predictions, validation_data$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall <- confusionMatrix(predictions, validation_data$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity <- confusionMatrix(predictions, validation_data$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score <- confusionMatrix(predictions, validation_data$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC

roc <- roc(validation_data$Attrition_Flag, as.numeric(predictions))
auc_roc <- auc(roc)

original_performance <- c(Accuracy = accuracy, Precision = precision, Recall = recall, 
                          Specificity = specificity, F1_Score = f1_score, AUC_ROC = auc_roc)

original_performance

# Now, let's consider the logistic regression to decide the significant variables from glm based on p-values

#LR model to select significant variables and then use those variables for classification tree

lr1 <- glm(Attrition_Flag~., data = bank, family = binomial)
summary(lr1)

# Summary shows NA values also. This is case when alias are present. Let's check alias. 
alias(lr1)

# The variable "Avg_Open_To_But" is alias. We will remove this and run LR. 

bank_no_alias <- bank[, -15] # creating data removing alias for running logistic regression. 

lr2 <- glm(Attrition_Flag~., data = bank_no_alias, family = binomial)
summary(lr2)

#From the summary of lr2 model, the significant variables are:
#Gender, Dependent_count,  Marital_Status,  Income_Category, Card_Category, Total_Relationship_Count,
#Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit,Total_Revolving_Bal, 
#Total_Amt_Chng_Q4_Q1, Total_Trans_Amt, Total_Trans_Ct, Total_Ct_Chng_Q4_Q1

## check multicollinearity

# Check VIF for the new model

vif_values <- car::vif(lr2)

print(vif_values)

# There doesn't seems to be multicollinearity issue. 

# clasification tree - reduced model, selected from lr2 model

train_data <- train_data[,c("Attrition_Flag", "Gender", "Dependent_count", "Marital_Status", "Income_Category", "Card_Category", 
                        "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", 
                        "Total_Revolving_Bal", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1")]

validation_data <- validation_data[,c("Attrition_Flag", "Gender", "Dependent_count", "Marital_Status", "Income_Category", "Card_Category", 
                            "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", 
                            "Total_Revolving_Bal", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1")]


fit1 <- rpart(Attrition_Flag ~ Gender + Dependent_count + Marital_Status + Income_Category + Card_Category + Total_Relationship_Count + 
               Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
                Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, method="class", data = train_data)
plotcp(fit1)

rpart.plot(fit1, extra=1, digits=4, box.palette="auto")

predictions_rm <- predict(fit1, type = "class", newdata = validation_data)

#Performance indices:

# Calculate accuracy
accuracy_rm <- confusionMatrix(predictions_rm, validation_data$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_rm <- confusionMatrix(predictions_rm, validation_data$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_rm <- confusionMatrix(predictions_rm, validation_data$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_rm <- confusionMatrix(predictions_rm, validation_data$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_rm <- confusionMatrix(predictions_rm, validation_data$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
roc_rm <- roc(validation_data$Attrition_Flag, as.numeric(predictions_rm))
auc_roc_rm <- auc(roc_rm)

rm_performance <- c(Accuracy = accuracy_rm, Precision = precision_rm, Recall = recall_rm, 
                          Specificity = specificity_rm, F1_Score = f1_score_rm, AUC_ROC = auc_roc_rm)

rm_performance


#k-fold cross validation

ctrl <- trainControl(method = "cv",  # Use k-fold cross-validation
                     number = 10)     # Number of folds (e.g., 10-fold)


# Perform k-fold cross-validation
set.seed(123)
cv <- train(Attrition_Flag ~ Gender + Dependent_count + Marital_Status + Income_Category + Card_Category + Total_Relationship_Count + 
              Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
              Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, data = train_data, method = "rpart", trControl = ctrl)      

# View the cross-validation results
print(cv)

predictions_kfold <- predict(cv, type = "raw", newdata = validation_data)

#Performance indices:

# Calculate accuracy
accuracy_kfold <- confusionMatrix(predictions_kfold, validation_data$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_kfold <- confusionMatrix(predictions_kfold, validation_data$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_kfold <- confusionMatrix(predictions_kfold, validation_data$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_kfold <- confusionMatrix(predictions_kfold, validation_data$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_kfold <- confusionMatrix(predictions_kfold, validation_data$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
roc_kfold <- roc(validation_data$Attrition_Flag, as.numeric(predictions_kfold))
auc_roc_kfold <- auc(roc_kfold)

kfold_performance <- c(Accuracy = accuracy_kfold, Precision = precision_kfold, Recall = recall_kfold, 
                    Specificity = specificity_kfold, F1_Score = f1_score_kfold, AUC_ROC = auc_roc_kfold)

kfold_performance

#Accuracy for reduced model fit2 without performing kfold is better. So, next we try tuning a tree of reduced model. 

#Hyperparameter tuning

# partition the data

# Define parameters for tuning
ctrl <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0.011)



# Perform grid search
fit_tune <- rpart(Attrition_Flag ~ Gender + Dependent_count + Marital_Status + Income_Category + Card_Category + Total_Relationship_Count + 
                    Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
                    Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, method = "class", data = train_data, control = ctrl)
# Evaluate performance
print(fit_tune)

rpart.plot(fit_tune, extra=1, digits=4, box.palette="auto")

# print summary of best model
#summary(best_model)

# Access the split nodes starting from the root
#split_nodes <- best_model$splits[, "variable"]
#unique_split_nodes <- unique(split_nodes)
# Print the unique split nodes
#print(unique_split_nodes)



#split_variables <- unique(best_model$variable)


predictions_tune <- predict(fit_tune, newdata = validation_data, type = "class")

levels(predictions_tune)
table(predictions_tune)

dim(data.frame(predictions_tune))

#Performance indices:

# Calculate accuracy
accuracy_tune <- confusionMatrix(predictions_tune, validation_data$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_tune <- confusionMatrix(predictions_tune, validation_data$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_tune <- confusionMatrix(predictions_tune, validation_data$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_tune <- confusionMatrix(predictions_tune, validation_data$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_tune <- confusionMatrix(predictions_tune, validation_data$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC
roc_tune <- roc(validation_data$Attrition_Flag, as.numeric(predictions_tune))
auc_roc_tune <- auc(roc_tune)

tune_tree_performance <- c(Accuracy = accuracy_tune, Precision = precision_tune, Recall = recall_tune, 
                       Specificity = specificity_tune, F1_Score = f1_score_tune, AUC_ROC = auc_roc_tune)

tune_tree_performance


# Checking balance in response variable:

table(bank$Attrition_Flag)

#There is high imbalance in data. Applying ROSE() to get balanced data

# cat("Attrited customers (rare class): ",sum(bank_logistic$Attrition_Flag==1))
# cat("Existing customers (maj class): ",sum(bank_logistic$Attrition_Flag==0))
# cat("Proportion of attrited:",
#     sum(bank_logistic$Attrition_Flag==1)/sum(table(bank_logistic$Attrition_Flag))*100,"%")

install.packages("ROSE")
library(ROSE)
train_balanced<- ROSE(Attrition_Flag~., data=train_data, seed = 123)$data

table(train_balanced$Attrition_Flag)


#Now we can run a classification tree model on the new balanced dataset:

ROSE_tree_fit <- rpart(Attrition_Flag ~ ., method="class", data = train_balanced)

rpart.plot(ROSE_tree_fit, extra=1, digits=4, box.palette="auto")

predictions_ROSE <- predict(ROSE_tree_fit, type = "class", newdata= validation_data)

#Performance indices:

# Calculate accuracy
accuracy_ROSE <- confusionMatrix(predictions_ROSE, validation_data$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_ROSE <- confusionMatrix(predictions_ROSE, validation_data$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_ROSE <- confusionMatrix(predictions_ROSE, validation_data$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_ROSE <- confusionMatrix(predictions_ROSE, validation_data$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_ROSE <- confusionMatrix(predictions_ROSE, validation_data$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC

roc_ROSE <- roc(validation_data$Attrition_Flag, as.numeric(predictions_ROSE))
auc_roc_ROSE <- auc(roc)

ROSE_performance <- c(Accuracy = accuracy_ROSE, Precision = precision_ROSE, Recall = recall_ROSE, 
                          Specificity = specificity_ROSE, F1_Score = f1_score_ROSE, AUC_ROC = auc_roc_ROSE)

ROSE_performance


# plotting of performance indices for different trees methods tried

all_trees <- data.frame(
  Method = rep(c("Full_tree","Reduced_tree","kfold_tree","Tuned_tree", "ROSE_tree"), each = 6),
  Metric = rep(c("Accuracy", "Precision","Recall", "Specificity", "F1_Score", "AUC_ROC"), times = 5),
  Value = c(original_performance, rm_performance, kfold_performance, tune_tree_performance, ROSE_performance)
)

ggplot(all_trees, aes(x = Method, y = Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Comparison of Classification Tree Methods",
       x = "Method", y = "Value") +
  theme_minimal()
