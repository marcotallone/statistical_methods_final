#install.packages("caret")
# LOADING AND PREPROCESSING ----------------------------------------------------
print(dirname(rstudioapi::getSourceEditorContext()$path))
# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


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

# Checking balance in response variable:
table(bank$Attrition_Flag)
#There is high imbalance in data. Applying ROSE() to get balanced data

# cat("Attrited customers (rare class): ",sum(bank_logistic$Attrition_Flag==1))
# cat("Existing customers (maj class): ",sum(bank_logistic$Attrition_Flag==0))
# cat("Proportion of attrited:",
#     sum(bank_logistic$Attrition_Flag==1)/sum(table(bank_logistic$Attrition_Flag))*100,"%")
bank_ROSE <- bank[,c("Attrition_Flag", "Gender", "Dependent_count", "Marital_Status", "Income_Category", "Card_Category", 
                     "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", 
                     "Total_Revolving_Bal", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt","Total_Trans_Ct","Total_Ct_Chng_Q4_Q1")]
#install.packages("ROSE")
library(ROSE)
bank_balanced<- ROSE(Attrition_Flag ~ ., data=bank_ROSE, seed = 123)$data

table(bank_balanced$Attrition_Flag)


#Now we can run a classification tree model on the new balanced dataset:

ROSE_tree_fit <- rpart(Attrition_Flag ~ ., method="class", data = bank_balanced)

rpart.plot(ROSE_tree_fit, extra=1, digits=4, box.palette="auto")

predictions_ROSE <- predict(ROSE_tree_fit, type = "class", newdata= bank_balanced)

#Performance indices:

# Calculate accuracy
accuracy_ROSE <- confusionMatrix(predictions_ROSE, bank_balanced$Attrition_Flag)$overall['Accuracy']

# Calculate precision
precision_ROSE <- confusionMatrix(predictions_ROSE, bank_balanced$Attrition_Flag)$byClass['Precision']

# Calculate recall
recall_ROSE <- confusionMatrix(predictions_ROSE, bank_balanced$Attrition_Flag)$byClass['Sensitivity']

# Calculate specificity
specificity_ROSE <- confusionMatrix(predictions_ROSE, bank_balanced$Attrition_Flag)$byClass['Specificity']

# Calculate F1 score
f1_score_ROSE <- confusionMatrix(predictions_ROSE, bank_balanced$Attrition_Flag)$byClass['F1']

# Calculate AUC-ROC

roc_ROSE <- roc(bank_balanced$Attrition_Flag, as.numeric(predictions_ROSE))
auc_roc_ROSE <- auc(roc_ROSE)

ROSE_performance <- c(Accuracy = accuracy_ROSE, Precision = precision_ROSE, Recall = recall_ROSE, 
                      Specificity = specificity_ROSE, F1_Score = f1_score_ROSE, AUC_ROC = auc_roc_ROSE)

ROSE_performance
