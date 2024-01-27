#-------------------------------------------------------------------------------
library(forcats)
# DATA PRE-PROCESSING
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset and pre-process it
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")
bank <- bank[, -c(1, 3, 5, 6, 9, 10, 14, 16, 17, 21, 22, 23)]
#bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)
# Convert Attrition_Flag to a binary factor
bank$Attrition_Flag <- factor(bank$Attrition_Flag == "Attrited Customer", levels = c(FALSE, TRUE))

# If "Attrited Customer" is TRUE, it will be coded as 1, and other values will be coded as 0


# Convert all categorical variables to factors and reorder the levels
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

# Joining toghether the levels after 4 months
bank$Months_Inactive_12_mon <- fct_collapse(bank$Months_Inactive_12_mon,
                                            "4+" = c("4", "5", "6+"))
# Override the Total_Trans_Amt variable with its log !!!
bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)
# Standardization (optional) all columns except response and categorical
#bank[, -c(1, 2, 3, 4)] <- scale(bank[, -c(1, 2, 3, 4)]) 

#-------------------------------------------------------------------------------
# IMPORT LIBRARIES
library(randomForest)
library(caret)
library(adabag)   # Boosting

# FIRST TRY WITH BOOSTING
# Static splitting the dataset into the Training set and Test set
set.seed(1234)
index <- createDataPartition(bank$Attrition_Flag , p =0.8, list = FALSE)

train.bank <- bank[index,]
test.bank <- bank[-index,]
cat("Attrited customers (rare class): ",table(train.bank$Attrition_Flag)[2])
cat("Existing customers (maj class): ",table(train.bank$Attrition_Flag)[1])

# Fitting AdaBoost to the Training set and keep track of variable importance
bank.boost <- boosting(Attrition_Flag ~ ., data = train.bank, boos = TRUE)
print(bank.boost)

# Predicting the Test set results
pred <- predict(bank.boost, newdata = test.bank)
pred$confusion

# Extract information about variable selection
# Extract the variable selection information for each boosting iteration
var_selection <- bank.boost$importance

# Sort the variable importance in descending order
sorted_variable_importance <- sort(var_selection, decreasing = TRUE)

# Convert variable importance to a data frame
variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                     Importance = sorted_variable_importance,
                                     row.names = NULL)

# Print the variable importance as a column
print(variable_importance_df)

# CV splitting the dataset into the Training set and Test set
set.seed(1234)
bank.boostcv <- boosting.cv(Attrition_Flag ~ ., data = bank, boos = TRUE,
                             v=10)

# Confusion matrix for the best iteration
bank.boostcv$confusion

#-------------------------------------------------------------------------------
# SECOND TRY WITH RANDOM FOREST
bank.rf <- randomForest(Attrition_Flag ~ ., data = train.bank, ntree = 500,
                        seed=123, importance = TRUE)

# Predicting the Test set results
pred <- predict(bank.rf, newdata = test.bank)
confusion_matrix<- table(pred, test.bank$Attrition_Flag)
confusion_matrix

print(bank.rf)

# Variable importance plot
varImpPlot(bank.rf, sort = TRUE, n.var = 10, main = "Variable Importance")

# Plotting the tree
plot(bank.rf, main = "Random Forest")
legend("topright", colnames(bank.rf$err.rate), col = 1:3, fill = 1:3)

#-------------------------------------------------------------------------------
# Assessment
# 1. What is the accuracy of the boosting model on the test set?
accuracy <- (pred$confusion[1,1] + pred$confusion[2,2]) / sum(pred$confusion)

# 2. AUC index
# Predict class probabilities
pred_prob <- predict(bank.boost, newdata = test.bank, type = "prob")
pred_prob

# Extract probabilities for the positive class
pred_prob_positive <- pred_prob$prob[,2]

# Create a binary vector indicating the true positive class
true_class <- factor(ifelse(test.bank$Attrition_Flag == "Attrited Customer", 1, 0))

# Load the pROC package
library(pROC)

# Compute AUC
auc <- auc(roc(true_class, pred_prob_positive))
print(paste("AUC:", auc))


# Print of assessment indexes
cat("----------------------------------------\n")
print(pred$confusion)
cat("----------------------------------------\n")
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("----------------------------------------\n")
cat("AUC:", round(results$auc * 100, 2), "%\n")
cat("Dummy classifier AUC:",
    round(results$dummy_classifier_auc * 100, 2), "%\n")
cat("----------------------------------------\n")
cat("FPR:", round(results$fpr * 100, 2), "%\n")
cat("FNR:", round(results$fnr * 100, 2), "%\n")
cat("----------------------------------------\n")
