#-------------------------------------------------------------------------------
# DATA PRE-PROCESSING
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset and pre-process it
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")
bank <- bank[, -c(1, 3, 5, 6, 9, 10, 14, 16, 17, 21, 22, 23)]
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)
#bank$Attrition_Flag<- as.factor(bank$Attrition_Flag)
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

# VALUTARE SE RENDERE FACTOR ANCHE Contacts_Count_12_mon
#-------------------------------------------------------------------------------
# IMBALANCED DATA
table(bank$Attrition_Flag)
cat("Attrited customers (rare class): ",sum(bank$Attrition_Flag==1))
cat("Existing customers (maj class): ",sum(bank$Attrition_Flag==0))
cat("Proportion of attrited:",
    sum(bank$Attrition_Flag==1)/sum(table(bank$Attrition_Flag))*100,"%")
#-------------------------------------------------------------------------------
# APPLYING ROSE
library(ROSE)
bank_balanced<- ROSE(Attrition_Flag~.,data=bank,seed = 123)$data
table(bank_balanced$Attrition_Flag)
cat("Attrited customers (rare class): ",sum(bank_balanced$Attrition_Flag==1))
cat("Existing customers (maj class): ",sum(bank_balanced$Attrition_Flag==0))
cat("Proportion of attrited:",
    sum(bank_balanced$Attrition_Flag==1)/sum(table(bank_balanced$Attrition_Flag))*100,"%")
#-------------------------------------------------------------------------------
# LOGISTIC REGRESSION (WILL IT IMPROVE?)
source("assessment_utils.R") # I put all the assessment functions here

bank_logistic <- learn(bank_balanced)
summary(bank_logistic)
anova(bank_logistic, test = "Chisq")
vif(bank_logistic)

# Assessing the model
cat("Results on whole dataset:\n")
results <- assess(bank_logistic, bank_balanced)

cat("Results on 10-fold cross validation:\n")
cv(bank_balanced)

