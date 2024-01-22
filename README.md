# Statistical Methods Final Project

> Final project for the Statistical Methods course of SDIC/DSAI Master degrees - UniTs 2023/2024

## Contributors

| Name | Surname | Master |
|:---:|:---:|:---:|
| Sara | - | DSAI |
| Giulio | - | DSAI |
| Vishal | - | DSAI |
| Marco | Tallone | SDIC |
| Alessio | - | DSAI |

## TO-DO

* [x] Create Repository
* [x] Add README
* [x] Download dataset
* [x] R Markdown
* [x] R Scripts Folder
* [x] Add Contributors
* [x] Preprocessing script
* [x] Exploratory Analysis
* [x] Feature selection and variables importance
* [x] Logistic Regression
* [x] K-fold CV
* [ ] Check computation of AUC, fpr and fnr
* [ ] Make better exploratory analysis plots
* [ ] Prepare presentation slides

## Project Structure

The project's structure is the following.

```bash
.
├── datasets # Folder with datasets
│   └── BankChurners.csv
├── GroupB_Final.Rmd # Final R Markdown
├── README.md # This file
└── r_scripts # R scripts
    └── bank.R

```

## Assignment

Predicting the `Attrition Flag` response variable from the [Credit Card costumers dataset]( https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers ) available on Kaggle.\
The dataset can be found in the `datasets/` folder.\
Other projects based on this dataset [here](https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers/code).

## About the dataset

A manager at the bank is disturbed with more and more customers leaving their credit card services. They would really appreciate if one could predict for them who is gonna get churned so they can proactively go to the customer to provide them better services and turn customers' decisions in the opposite direction

I got this dataset from the [leaps.analyttica wesite](https://leaps.analyttica.com/home). I have been using this for a while to get datasets and accordingly work on them to produce fruitful results. The site explains how to solve a particular business problem.

Now, this dataset consists of 10,000 customers mentioning their age, salary, marital_status, credit card limit, credit card category, etc. There are nearly 18 features.

We have only 16.07% of customers who have churned. Thus, it's a bit difficult to train our model to predict churning customers.

*~ source: [Kaggle]( https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers ).*

## Dataset Summary

Here is a brief summary of what the dataset contains.

> [!WARNING]
> **PLEASE IGNORE THE LAST 2 COLUMNS (NAIVE BAYES CLAS…). I SUGGEST TO RATHER DELETE IT BEFORE DOING ANYTHING**

>[!IMPORTANT]
>A business manager of a consumer credit card portfolio is facing the problem of customer attrition. They want to analyze the data to find out the reason behind this and leverage the same to predict customers who are likely to drop off.

Variables description:

> **Legend**:  *🆎: categorical,  🔢: numerical,  🔀: binary*

| # | Variable Name | Description | Type |
|:---:|:---|:---|:---:|
| 1 | `CLIENTNUM` | Client number. Unique identifier for the customer holding the account. | 🔢 |
| 2 | `Attrition_Flag` | Internal event (customer activity) variable - if the account is closed then 1 else 0 | 🔀* |
| 3 | `Costumer_Age` | Demographic variable - Customer's Age in Years | 🔢 |
| 4 | `Gender` | Demographic variable - M=Male, F=Female | 🔀* |
| 5 | `Dependent_Count` | Demographic variable - Number of dependents | 🔢 |
| 6 | `Education_Level` | Demographic variable - Educational Qualification of the account holder (example: high school, college graduate, etc.) | 🆎 |
| 7 | `Marital_Status` | Demographic variable - Married, Single, Divorced, Unknown | 🆎 |
| 8 | `Income_Category` | Demographic variable - Annual Income Category of the account holder (< $40K, $40K - 60K, $60K - $80K, $80K-$120K, > | 🆎 |
| 9 | `Card_Category` | Product Variable - Type of Card (Blue, Silver, Gold, Platinum) | 🆎 |
| 10 | `Months_on_book` | Period of relationship with bank | 🔢 |
| 11 | `Total_Relationhip_Count` |Total no. of products held by the customer | 🔢 |
| 12 | `Months_Inactive_12_mon` | No. of months inactive in the last 12 months | 🔢 |
| 13 | `Contacts_Count_12_mon` | No. of Contacts in the last 12 months | 🔢 |
| 14 | `Credit_Limit` | Credit Limit on the Credit Card | 🔢 |
| 15 | `Total_Revolving_Bal` | Total Revolving Balance on the Credit Card | 🔢 |
| 16 | `Avg_Open_To_Buy` | Open to Buy Credit Line (Average of last 12 months) | 🔢 |
| 17 | `Total_Amt_Chng_Q4_Q1` | Change in Transaction Amount (Q4 over Q1) | 🔢 |
| 18 | `Total_Trans_Amt` | Total Transaction Amount (Last 12 months) | 🔢 |
| 19 | `Total_Trans_Ct` | Total Transaction Count (Last 12 months) | 🔢 |
| 20 | `Total_Ct_Chng_Q4_Q1` | Change in Transaction Count (Q4 over Q1) | 🔢 |
| 21 | `Avg_Utilization_Ratio` | Average Card Utilization Ratio | 🔢 |
| 22 | ~~`Naive_Bayes_Cla..._1`~~ | ~~Naive Bayes~~ | ~~🔢~~ |
| 23 | ~~`Naive_Bayes_Cla..._2`~~ | ~~Naive Bayes~~ | ~~🔢~~ |

> \* *after conversion*

## Importing the dataset

To import and use the dataset in an R script or R Markdown file, use the following code.

```r
# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset from the datasets/ folder
bank <- read.csv("path/to/datasets/BankChurners.csv", sep = ",")
```

## Preprocessing

>[!NOTE]
> The preprocessing steps can be found in the `r_scripts/preprocessing.r` file.

As suggested from the Kaggel description of the dataset, we removed the last two columns.

```r
# Remove the last two columns as suggested in the README
bank <- bank[, -c(22, 23)]
```

Then, we removed the `CLIENTNUM` column as it is just an identifier.

```r
# Remove the first column as it is just an index
bank <- bank[, -1]
```

After that, it was necessary to convert the `Attrition_Flag` column to a binary variable:

* `0` if the account is not closed, i.e. for the `Existing Customer` value
* `1` if the account is closed, i.e. for the `Attrited Customer` value

```r
# Convert the Attrition_Flag column to a binary variable
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)
```

Accordingly all categorical variables were coverted to factors:

```r
# Convert all categorical variables to factors
bank$Gender <- as.factor(bank$Gender)
bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)
```

Luckily there were no missing values in the dataset, so we could proceed with the analysis.

## Logistic regression

>[!NOTE]
> The logistic regression can be found in the `r_scripts/logistic_regression.r` file.

### Feature Engineering

A model using logistic regression has been built to predict the `Attrition_Flag` response variable.\
In an initial phase the most relevant variables have been selected to build the model.\
The selection criteria used have been the following:

* Variables with a p-value lower than 0.05 in the `glm()` model summary have been selected
* Variables with low correlation with the response variable have been removed
* Using the `anova()` test, only the most significant variables (including categorical variables) have been selected
* To avoid multicollinearity, variables with low VIF have been selected

The final model has been built using the following variables:

* `Gender`: the gender of the customer
* `Marital_Status`: the marital status of the customer
* `Income_Category`: the income category of the customer
* `Total_Relationship_Count`: the total number of products held by the customer
* `Months_Inactive_12_mon`: the number of months inactive in the last 12 months
* `Contacts_Count_12_mon`: the number of contacts in the last 12 months
* `Total_Revolving_Bal`: the total revolving balance on the credit card
* `Total_Trans_Amt`: the total transaction amount in the last 12 months
* `Total_Trans_Ct`: the total transaction count in the last 12 months
* `Total_Ct_Chng_Q4_Q1`: the change in transaction count from Q4 to Q1

The result of the ANOVA test is the following:

```terminal
Analysis of Deviance Table

Model: binomial, link: logit

Response: Attrition_Flag

Terms added sequentially (first to last)


                         Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                                     10126     8927.2              
Gender                    1    14.12     10125     8913.1 0.0001715 ***
Marital_Status            3     5.90     10122     8907.2 0.1165956    
Income_Category           5    11.16     10117     8896.0 0.0482313 *  
Total_Relationship_Count  1   225.84     10116     8670.2 < 2.2e-16 ***
Months_Inactive_12_mon    1   230.09     10115     8440.1 < 2.2e-16 ***
Contacts_Count_12_mon     1   495.91     10114     7944.2 < 2.2e-16 ***
Total_Revolving_Bal       1   628.78     10113     7315.4 < 2.2e-16 ***
Total_Trans_Amt           1   679.95     10112     6635.4 < 2.2e-16 ***
Total_Trans_Ct            1  1800.47     10111     4835.0 < 2.2e-16 ***
Total_Ct_Chng_Q4_Q1       1   430.12     10110     4404.9 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

While the VIF test:

```terminal
                             GVIF Df GVIF^(1/(2*Df))
Gender                   3.569521  1        1.889318
Marital_Status           1.065029  3        1.010556
Income_Category          3.574890  5        1.135864
Total_Relationship_Count 1.117525  1        1.057130
Months_Inactive_12_mon   1.024143  1        1.012000
Contacts_Count_12_mon    1.032402  1        1.016072
Total_Revolving_Bal      1.042901  1        1.021225
Total_Trans_Amt          6.669191  1        2.582478
Total_Trans_Ct           6.845917  1        2.616470
Total_Ct_Chng_Q4_Q1      1.096680  1        1.047225
```

### Model assessment

In the relative `R` script, appropiate learning and prediction functions have been defined as well as methods to compute effectveness metrics on the whole dataset and performing a k-fold cross validation.\
The effectiveness metrics used so far are the following:

* Accuracy
* AUC
* FPR (False Positive Rate)
* FNR (False Negative Rate)
* Confusion matrix (*only in the whole dataset case*)
* AIC
* BIC

The Dummy classifier has been taken as a baseline for comparison.

### Results

The model fitted on the whole dataset is the following:

```terminal
Call:
glm(formula = Attrition_Flag ~ ., family = binomial(link = "logit"), 
    data = data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.0843  -0.3420  -0.1450  -0.0473   3.6999  

Coefficients:
                              Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   -2.12638    0.25019  -8.499  < 2e-16 ***
GenderM                       -0.86146    0.14754  -5.839 5.26e-09 ***
Marital_StatusMarried         -0.45238    0.16146  -2.802 0.005081 ** 
Marital_StatusSingle           0.01729    0.16248   0.106 0.915259    
Marital_StatusUnknown         -0.04065    0.20624  -0.197 0.843768    
Income_Category$40K - $60K    -0.72166    0.18969  -3.804 0.000142 ***
Income_Category$60K - $80K    -0.46179    0.17566  -2.629 0.008566 ** 
Income_Category$80K - $120K   -0.25768    0.16980  -1.518 0.129127    
Income_CategoryLess than $40K -0.63249    0.20376  -3.104 0.001909 ** 
Income_CategoryUnknown        -0.79028    0.23044  -3.430 0.000605 ***
Total_Relationship_Count      -0.76106    0.04315 -17.638  < 2e-16 ***
Months_Inactive_12_mon         0.49064    0.03946  12.435  < 2e-16 ***
Contacts_Count_12_mon          0.54786    0.04157  13.179  < 2e-16 ***
Total_Revolving_Bal           -0.74870    0.03858 -19.405  < 2e-16 ***
Total_Trans_Amt                2.62421    0.10454  25.103  < 2e-16 ***
Total_Trans_Ct                -4.17099    0.12845 -32.472  < 2e-16 ***
Total_Ct_Chng_Q4_Q1           -0.79465    0.04534 -17.527  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 8927.2  on 10126  degrees of freedom
Residual deviance: 4404.9  on 10110  degrees of freedom
AIC: 4438.9

Number of Fisher Scoring iterations: 7
```

The results obtained on the same training dataset are the following:

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     8225      275
  Attrited      591     1036
----------------------------------------
Accuracy: 91.45 %
Dummy classifier accuracy: 83.93 %
----------------------------------------
AUC: 93.54 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 36.32 %
FNR: 3.24 %
----------------------------------------
AIC: 4438.858 
BIC: 4561.648 
----------------------------------------
```

Note that the accuracy of the Dummy classifier should be taken into account due to the relatively high imbalance of the dataset.
The results obtained using a 10-fold cross validation are the following:

```terminal
----------------------------------------
Average accuracy: 91.34 +/- 1.14 %
----------------------------------------
Average AUC: 93.48 +/- 0.72 %
----------------------------------------
Average FPR: 3.23 +/- 0.73 %
Average FNR: 45.27 +/- 7.74 %
----------------------------------------
Average AIC: 3996.685 +/- 32.0919 
Average BIC: 4117.684 +/- 32.09195 
----------------------------------------
```

### Summary

| Metric | Value | Standard Deviation | Assessment Technique |
|:---:|:---:|:---:|:---:|
| Accuracy | 91.45 % | - | Whole dataset |
| AUC | 93.54 % | - | Whole dataset |
| Dummy accuracy | 83.93 % | - | Whole dataset |
| Dummy AUC | 50 % | - | Whole dataset |
| FPR | 36.32 % | - | Whole dataset |
| FNR | 3.24 % | - | Whole dataset |
| AIC | 4438.858 | - | Whole dataset |
| BIC | 4561.648 | - | Whole dataset |
| Accuracy | 91.34 % | 1.14 % | 10-fold CV |
| AUC | 93.48 % | 0.72 % | 10-fold CV |
| FPR | 3.23 % | 0.73 % | 10-fold CV |
| FNR | 45.27 % | 7.74 % | 10-fold CV |
| AIC | 3996.685 | 32.0919 | 10-fold CV |
| BIC | 4117.684 | 32.09195 | 10-fold CV |
