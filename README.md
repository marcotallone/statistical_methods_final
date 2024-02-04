# Statistical Methods Final Project

> Final project for the Statistical Methods course of SDIC/DSAI Master degrees - UniTs 2023/2024

## Contributors

| Name | Surname | Master |
|:---:|:---:|:---:|
| Sara | - | DSAI |
| Giulio | Fantuzzi | DSAI |
| Vishal | - | DSAI |
| Marco | Tallone | SDIC |
| Alessio | Valentinis | DSAI |

## Table of contents
- [TO-DO list](#to-do)
- [General info](#general-info)
  - [Project Structure](#project-structure)
  - [About the assignment](#assignment)
  - [About the dataset](#about-the-dataset)
- [Implemented Models](#implemented-models)
  - [Logistic Regression](#logistic-regression)
  - [Penalized Regression](#penalized-regression)
  - [Splines](#splines)
  - [Ensamble methods](#ensamble-methods)
- [Applying ROSE package](#rose-package-to-deal-with-class-imbalance)
  - [ROSE for Logistic Regression](#rose-results-for-logistic-regression)
  - [ROSE for Splines](#rose-results-for-splines)
  - [ROSE for Ensamble Methods](#rose-results-for-ensamble-methods)

---
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
* [x] Apply ROSE and check how models improve (if they improve)
* [x] Check computation of AUC, fpr and fnr
* [ ] ABSOLUTELY CHANGE NAMES TO LEARN/PREDICT FUNCTIONS IF WE WILL CREATE A UNIQUE .Rmd
* [ ] Decide presentation scheduling and timings
* [ ] Just something I noticed: *assess()* can be assigned to an object; while cv functions not

# General Info

## Project Structure

The project's structure is the following.

```bash
.
├── datasets # Folder with datasets
│   └── BankChurners.csv
├── plots # Folder with saved R plots
│   └── plot.png
├── GroupB_Final.Rmd # Final R Markdown
├── README.md # This file
└── r_scripts # R scripts
    └── bank.R

```

## Assignment

Predicting the `Attrition Flag` response variable from the [Credit Card costumers dataset]( https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers ) available on Kaggle.\
The dataset can be found in the `datasets/` folder.\
Other projects based on this dataset [here](https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers/code).\
Interesting notebook to look at [here](https://www.kaggle.com/code/kaushikmajumder/credit-card-customer-churn-prediction).

## About the dataset

A manager at the bank is disturbed with more and more customers leaving their credit card services. They would really appreciate if one could predict for them who is gonna get churned so they can proactively go to the customer to provide them better services and turn customers' decisions in the opposite direction.

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

# Implemented Models

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
* ~~`Marital_Status`: the marital status of the customer~~
* ~~`Income_Category`: the income category of the customer~~
* `Total_Relationship_Count`: the total number of products held by the customer
* `Months_Inactive_12_mon`: the number of months inactive in the last 12 months
* `Contacts_Count_12_mon`: the number of contacts in the last 12 months
* `Total_Revolving_Bal`: the total revolving balance on the credit card
* `Total_Trans_Amt`: the total transaction amount in the last 12 months
* `Total_Trans_Ct`: the total transaction count in the last 12 months
* `Total_Ct_Chng_Q4_Q1`: the change in transaction count from Q4 to Q1

Aditionally, looking at the data distribution it has been taken the logarihmic values of the `Total_Trans_Amt` variable. Also, the `Months_Inactive_12_mon` variable has been converted to a factor due to its peculiar distribution (see plot) with the levels `1`, `2`, `3` and `4+` months.
Both these changes have significantly improved the model performance as portrayed by the metrics below.

The result of the ANOVA test is the following:

```terminal
Analysis of Deviance Table

Model: binomial, link: logit

Response: Attrition_Flag

Terms added sequentially (first to last)


                         Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                                     10126     8927.2              
Gender                    1    14.12     10125     8913.1 0.0001715 ***
Total_Relationship_Count  1   227.81     10124     8685.3 < 2.2e-16 ***
Months_Inactive_12_mon    4   434.03     10120     8251.2 < 2.2e-16 ***
Contacts_Count_12_mon     1   479.80     10119     7771.4 < 2.2e-16 ***
Total_Revolving_Bal       1   608.24     10118     7163.2 < 2.2e-16 ***
Total_Trans_Amt           1   636.63     10117     6526.6 < 2.2e-16 ***
Total_Trans_Ct            1  1724.51     10116     4802.0 < 2.2e-16 ***
Total_Ct_Chng_Q4_Q1       1   439.71     10115     4362.3 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

While the VIF test:

```terminal
                             GVIF Df GVIF^(1/(2*Df))
Gender                   1.024050  1        1.011954
Total_Relationship_Count 1.119040  1        1.057847
Months_Inactive_12_mon   1.027901  4        1.003446
Contacts_Count_12_mon    1.026478  1        1.013152
Total_Revolving_Bal      1.039958  1        1.019783
Total_Trans_Amt          6.658530  1        2.580413
Total_Trans_Ct           6.762163  1        2.600416
Total_Ct_Chng_Q4_Q1      1.101724  1        1.049630
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
-2.9883  -0.3308  -0.1429  -0.0468   3.4870  

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -0.59957    0.66799  -0.898 0.369414    
GenderM                  -0.54425    0.07935  -6.858 6.96e-12 ***
Total_Relationship_Count -0.76274    0.04339 -17.580  < 2e-16 ***
Months_Inactive_12_mon1  -3.71092    0.68026  -5.455 4.89e-08 ***
Months_Inactive_12_mon2  -2.39858    0.67176  -3.571 0.000356 ***
Months_Inactive_12_mon3  -1.94577    0.67049  -2.902 0.003708 ** 
Months_Inactive_12_mon4+ -1.59904    0.68009  -2.351 0.018711 *  
Contacts_Count_12_mon     0.52866    0.04167  12.685  < 2e-16 ***
Total_Revolving_Bal      -0.74802    0.03870 -19.327  < 2e-16 ***
Total_Trans_Amt           2.61316    0.10463  24.975  < 2e-16 ***
Total_Trans_Ct           -4.08983    0.12788 -31.982  < 2e-16 ***
Total_Ct_Chng_Q4_Q1      -0.80523    0.04547 -17.707  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 8927.2  on 10126  degrees of freedom
Residual deviance: 4362.3  on 10115  degrees of freedom
AIC: 4386.3

Number of Fisher Scoring iterations: 7
```

The results obtained on the same training dataset are the following:

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     8238      262
  Attrited      576     1051
----------------------------------------
Accuracy: 91.73 %
Dummy classifier accuracy: 83.93 %
----------------------------------------
AUC: 93.67 %
Random classifier AUC: 50 %
----------------------------------------
FPR: 3.08 %
FNR: 35.4 %
----------------------------------------
AIC: 4386.343 
BIC: 4473.018 
----------------------------------------
```

Note that the accuracy of the Dummy classifier should be taken into account due to the relatively high imbalance of the dataset.
The results obtained using a 10-fold cross validation are the following:

```terminal
----------------------------------------
Average accuracy: 91.66 +/- 1.22 %
----------------------------------------
Average AUC: 93.66 +/- 1.34 %
----------------------------------------
Average FPR: 3.02 +/- 0.75 %
Average FNR: 44.45 +/- 11.22 %
----------------------------------------
Average AIC: 3948.844 +/- 49.15868 
Average BIC: 4034.255 +/- 49.15871 
----------------------------------------
```

### Summary

| Metric | Value | Standard Deviation | Assessment Technique |
|:---:|:---:|:---:|:---:|
| Accuracy | 91.73% | - | Whole dataset |
| AUC | 93.67 % | - | Whole dataset |
| Dummy accuracy | 83.93 % | - | Whole dataset |
| Random AUC | 50 % | - | Whole dataset |
| FPR | 3.08 % | - | Whole dataset |
| FNR | 35.4 % | - | Whole dataset |
| AIC | 4386.343 | - | Whole dataset |
| BIC | 4473.018 | - | Whole dataset |
| Accuracy | 91.66 % | 1.22 % | 10-fold CV |
| AUC | 93.66 % | 0.34 % | 10-fold CV |
| FPR | 3.02 % | 0.75 % | 10-fold CV |
| FNR | 44.45 % | 11.22 % | 10-fold CV |
| AIC | 3948.844 | 49.15868 | 10-fold CV |
| BIC | 4034.255 | 49.15871 | 10-fold CV |

## Penalized Regression
>[!NOTE]
> The model with splines can be found in the `r_scripts/penalized_regression.r` file.

I decided to make a unique script with general functions that allow the user to select its preference between RIDGE/LASSO

Regarding RIDGE, the results from a 10-fold CV were:
```terminal
----------------------------------------
Average accuracy: 90.31 +/- 0.56 %
----------------------------------------
Average AUC: 91.53 +/- 1.36 %
----------------------------------------
Average FPR: 1.61 +/- 0.48 %
Average FNR: 88.74 +/- 8.03 %
----------------------------------------
```

Regarding LASSO, the results from a 10-fold CV were:

```terminal
----------------------------------------
Average accuracy: 91.42 +/- 0.8 %
----------------------------------------
Average AUC: 93.43 +/- 0.75 %
----------------------------------------
Average FPR: 3.12 +/- 0.79 %
Average FNR: 45.89 +/- 7.79 %
----------------------------------------
```

**NOTES**: as we can see, the value of FNR is extremely high😓. Good news is that with ROSE this will improve a lot (see [here](#rose-results-for-penalized-regression))

## Splines
I used the gam function available in MASS package.
>[!NOTE]
> The model with splines can be found in the `r_scripts/splines.r` file.

### Feature Engineering
Regarding preprocessing I used the codes available in preprocesing.R in logistic_regression.r and testing_ROSE.R. I decided not to delete at first columns  3, 5, 6, 9, 10, 14, 16, 17, 21, and I implemented 2 differents models to compare them and see if those wariables would be helpful in a gam approach.

As we can see from the summary of gamfit_first_try:

```terminal

Family: gaussian 
Link function: identity 

Formula:
Attrition_Flag ~ s(Customer_Age) + Gender + Dependent_count + 
    Education_Level + Marital_Status + Income_Category + Card_Category + 
    s(Months_on_book) + Total_Relationship_Count + Months_Inactive_12_mon + 
    Contacts_Count_12_mon + s(Credit_Limit) + s(Total_Revolving_Bal) + 
    s(Avg_Open_To_Buy) + s(Total_Amt_Chng_Q4_Q1) + s(Total_Trans_Amt) + 
    s(Total_Trans_Ct) + s(Total_Ct_Chng_Q4_Q1) + s(Avg_Utilization_Ratio)

Parametric coefficients:
                               Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    0.368078   0.045587   8.074 7.57e-16 ***
GenderM                       -0.031407   0.005340  -5.882 4.19e-09 ***
Dependent_count                0.003172   0.002103   1.508  0.13158    
Education_LevelDoctorate       0.018007   0.013043   1.381  0.16743    
Education_LevelGraduate        0.001397   0.008311   0.168  0.86651    
Education_LevelHigh School     0.004338   0.008855   0.490  0.62425    
Education_LevelPost-Graduate   0.017628   0.012440   1.417  0.15652    
Education_LevelUneducated      0.002446   0.009372   0.261  0.79410    
Education_LevelUnknown         0.009723   0.009333   1.042  0.29752    
Marital_StatusMarried         -0.034573   0.004725  -7.317 2.72e-13 ***
Income_CategoryLess than 120K -0.016113   0.009762  -1.651  0.09885 .  
Card_CategoryGold              0.041499   0.022734   1.825  0.06796 .  
Card_CategoryPlatinum          0.049960   0.052124   0.958  0.33784    
Card_CategorySilver            0.003453   0.011712   0.295  0.76815    
Total_Relationship_Count      -0.020870   0.001654 -12.615  < 2e-16 ***
Months_Inactive_12_mon1       -0.210735   0.043096  -4.890 1.02e-06 ***
Months_Inactive_12_mon2       -0.160449   0.042975  -3.734  0.00019 ***
Months_Inactive_12_mon3       -0.132801   0.042942  -3.093  0.00199 ** 
Months_Inactive_12_mon4+      -0.118787   0.043574  -2.726  0.00642 ** 
Contacts_Count_12_mon          0.025425   0.002132  11.924  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                            edf Ref.df       F  p-value    
s(Customer_Age)          8.8777 8.9915  29.833  < 2e-16 ***
s(Months_on_book)        1.0000 1.0000   1.826   0.1766    
s(Credit_Limit)          0.5021 0.5021  38.230 1.20e-05 ***
s(Total_Revolving_Bal)   8.8682 8.9905  52.414  < 2e-16 ***
s(Avg_Open_To_Buy)       1.3158 1.7523  12.087 2.81e-05 ***
s(Total_Amt_Chng_Q4_Q1)  8.2570 8.8236  58.890  < 2e-16 ***
s(Total_Trans_Amt)       8.9462 8.9990 407.685  < 2e-16 ***
s(Total_Trans_Ct)        8.4391 8.8853 250.697  < 2e-16 ***
s(Total_Ct_Chng_Q4_Q1)   7.7431 8.5535  40.753  < 2e-16 ***
s(Avg_Utilization_Ratio) 3.9858 4.9592   2.883   0.0131 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Rank: 109/110
R-sq.(adj) =  0.611   Deviance explained = 61.4%
GCV = 0.05286  Scale est. = 0.052453  n = 10127
```
the edf of Months_on_book, Credit_Limit, Avg_Open_To_Buy are <2. So I decided to include in the gamfit model in an additive way without using splines.

In the gamfit_less_vars I considered only the variables: Gender, Marital_Status, Income_Category,Total_Relationship_Count,Months_Inactive_12_month, Contacts_Count_12_mon; and as splines:Total_Revolving_Bal, Total_Trans_Amt, Total_Trans_Ct, Total_Ct_Chng_Q4_Q1.

### Assessment
For the assessment the code is almost the same as for Logistic regression. I slightly changed the code of cv in order to let it execute with different learn function, to compare the results of the 2 models.

For gamfit:
```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     8378      122
  Attrited      371     1256
----------------------------------------
Accuracy: 95.13 %
Dummy classifier accuracy: 83.93 %
----------------------------------------
AUC: 98.2 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 1.44 %
FNR: 22.8 %
----------------------------------------
AIC: -1032.97 
BIC: -473.1245 
----------------------------------------
```

For gamfit_less_vars:
```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     8331      169
  Attrited      430     1197
----------------------------------------
Accuracy: 94.09 %
Dummy classifier accuracy: 83.93 %
----------------------------------------
AUC: 97.52 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 1.99 %
FNR: 26.43 %
----------------------------------------
AIC: -219.6766 
BIC: 98.15811 
----------------------------------------
```

For gamfit with 10 CV:
```terminal
----------------------------------------
Average accuracy: 95.13 +/- 1.24 %
----------------------------------------
Average AUC: 98.22 +/- 0.55 %
----------------------------------------
Average FPR: 1.4 +/- 0.4 %
Average FNR: 27.16 +/- 8.87 %
----------------------------------------
Average AIC: -1032.97 +/- 0 
Average BIC: -473.1245 +/- 0 
----------------------------------------
```

For gamfit_less_vars with 10 CV:
```terminal
----------------------------------------
Average accuracy: 94.09 +/- 0.62 %
----------------------------------------
Average AUC: 97.53 +/- 0.4 %
----------------------------------------
Average FPR: 1.93 +/- 0.43 %
Average FNR: 31.94 +/- 6.58 %
----------------------------------------
Average AIC: -219.6766 +/- 0 
Average BIC: 98.15811 +/- 0 
----------------------------------------
```

## Ensamble methods

**NB: I noticed that in the preprocessing steps, we modified the Attrition_Flag variable making it binary, but we let it NUMERICAL (forcing models to do regression on it)! I don't know if it was intended, but either case it's worth a check. I already made a change in my file called `ensamble.R`**

>[!NOTE]
> The model with ensamble can be found in the `r_scripts/ensambe.R` file.

I first looked at AdaBoost method, first with a static train-test division of 80%- 20% and then with a 10-fold cross validation. I used the `adabag` package.

### Analysis on the whole dataset

### AdaBoost with static train-test division

Firstly I tried to use the whole dataset.

### Assessment
I made by hand all the assessments, as the models I implemented didn't come with all the parameters required in the functions into the `assessment_utils.R` script.

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     1680       20
  Attrited       48      277
----------------------------------------
Accuracy: 96.64 %
Dummy classifier accuracy: 83.95 %
----------------------------------------
AUC: 98.98 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 1.18 %
FNR: 14.77 %
----------------------------------------
Variable importance:
                   Variable Mean_Gini_Decrease
1           Total_Trans_Amt         21.2023085
2            Total_Trans_Ct         16.0602470
3      Total_Amt_Chng_Q4_Q1         12.6190669
4       Total_Ct_Chng_Q4_Q1          6.3711922
5       Total_Revolving_Bal          6.1183427
6              Customer_Age          5.6225678
7              Credit_Limit          5.0469853
8  Total_Relationship_Count          5.0028304
9           Avg_Open_To_Buy          3.7004296
10          Education_Level          3.6121165
11    Contacts_Count_12_mon          3.4454316
12           Months_on_book          2.9867131
13   Months_Inactive_12_mon          2.4091363
14    Avg_Utilization_Ratio          2.1693641
15           Marital_Status          1.2552304
16          Dependent_count          1.2051558
17                   Gender          0.7694100
18            Card_Category          0.2062053
19          Income_Category          0.1972667
----------------------------------------
```

### AdaBoost with 10-fold cross validation

I used the same parameters as before, but I used a 10-fold cross validation.
```terminal
----------------------------------------
Average accuracy: 97.33 +/- 0.65 %
----------------------------------------
Average AUC: 99.39 +/- 0.29 %
----------------------------------------
Average FPR: 1.12 +/- 0.36 %
Average FNR: 10.76 +/- 2.53 %
----------------------------------------
Average variable importance ranking:
                   Variable Mean_Gini_Decrease    Std_Dev
1           Total_Trans_Amt         21.3956578 1.02211133
2            Total_Trans_Ct         17.1991349 0.51198369
3      Total_Amt_Chng_Q4_Q1         11.3672550 0.41708711
4       Total_Revolving_Bal          7.2367961 0.25506424
5       Total_Ct_Chng_Q4_Q1          6.2949893 0.49071970
6              Customer_Age          5.1833259 0.35377941
7  Total_Relationship_Count          4.7237441 0.24920299
8              Credit_Limit          4.4433490 0.22454304
9           Education_Level          3.9403137 0.18198034
10          Avg_Open_To_Buy          3.7138876 0.21357719
11           Months_on_book          3.4115906 0.15480877
12    Contacts_Count_12_mon          3.1450846 0.20233821
13   Months_Inactive_12_mon          2.7952042 0.29731693
14    Avg_Utilization_Ratio          2.0598053 0.39425455
15          Dependent_count          1.1766892 0.14792473
16           Marital_Status          0.9806874 0.17593701
17                   Gender          0.5731588 0.10085429
18            Card_Category          0.2416357 0.11075239
19          Income_Category          0.1176907 0.06116263
----------------------------------------
```

### Random Forest with static train-test division

I used the `randomForest` package, and I fitted the model with default parameters.

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     1680       20
  Attrited       64      261
----------------------------------------
Accuracy: 95.85 %
Dummy classifier accuracy: 83.95 %
----------------------------------------
AUC: 98.71 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 1.18 %
FNR: 19.69 %
----------------------------------------
Variable importance:
                   Variable Mean_Gini_Decrease
16          Total_Trans_Amt         399.827927
17           Total_Trans_Ct         384.831802
13      Total_Revolving_Bal         264.123972
18      Total_Ct_Chng_Q4_Q1         230.778923
9  Total_Relationship_Count         142.053837
15     Total_Amt_Chng_Q4_Q1         141.913448
19    Avg_Utilization_Ratio         129.581664
1              Customer_Age          73.329894
12             Credit_Limit          71.973182
14          Avg_Open_To_Buy          67.580828
10   Months_Inactive_12_mon          59.108270
11    Contacts_Count_12_mon          58.958746
8            Months_on_book          52.208794
4           Education_Level          43.188889
3           Dependent_count          27.821580
2                    Gender          19.235724
5            Marital_Status          11.682715
7             Card_Category           5.858160
6           Income_Category           3.350073
----------------------------------------
```

### Random Forest with 10-fold cross validation

I used the same parameters as before, but I used a 10-fold cross validation.

```terminal
----------------------------------------
Average accuracy: 99.16 +/- 0.31 %
----------------------------------------
Average AUC: 99.9 +/- 0.07 %
----------------------------------------
Average FPR: 0.21 +/- 0.16 %
Average FNR: 4.28 +/- 1.96 %
----------------------------------------
Average variable importance ranking:
                   Variable Mean_Gini_Decrease   Std_Dev
1           Total_Trans_Amt         400.370327 0.7560910
2            Total_Trans_Ct         377.983812 0.6455279
3       Total_Revolving_Bal         251.801778 0.3734412
4       Total_Ct_Chng_Q4_Q1         236.051413 0.6783163
5     Avg_Utilization_Ratio         141.900558 0.2279646
6  Total_Relationship_Count         140.834560 0.1284268
7      Total_Amt_Chng_Q4_Q1         139.560874 0.1362001
8              Customer_Age          73.525782 0.6375022
9              Credit_Limit          72.771843 2.1550593
10          Avg_Open_To_Buy          68.327976 0.7628449
11    Contacts_Count_12_mon          59.880585 0.9598183
12   Months_Inactive_12_mon          58.126678 0.8253341
13           Months_on_book          52.115183 6.5760340
14          Education_Level          44.475905 0.7811312
15          Dependent_count          28.768839 1.6842069
16                   Gender          18.410534 2.0749535
17           Marital_Status          11.079980 4.9998442
18            Card_Category           5.694400 4.8870951
19          Income_Category           3.357699 6.5292501
----------------------------------------
```

### Analysis on the reduced dataset

I took away the variables non considered also in the other models, as variable importance may not be an indicative value of which variables are viable to be taken away.

>[!NOTE]
> Let me know if it may be sensible to remove the least important variables according to the ensemble methods, as they are different (I would take away the ones with mean gini decrease < 1(or 2) for boosting and <10(or 20) for randomforest)!!.

### AdaBoost with static train-test division

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     1673       27
  Attrited       55      270
----------------------------------------
Accuracy: 95.95 %
Dummy classifier accuracy: 83.95 %
----------------------------------------
AUC: 98.4 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 1.59 %
FNR: 16.92 %
----------------------------------------
Variable importance:
                   Variable Mean_Gini_Decrease
1            Total_Trans_Ct         32.1290080
2           Total_Trans_Amt         28.0619350
3       Total_Ct_Chng_Q4_Q1         11.6521977
4       Total_Revolving_Bal         11.4968680
5  Total_Relationship_Count          6.9770316
6     Contacts_Count_12_mon          3.9556203
7    Months_Inactive_12_mon          3.6111736
8                    Gender          0.8720491
9            Marital_Status          0.8677845
10          Income_Category          0.3763321
----------------------------------------
```

### AdaBoost with 10-fold cross validation

I used the same parameters as before, but I used a 10-fold cross validation.
```terminal
----------------------------------------
Average accuracy: 95.49 +/- 0.53 %
----------------------------------------
Average AUC: 98.55 +/- 0.21 %
----------------------------------------
Average FPR: 2.27 +/- 0.56 %
Average FNR: 16.22 +/- 1.99 %
----------------------------------------
Average variable importance ranking:
                   Variable Mean_Gini_Decrease    Std_Dev
1            Total_Trans_Ct         35.2607130 1.51321544
2           Total_Trans_Amt         27.1277284 0.63385074
3       Total_Revolving_Bal         12.3358661 0.66433659
4       Total_Ct_Chng_Q4_Q1         10.2410856 0.60937211
5  Total_Relationship_Count          6.8953816 0.57825762
6     Contacts_Count_12_mon          3.6713650 0.40844293
7    Months_Inactive_12_mon          2.7927028 0.31481083
8                    Gender          0.8494183 0.12389372
9            Marital_Status          0.6127634 0.18474584
10          Income_Category          0.2129757 0.08584478
----------------------------------------
```

### Random Forest with static train-test division

I used the `randomForest` package, and I fitted the model with default parameters.

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     1676       24
  Attrited       65      260
----------------------------------------
Accuracy: 95.6 %
Dummy classifier accuracy: 83.95 %
----------------------------------------
AUC: 98.14 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 1.41 %
FNR: 20 %
----------------------------------------
Variable importance:
                   Variable Mean_Gini_Decrease
8           Total_Trans_Amt         529.807126
9            Total_Trans_Ct         485.608407
7       Total_Revolving_Bal         393.019449
10      Total_Ct_Chng_Q4_Q1         318.191270
4  Total_Relationship_Count         205.084791
6     Contacts_Count_12_mon          90.515870
5    Months_Inactive_12_mon          85.966817
1                    Gender          35.993873
2            Marital_Status          23.708807
3           Income_Category           8.235691
----------------------------------------
```

### Random Forest with 10-fold cross validation

I used the same parameters as before, but I used a 10-fold cross validation.

```terminal
----------------------------------------
Average accuracy: 99.14 +/- 0.32 %
----------------------------------------
Average AUC: 99.79 +/- 0.11 %
----------------------------------------
Average FPR: 0.29 +/- 0.14 %
Average FNR: 3.93 +/- 1.96 %
----------------------------------------
Average variable importance ranking:
                   Variable Mean_Gini_Decrease   Std_Dev
1           Total_Trans_Amt         517.792419 0.5958843
2            Total_Trans_Ct         486.881318 0.2789695
3       Total_Revolving_Bal         393.766226 0.2129571
4       Total_Ct_Chng_Q4_Q1         329.641423 1.8840771
5  Total_Relationship_Count         201.542137 0.9021276
6     Contacts_Count_12_mon          89.244495 0.8485140
7    Months_Inactive_12_mon          85.594244 4.4140412
8                    Gender          35.987968 5.0163658
9            Marital_Status          24.198076 5.2970795
10          Income_Category           8.085316 5.1896773
----------------------------------------
```

**NB: my code is still really verbose and has much more computation than required, but it is just as a backup and validation to see if "manually-computed" coefficients were consistent with the ones given from the libraries.**
**TODO: Fix Dummy AUC (50% doesn't seem right)**


## ROSE package to deal with class imbalance
In this section we will explore how ROSE package might impact on the performance of all the models implemented above

>[!NOTE]
> The ROSE package has been tested in  `r_scripts/testing_ROSE/` 

There was an evident imbalance among the target variable's classes:
```R
>>table(bank$Attrition_Flag)
  0    1 
8500 1627 
```
- Attrited customers (1): 1627
- Existing customers (0): 8500
- Attrited customers proportion: 16.06596 %

A new (synthetic) dataset was obtained by applying ROSE package, as follows:
```R
>>library(ROSE)
>>bank_balanced<- ROSE(Attrition_Flag~.,data=bank,seed = 123)$data
>>table(bank$Attrition_Flag)
  0    1 
5123 5004 
```
- Attrited customers (1): 5004
- Existing customers (0): 5123
- Attrited customers proportion: 49.41246 %

### ROSE results for Logistic regression
>[!NOTE]
> Look at  `r_scripts/testing_ROSE/ROSE_logistic_regression.R` 

Single-run result
```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     4238      885
  Attrited      875     4129
----------------------------------------
Accuracy: 82.62 %
Dummy classifier accuracy: 50.59 %
----------------------------------------
AUC: 90.27 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 17.28 %
FNR: 17.49 %
----------------------------------------
AIC: 8036.421 
BIC: 8137.542 
----------------------------------------
```

10 fold CV result:
```terminal
----------------------------------------
Average accuracy: 82.57 +/- 1.49 %
----------------------------------------
Average AUC: 90.2 +/- 0.87 %
----------------------------------------
Average FPR: 17.41 +/- 2.87 %
Average FNR: 17.51 +/- 2.19 %
----------------------------------------
Average AIC: 7233.987 +/- 33.24618 
Average BIC: 7333.634 +/- 33.24607 
----------------------------------------
```

### ROSE results for Penalized Regression
>[!NOTE]
> Look at  `r_scripts/testing_ROSE/ROSE_penalized_regression.R`

Results for RIDGE regression:

```terminal
----------------------------------------
Average accuracy: 81.43 +/- 1.22 %
----------------------------------------
Average AUC: 89.17 +/- 1.08 %
----------------------------------------
Average FPR: 17.73 +/- 1.99 %
Average FNR: 19.5 +/- 1.43 %
----------------------------------------
```

Results for LASSO regression:

```terminal
----------------------------------------
Average accuracy: 81.47 +/- 1.07 %
----------------------------------------
Average AUC: 89.53 +/- 1.2 %
----------------------------------------
Average FPR: 18.42 +/- 1.93 %
Average FNR: 18.72 +/- 1.47 %
----------------------------------------
```

### ROSE Results for Splines
>[!NOTE]
> Look at  `r_scripts/testing_ROSE/ROSE_splines.R`

**gamfit with static train/test division**

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     4307      816
  Attrited      694     4310
----------------------------------------
Accuracy: 85.09 %
Dummy classifier accuracy: 50.59 %
----------------------------------------
AUC: 92.94 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 15.93 %
FNR: 13.87 %
----------------------------------------
AIC: 6993.906 
BIC: 7493.183 
----------------------------------------
```

**gamfit_less_vars with static train/test division**

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing     4279      844
  Attrited      735     4269
----------------------------------------
Accuracy: 84.41 %
Dummy classifier accuracy: 50.59 %
----------------------------------------
AUC: 92.32 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 16.47 %
FNR: 14.69 %
----------------------------------------
AIC: 7223.264 
BIC: 7503.034 
----------------------------------------
```

**gamfit with CV**

```terminal
----------------------------------------
Average accuracy: 84.68 +/- 1.75 %
----------------------------------------
Average AUC: 92.52 +/- 0.93 %
----------------------------------------
Average FPR: 16.75 +/- 3.27 %
Average FNR: 13.98 +/- 1.81 %
----------------------------------------
Average AIC: 6299.932 +/- 40.04351 
Average BIC: 6778.618 +/- 44.47466 
----------------------------------------
```

**gamfit_less_vars with CV**

```terminal
----------------------------------------
Average accuracy: 84.12 +/- 1.36 %
----------------------------------------
Average AUC: 92.11 +/- 0.93 %
----------------------------------------
Average FPR: 17.13 +/- 3.12 %
Average FNR: 14.72 +/- 1.62 %
----------------------------------------
Average AIC: 6505.484 +/- 39.65097 
Average BIC: 6772.415 +/- 39.65286 
----------------------------------------
```


### ROSE Results for Ensamble Methods
>[!NOTE]
> Look at  `r_scripts/testing_ROSE/ROSE_ensamble.R` 
> EVALUTATION OF MODELS WITH REDUCED ATTRIBUTES NOT DONE YET

**(1) AdaBoost with static train/test division**

```terminal
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing      889      135
  Attrited      143      857
----------------------------------------
Accuracy: 86.26 %
Dummy classifier accuracy: 50.59 %
----------------------------------------
AUC: 94.27 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 13.18 %
FNR: 14.3 %
----------------------------------------
Variable importance:
                   Variable Mean_Gini_Decrease
1            Total_Trans_Ct        30.73697237
2       Total_Revolving_Bal        11.78081861
3           Total_Trans_Amt         9.92451533
4  Total_Relationship_Count         6.53934880
5    Months_Inactive_12_mon         5.67923580
6      Total_Amt_Chng_Q4_Q1         5.45173144
7       Total_Ct_Chng_Q4_Q1         5.36548361
8     Contacts_Count_12_mon         4.80681664
9            Months_on_book         4.27897800
10    Avg_Utilization_Ratio         3.10059310
11             Customer_Age         2.74046246
12             Credit_Limit         2.47675632
13          Avg_Open_To_Buy         1.96121788
14          Dependent_count         1.67558174
15          Education_Level         1.40795148
16                   Gender         1.28538718
17            Card_Category         0.35993893
18           Marital_Status         0.35973450
19          Income_Category         0.06847582
----------------------------------------
```

**(2) Random Forest with static train/test division**

```terminal 
----------------------------------------
          Predicted
Actual     Existing Attrited
  Existing      895      129
  Attrited      122      878
----------------------------------------
Accuracy: 87.6 %
Dummy classifier accuracy: 50.59 %
----------------------------------------
AUC: 94.47 %
Dummy classifier AUC: 50 %
----------------------------------------
FPR: 12.6 %
FNR: 12.2 %
----------------------------------------
Variable importance:
                   Variable Mean_Gini_Decrease
17           Total_Trans_Ct          900.08843
13      Total_Revolving_Bal          495.51746
18      Total_Ct_Chng_Q4_Q1          424.42724
16          Total_Trans_Amt          311.82450
9  Total_Relationship_Count          274.31340
11    Contacts_Count_12_mon          210.51677
15     Total_Amt_Chng_Q4_Q1          191.69802
19    Avg_Utilization_Ratio          189.09073
10   Months_Inactive_12_mon          175.51852
8            Months_on_book          144.87180
1              Customer_Age          139.52477
14          Avg_Open_To_Buy          135.38430
12             Credit_Limit          128.35728
3           Dependent_count          124.38720
4           Education_Level          116.00039
2                    Gender           42.53218
5            Marital_Status           20.06260
7             Card_Category           17.94270
6           Income_Category            8.24609
----------------------------------------
```

**(3) AdaBoost with CV**

```terminal
----------------------------------------
Average accuracy: 87.25 +/- 0.86 %
----------------------------------------
Average AUC: 94.57 +/- 0.8 %
----------------------------------------
Average FPR: 13.22 +/- 1.3 %
Average FNR: 12.27 +/- 1.11 %
----------------------------------------
Average variable importance ranking:
                   Variable Mean_Gini_Decrease    Std_Dev
1            Total_Trans_Ct        34.75402277 1.91066726
2       Total_Revolving_Bal        12.94150229 1.37714803
3           Total_Trans_Amt         8.86350309 0.58198055
4  Total_Relationship_Count         7.21951758 0.38540495
5       Total_Ct_Chng_Q4_Q1         6.38904043 0.32115247
6    Months_Inactive_12_mon         5.56077118 0.24868059
7      Total_Amt_Chng_Q4_Q1         5.03931768 0.42429973
8     Contacts_Count_12_mon         4.52194348 0.23543461
9            Months_on_book         3.31371989 0.55059750
10    Avg_Utilization_Ratio         2.42278591 0.23323968
11             Customer_Age         1.94948740 0.29291633
12          Dependent_count         1.57882847 0.24071690
13          Avg_Open_To_Buy         1.39104697 0.24696721
14          Education_Level         1.26301133 0.21294553
15                   Gender         1.18618812 0.21157629
16             Credit_Limit         1.00167942 0.20688228
17           Marital_Status         0.36009097 0.07319159
18            Card_Category         0.20161414 0.10068651
19          Income_Category         0.04192886 0.04021035
----------------------------------------
```

**(4) Random Forest with CV**

```terminal
----------------------------------------
Average accuracy: 87.4 +/- 0.94 %
----------------------------------------
Average AUC: 94.59 +/- 0.55 %
----------------------------------------
Average FPR: 13.27 +/- 1.64 %
Average FNR: 11.98 +/- 1.38 %
----------------------------------------
Average variable importance ranking:
                   Variable Mean_Gini_Decrease    Std_Dev
1            Total_Trans_Ct        1007.348407  1.1698639
2       Total_Revolving_Bal         563.033669  2.4616142
3       Total_Ct_Chng_Q4_Q1         469.475970  2.3207382
4           Total_Trans_Amt         364.184981  1.7972846
5  Total_Relationship_Count         300.915929  0.9388124
6     Contacts_Count_12_mon         230.128678  0.3218984
7      Total_Amt_Chng_Q4_Q1         218.784653  1.1144787
8     Avg_Utilization_Ratio         215.417326  1.3194480
9    Months_Inactive_12_mon         198.560604  4.6139068
10           Months_on_book         160.174048  5.4049504
11             Customer_Age         156.434189  4.8913729
12          Avg_Open_To_Buy         149.690026  1.3836461
13             Credit_Limit         145.677997  9.7837391
14          Dependent_count         140.642761  1.0893933
15          Education_Level         128.893361  3.8013993
16                   Gender          52.817680  9.8382519
17           Marital_Status          23.393058 16.5777671
18            Card_Category          21.044929  8.8143522
19          Income_Category           9.318932  3.9632489
----------------------------------------
```