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
* [ ] Add Contributors
* [ ] ...

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
| 2 | `Attrition_Flag` | Internal event (customer activity) variable - if the account is closed then 1 else 0 | 🔀 |
| 3 | `Costumer_Age` | Demographic variable - Customer's Age in Years | 🔢 |
| 4 | `Gender` | Demographic variable - M=Male, F=Female | 🆎 |
| 5 | `Dependent Count` | Demographic variable - Number of dependents | 🔢 |
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

## Importing the dataset

To import and use the dataset in an R script or R Markdown file, use the following code.

```r
# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset from the datasets/ folder
bank <- read.csv("path/to/datasets/BankChurners.csv", sep = ",")

# ⚠️ Remove the last two columns as suggested in the README
bank <- bank[, -c(22, 23)]
```
