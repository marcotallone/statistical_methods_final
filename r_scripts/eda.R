# Exploratory data analysis (EDA)

#-------------------------------------------------------------------------------
# IMPORT PACKAGES AND DATA
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(ggplot2)
library(corrplot)
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")
#-------------------------------------------------------------------------------
# DATA PREPARATION

# Remove the last two columns as the README suggested and CLIENTNUM (useless)
bank <- bank[, -c(1,22, 23)]

# Check presence of NA (or empty field)
sum(is.na(bank)) #--->0 NA
sum(bank=="")    #--->0 empty 

# encode as factor all categorical variables
for(attribute in colnames(bank)){
  if(is.character(bank[,attribute])){
    bank[,attribute] <- as.factor(bank[,attribute] )
  }
}
#-------------------------------------------------------------------------------
# EXPLORATORY DATA ANALYSIS

# (1) How the target variable is distributed among classes?
table(bank$Attrition_Flag)

barplot(table(bank$Attrition_Flag),col=c("red4","green4",width=0.5),
        main="Attrition flag barplot",border = "black",space = 0.5)

ggplot(bank,aes(x=Attrition_Flag,fill=Attrition_Flag))+
  geom_bar(width = 0.5, colour="black")+
  ggtitle("Attrition flag distribution among classes")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

cat("Imbalance rate = ",
    sum(bank$Attrition_Flag=="Attrited Customer")/sum(bank$Attrition_Flag=="Existing Customer"))
# Comment: it might be worth to try ROSE package to deal with class imbalance!

# (2) See how target variable behaves in different levels of some regressors

ggplot(bank,aes(x=Customer_Age,fill=Attrition_Flag))+
  geom_bar(width = 0.5, colour="black")+
  ggtitle("Customer Age by attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank,aes(x=Dependent_count,fill=Attrition_Flag))+
  geom_bar(width = 0.5, colour="black")+
  ggtitle("Dependent_count by attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank,aes(x=Months_on_book,fill=Attrition_Flag))+
  geom_bar(width = 0.5, colour="black")+
  ggtitle("Months_on_book by attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank,aes(x=Total_Relationship_Count,fill=Attrition_Flag))+
  geom_bar(width = 0.5, colour="black")+
  ggtitle("Total_Relationship_Count by attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank,aes(x=Months_Inactive_12_mon,fill=Attrition_Flag))+
  geom_bar(width = 0.5, colour="black")+
  ggtitle("Months_Inactive_12_mon by attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank, aes(x=Total_Trans_Amt, color=Attrition_Flag,fill=Attrition_Flag)) +
  geom_histogram(alpha=0.5, position="dodge",colour="black")+
  ggtitle("Total_Trans_Amt and attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank, aes(x=Total_Trans_Ct, color=Attrition_Flag,fill=Attrition_Flag)) +
  geom_histogram(alpha=0.5, position="dodge",bins =40,colour="black",stat = )+
  ggtitle("Total_Trans_Ct and attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank, aes(x=Total_Ct_Chng_Q4_Q1, color=Attrition_Flag,fill=Attrition_Flag)) +
  geom_histogram(alpha=0.5, position="dodge",bins =40,colour="black")+
  ggtitle("Total_Ct_Chng_Q4_Q1 and attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank, aes(x=Avg_Utilization_Ratio, color=Attrition_Flag,fill=Attrition_Flag)) +
  geom_histogram(alpha=0.5, position="dodge",bins =40,colour="black")+
  ggtitle("Avg_Utilization_Ratio and attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bank, aes(x=Credit_Limit, color=Attrition_Flag,fill=Attrition_Flag)) +
  geom_histogram(alpha=0.5, position="dodge",bins =40,colour="black")+
  ggtitle("Credit_Limit and attrition flag")+
  scale_fill_manual(values=c("red4","green4"))+
  theme(plot.title = element_text(hjust = 0.5))

# (3) Inspect correlation among regressors
# We can compute it only for numerical vars
numerical_attr= c()
for(attribute in colnames(bank)){
  if(is.numeric(bank[,attribute])){
    numerical_attr<- c(numerical_vars,attribute)
  }
}
corrplot(cor(bank[,numerical_attr]),type="upper",method="color", 
         addgrid.col = "black",tl.col="black",tl.cex = 0.7,addCoef.col = "black",
         number.cex=0.7,col = COL1("Greens"), order="alphabet")
#-------------------------------------------------------------------------------