# Customer Churn assignment answer
# Student name : Vajira Viduranga


library("purrr")
library(class)
library(gmodels)

# Read data from csv file
CustomerChurn = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, stringsAsFactors = TRUE)
str(CustomerChurn)

# Check N/A value containing data rows
# Removing 11 data rows that containing N/A data values to ease numeric calculations.
# Romoval of 11(inclomple) data records is not much affecting to the data set because data set is contain more than 7000 data records.
row.has.na <- apply(CustomerChurn,MARGIN = 1, function(x){any(is.na(x))})
sum(row.has.na)
CustomerChurn_cleaned =  CustomerChurn[!row.has.na,]

str(CustomerChurn_cleaned)

# Column vise Converting string data from factors to numeric data type to enable this dataset for numeric calculations
cols = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);    
CustomerChurn_cleaned[,cols] = apply(CustomerChurn_cleaned[,cols], MARGIN =2, FUN= function(x){as.numeric(as.factor(x))})


# Deviding data set into two samples 
# containing 80% data in training sample and 20% in testing sample for applying KNN algorithm
# Removing "Customer ID" data from train and test data sets since it is not impacting to churn outcome 
# Removing "churn" column data from train and test data sets and using "churn" column(21st column) data as testing label for KNN algorithm
index = sample(2, nrow(CustomerChurn_cleaned), replace=TRUE, prob=c(0.8,0.2))

CustomerChurn.train = CustomerChurn_cleaned[index==1, 2:20]
CustomerChurn.test = CustomerChurn_cleaned[index==2, 2:20]

CustomerChurn.trainLabel = CustomerChurn_cleaned[index==1, 21]
CustomerChurn.testLabel = CustomerChurn_cleaned[index==2, 21]

# Use PCA algorithm for dimension reduction
# Converting both train and test data using PCA algorithm
CustomerChurn.train.pca = prcomp(CustomerChurn.train, center = TRUE, scale. = TRUE)
CustomerChurn.test.pca = predict(CustomerChurn.train.pca, newdata = CustomerChurn.test)

screeplot(CustomerChurn.train.pca, type="lines")


# Applying KNN algorithm for different K values
# Based on elbow principle, only using 1 to 4 PC values from "pca" test results to KNN algorithm 

set.seed(100)

CustomerChurn.pred.pca.knn3 = knn(train = CustomerChurn.train.pca$x[,1:4], test=CustomerChurn.test.pca[,1:4], cl=CustomerChurn.trainLabel, k=3 )
CustomerChurn.pred.pca.knn5 = knn(train = CustomerChurn.train.pca$x[,1:4], test=CustomerChurn.test.pca[,1:4], cl=CustomerChurn.trainLabel, k=5 )
CustomerChurn.pred.pca.knn10 = knn(train = CustomerChurn.train.pca$x[,1:5], test=CustomerChurn.test.pca[,1:5], cl=CustomerChurn.trainLabel, k=10 )
CustomerChurn.pred.pca.knn15 = knn(train = CustomerChurn.train.pca$x[,1:4], test=CustomerChurn.test.pca[,1:4], cl=CustomerChurn.trainLabel, k=15 )
CustomerChurn.pred.pca.knn20 = knn(train = CustomerChurn.train.pca$x[,1:5], test=CustomerChurn.test.pca[,1:5], cl=CustomerChurn.trainLabel, k=20 )
CustomerChurn.pred.pca.knn23 = knn(train = CustomerChurn.train.pca$x[,1:4], test=CustomerChurn.test.pca[,1:4], cl=CustomerChurn.trainLabel, k=23 )
CustomerChurn.pred.pca.knn25 = knn(train = CustomerChurn.train.pca$x[,1:4], test=CustomerChurn.test.pca[,1:4], cl=CustomerChurn.trainLabel, k=25 )
CustomerChurn.pred.pca.knn26 = knn(train = CustomerChurn.train.pca$x[,1:4], test=CustomerChurn.test.pca[,1:4], cl=CustomerChurn.trainLabel, k=26 )
CustomerChurn.pred.pca.knn30 = knn(train = CustomerChurn.train.pca$x[,1:5], test=CustomerChurn.test.pca[,1:5], cl=CustomerChurn.trainLabel, k=30 )



# Testing acuracy levels for eack K values
table(CustomerChurn.pred.pca.knn3, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn3 == CustomerChurn.testLabel) # Acuracy level = 0.76

table(CustomerChurn.pred.pca.knn5, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn5 == CustomerChurn.testLabel) # Acuracy level = 0.77

table(CustomerChurn.pred.pca.knn10, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn10 == CustomerChurn.testLabel) # Acuracy level = 0.78

table(CustomerChurn.pred.pca.knn15, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn15 == CustomerChurn.testLabel) # Acuracy level = 0.788

table(CustomerChurn.pred.pca.knn20, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn20 == CustomerChurn.testLabel) # Acuracy level = 0.78

table(CustomerChurn.pred.pca.knn23, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn23 == CustomerChurn.testLabel) # Acuracy level = 0.793

table(CustomerChurn.pred.pca.knn25, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn25 == CustomerChurn.testLabel) # Acuracy level = 0.797

table(CustomerChurn.pred.pca.knn26 ,CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn26 == CustomerChurn.testLabel) # Acuracy level = 0.794

table(CustomerChurn.pred.pca.knn30, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn30 == CustomerChurn.testLabel) # Acuracy level = 0.788



# K = 25 gives highest acuracy level 79.4%
table(CustomerChurn.pred.pca.knn25, CustomerChurn.testLabel)
mean(CustomerChurn.pred.pca.knn25 == CustomerChurn.testLabel)
CrossTable(x = CustomerChurn.testLabel, y = CustomerChurn.pred.pca.knn25, prop.chisq=FALSE)
