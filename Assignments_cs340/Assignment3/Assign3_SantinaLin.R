
# Assignment 3, CS340, Nov 5
# Santina Lin  87325149 

#' Disclaimer : some codes in helper fucntion file 
#' were copied/paste and then modified 
#' from the tutorial 4 script
#' but I fully understand how they work and they're commented. 

test_data <- read.csv("Assign3Test.csv", header = TRUE)
true_data<- read.csv('Assign3TrueValues.csv', header = TRUE)
data_missingloan <- read.csv("Assign3trainMissingValues.csv", header = TRUE)

# load helper functions and important packages (some are for the R markdown)
source("helper_functions_A3_SantinaLin.R")
library(dplyr)   #for for loop like functions
library(plyr)
library(ggplot2)  # for making plots
library(knitr)    # for making table
library(grid)
library(gridExtra)  # arranging graph


# ====== Part 1 : Classification with missing value samples removed =============

# extract rows that do not have NAs
cleaned_data <- data_missingloan[complete.cases(data_missingloan),]

# train a model on the cleaned data, (generalized linear model)
MClean <- glm(default10yr ~ income + age + loan, data = cleaned_data, family="binomial")
summary(MClean)

# Now we need to find cutoff for the classification to 
# calculate sensitivity and psecificity

# using the function  from tutorial4 written by one of the TAs
  findOptimalCutOff(MClean, cleaned_data)
# The intersection lies around 0.2, so we will use it as the optimal cutoff value.

# test the model on the testing data with 0.2 optimal cutoff 
matrix_MClean  <- getConfusionMatrix(MClean, test_data, 0.2)
getAccuracy(matrix_MClean) # 0.936
getSensitivity(matrix_MClean) # 0.9832536
getSpecificity(matrix_MClean) # 0.695122

# our model is 0.936 accurate. 


# ============ Part 2: Missing Value Imputation =====================

# train models based on income, age, and both 
RAge <-  lm(loan ~age, data = cleaned_data)
RIncome <- lm(loan ~income, data = cleaned_data)
RBoth <- lm(loan ~age+income, data = cleaned_data)

# Find the optimal cutoff for each of the three models: 
#findOptimalCutOff(RAge, cleaned_data) # 0.2 
#findOptimalCutOff(RIncome, cleaned_data) # 0.15 
#findOptimalCutOff(RBoth, cleaned_data) # 0.2 

# get subset of data that has loan attibute missing : 
na_data <- data_missingloan[!complete.cases(data_missingloan),]
# get the client ids from the na_data
client_ids <- na_data$clientid

# subset the true data that's client id 
true_data_subset  <- adply(client_ids, .margin = 1, function(id){
  val  <- true_data[true_data$clientid == id, ]
})

#' Make prediction on missing values
#' and change names to corresponding ids
#' calculate total error 


RAge_prediction <- predict(RAge, newdata=na_data,type="response")
names(RAge_prediction)  <- client_ids
error_RAge <- getTotalError(client_ids, RAge_prediction, true_data_subset)
  # 2105872839

RIncome_prediction <- predict(RIncome, newdata=na_data,type="response")
names(RIncome_prediction)  <- client_ids
error_RIncome <- getTotalError(client_ids, RIncome_prediction, true_data_subset)
  # 1657606433

RBoth_prediction <- predict(RBoth, newdata=na_data,type="response")
names(RBoth_prediction)  <- client_ids
error_RBoth <- getTotalError(client_ids, RBoth_prediction, true_data_subset)
  # 1662160777

# ===== Part 3: Classification with imputed missing values =================

#' adding imputed values to na_data dataset 
#' bind  the resulted data frame with cleaned data dataset 

na_data_wImputed_RAge  <- mutate(na_data, loan = as.vector(RAge_prediction))
data_missingloan_imputed_RAge  <- rbind(na_data_wImputed_RAge, cleaned_data)

na_data_wImputed_RIncome <- mutate(na_data, loan = as.vector(RIncome_prediction))
data_missingloan_imputed_RIncome  <- rbind(na_data_wImputed_RIncome, cleaned_data)

na_data_wImputed_RBoth <- mutate(na_data, loan = as.vector(RBoth_prediction))
data_missingloan_imputed_RBoth  <- rbind(na_data_wImputed_RBoth, cleaned_data)

# re- train a logistic regression model for each and calculate specificity/sensitivity
# I'm using default10yr = 0  as positive 

MAge <- glm(default10yr ~ income + age + loan, 
               data = data_missingloan_imputed_RAge
               , family="binomial")
findOptimalCutOff(MAge, data_missingloan_imputed_RAge) # 0.2
matrix_MAge <- getConfusionMatrix(MAge, test_data, 0.2)
sensitivity_MAge <- getSensitivity(matrix_MAge) # 0.9924812
specificity_MAge <- getSpecificity(matrix_MAge) # 0.6039604


MIncome <- glm(default10yr ~ income + age + loan, 
               data = data_missingloan_imputed_RIncome
               , family="binomial")
findOptimalCutOff(MIncome, data_missingloan_imputed_RIncome) # 0.2 
matrix_MIncome <- getConfusionMatrix(MIncome, test_data, 0.2)
sensitivity_MIncome <- getSensitivity(matrix_MIncome) # 0.9975186
specificity_MIncome <- getSpecificity(matrix_MIncome) # 0.6494845


MBoth <- glm(default10yr ~ income + age + loan, 
               data = data_missingloan_imputed_RBoth
               , family="binomial")
findOptimalCutOff(MBoth, data_missingloan_imputed_RBoth) # 0.2 
matrix_MBoth <- getConfusionMatrix(MBoth, test_data, 0.2)
sensitivity_MBoth <- getSensitivity(matrix_MBoth) # 0.9975186
specificity_MBoth <- getSpecificity(matrix_MBoth) # 0.6494845


# ===== Extra: summarizing everything ============= 
# Just gonna make a graph and and table about the goodness of all the models

MClean_goodness <- c(getAccuracy(matrix_MClean), 
                     getSensitivity(matrix_MClean), 
                     getSpecificity(matrix_MClean))

MBoth_goodness <- c(getAccuracy(matrix_MBoth), 
                     getSensitivity(matrix_MBoth), 
                     getSpecificity(matrix_MBoth))

MIncome_goodness <- c(getAccuracy(matrix_MIncome), 
                    getSensitivity(matrix_MIncome), 
                    getSpecificity(matrix_MIncome))

MAge_goodness <- c(getAccuracy(matrix_MAge), 
                    getSensitivity(matrix_MAge), 
                    getSpecificity(matrix_MAge))

# bind them into a table 
summary <- rbind(MClean_goodness, MBoth_goodness, MIncome_goodness, MAge_goodness)
colnames(summary) <- c("accuracy", "sensitivity", "specificity")
summary

# graph them
library(reshape)
summary2 <- melt(summary)
sensitivity_summary <- ggplot(data = summary2[summary2$X2 == "sensitivity", ], 
                               aes(x = X1, y = value)) + 
  geom_histogram(stat = "identity", alpha = 0.5)+ ggtitle("sensitivity")

specificity_summary <- ggplot(data = summary2[summary2$X2 == "specificity", ], 
                              aes(x = X1, y = value)) + 
  geom_histogram(stat = "identity") + ggtitle("specificity")


accuracy_summary <- ggplot(data = summary2[summary2$X2 == "accuracy", ], 
                              aes(x = X1, y = value)) + 
  geom_histogram(stat = "identity") +  ggtitle("accuracy")


# look into how to pass attribute into a functiond


