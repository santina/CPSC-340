#CS340 Assignment 2, Oct 15. Santina Lin (87325149)

library(SnowballC)
library(dplyr)
library(rpart) #need this for classification, decision tree
library(rpart.plot) # to make pretty tree, thanks to Jonathan Stiansen's suggestion
library(ggplot2)

#for calculating TP and TN and making the confusion matrix more clear 
source('./helperFuns_A2_SantinaLin.R')



#=======1. Load census training dataset ============================
#training data, full and 50% 
trainingData<- read.table("2014CensusTraining.csv", sep = ",", header = TRUE)



#=======2. Make a decision tree F5 with first 5 attributes==========
# . two classes are people making >50K or less, column name : class

#predictors #to see the predictors we can work with 
predictors <- names(trainingData)

#create decision tree 
tree_F5 <- rpart(class ~ workclass + age + fnlwgt + education + 
                    education.num,trainingData,method="class")
prp(tree_F5) #plot a pretty tree
#dt_table  <- table (test_df_withclass$classification,prediction)


#=======3. Make a decision tree F10 with first 10 attributes========

#first 10 columns (predictors) and the 15th (class)
trainingData_F10 <- trainingData[,c(1:10,15)] 
#make and plot a pretty tree
tree_F10 <- rpart(class ~ . ,trainingData_F10,method="class")
prp(tree_F10) 



#=======4. Make a decision tree F14 with all 14 attributes==========
# . means include all the terms in making the model
tree_F14 <- rpart(class ~ . ,trainingData,method="class")
prp(tree_F14) 



#=======5. Make a decision tree H5, H10, H14 with half data ========
training50  <- read.table("2014HalfCensusTraining.csv", sep = ",", header = TRUE)

#H5 
tree_H5 <- rpart(class ~ workclass + age + fnlwgt + education + 
                   education.num,training50,method="class")
#H10
training50_H10 <- trainingData[,c(1:10,15)] 
tree_H10 <- rpart(class ~ . ,training50_H10,method="class")
#H14 
tree_H14 <- rpart(class ~ . ,training50,method="class")



#=======6. Load the census test set=================================
#testing data 
testingData <- read.table("2014NewCensusTest.csv", sep = ",", header = TRUE)


#=======7. Compute sensitivity and psecificty for all trees ========
# I'm defining positive as people who full under <=50K category 

# Gonna store everything in one data frame 
trees <- list(tree_F5, tree_F10, tree_F14, tree_H5, tree_H10, tree_H14)
names <- c("F5", "F10", "F14", "H5", "H10", "H14")
predictionQuality <-  computeAllQualities(trees, testingData, names)

#inspect the results 
predictionQuality 

#make a histo graph of 'predictionQuality' 
p <- mutate(predictionQuality, tree=rownames(predictionQuality))
p <- melt(p, id="tree")
ggplot(data=p, aes(x=tree, y=value, fill=variable)) +
geom_bar(stat="identity", position=position_dodge(), colour="black")


#====================================================================

