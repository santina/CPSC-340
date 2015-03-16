# Assignment 4 : SVM and cross-validation on text data 
# Santina Lin (87325149)

# important libraries: 
library(tm)         # for text mining 
library(SnowballC)  # for using stemmming, finding common roots of words 
library(plyr)       # for easy computation with data frames
library(dplyr)      # do this after loading plyr
library(e1071)      # for SVM

# this contains Corpus processing, get vector of classes, and confusion matrix
source('./helperFunctions.R')

# ====================== Part 1: create a SVM classifier ====================== 

#' LOAD THE DATASET 
#'loop through each folder in training, create vector for classes depending on file name, 
#'also Corpus for each foldrer, process each, 

# get the list of file names 
training_files <- list.files("20news-bydate-train", full.name = TRUE)
testing_files  <- list.files("20news-bydate-test", full.name = TRUE)

# get their classes
trainClass  <- getClasses(training_files) #11314 files in total 
testClass  <- getClasses(testing_files)   #7532 files in total
# you must absolutely do this otherwise SVM won't work. 0 and 1 must be read as factors
train_classes <- as.factor(trainClass)
test_classes <- as.factor(testClass)

# loop through each, store corpus in list for training data
trainCorpusList <- alply(training_files, .margin = 1, function(f){
  singleCorpus <- Corpus(DirSource(f,  encoding="UTF-8"))
})
# same thing, for testing data 
testCorpusList <- alply(testing_files, .margin = 1, function(f){
	singleCorpus <- Corpus(DirSource(f,  encoding="UTF-8"))
})
#' reason I did this instead of loading it like DirSource(testing_files)
#' I want to make sure the order remain the same so I can use the classes vector 
#' correctly. Loading it like that might mess up the order 

# 20 elements, 91Mb wow. Now give name to each element in the list 
names(trainCorpusList) <- list.files("20news-bydate-train", full.name = FALSE)
names(testCorpusList) <- list.files("20news-bydate-test", full.name = FALSE)

trainCorpusList <- processCorpusList(trainCorpusList) # 20
testCorpusList <- processCorpusList(testCorpusList)   # 20
togetherCorpus  <- c(trainCorpusList, testCorpusList)


# check if it works, to extract out the element, use double bracket [[]]
# inspect(trainCorpusList[["talk.religion.misc"]]) 

# combine all corpuses togeter into one big one
allCorpus_train <- do.call(function(...) c(..., recursive = TRUE), trainCorpusList)
allCorpus_test <- do.call(function(...) c(..., recursive = TRUE), testCorpusList)
allCorpus  <- do.call(function(...) c(..., recursive = TRUE), togetherCorpus)


# then do document term freq and remove sparse term on them 
# (discussion board) I should prob combine the train and test together and then make DTM 
# and remove sparse term and separate them into their own dat frames so 
# that there won't be diferet numbers of terms 

# DTM_train <- DocumentTermMatrix(allCorpus_train,control=list(stemming = TRUE,weighting = weightTfIdf))
# DTM_test <- DocumentTermMatrix(allCorpus_test,control=list(stemming = TRUE,weighting = weightTfIdf))

DTM_all <- DocumentTermMatrix(allCorpus,control=list(stemming = TRUE,weighting = weightTfIdf))
# row: 18846 (number of documents) col = 104532 number of terms, sparsity 100%

DTM_all2 <- removeSparseTerms(DTM_all, sparse=0.91) 
# col = 120

DTM_all2 <- as.data.frame(inspect(DTM_all2))
DTM_all2_train <- DTM_all2[1:11314, ] 
DTM_all2_test  <- tail(DTM_all2, 7532)
# 11314: number of files in training set


# SVM classifications, train on the training set and its classes
SVM <- svm(DTM_all2_train,train_classes,kernel="linear") 

PredictionSVM <- predict(SVM,DTM_all2_test)
predictionSummary <- table(PredictionSVM,test_classes)
#             test_classes
#PredictionSVM    0    1
#             0 5268 1299
#             1  309  656

prop.table(table(test_classes==PredictionSVM))  # 83% accuracy

TN <- predictionSummary[1,1]
FN <- predictionSummary[1,2]
FP <- predictionSummary[2,1]
TP <- predictionSummary[2,2]


#I'll treat 1 (comp) as positive 
specificity <- TN/(TN+FP)  #0.9315044
sensitivity <- TP/(TP+FN)  #0.4736573

# ================= PART 2: 5-Fold Cross Validation ====================

# random seed for this class
set.seed(340)

# DTM_all2 is the DTM (with sparse term removed) that has all the data (train + test )
# train_classes and test_classes correspond to the first 11314 and the remining
# (in order ) of the data

# bind the data frame with all data and their classes (0 or 1) together 

DTM_all2$class <- factor(c(trainClass, testClass))

# shuffle the rows 
DTM_all2_shuffled <- DTM_all2[sample(nrow(DTM_all2)), ]

# split into 5 parts 
size  <- nrow(DTM_all2_shuffled)/5 
# 3769.2, doesn't divide evenly, make last set with 3770 
size <- round(nrow(DTM_all2_shuffled)/5)

set1 <- head(DTM_all2_shuffled, size)
set2 <- DTM_all2_shuffled[(size+1):(size*2), ]
set3 <- DTM_all2_shuffled[(size*2+1):(size*3), ]
set4 <- DTM_all2_shuffled[(size*3+1):(size*4), ]
set5 <- tail(DTM_all2_shuffled, 3770)

# the next 5 lines take forever. 
table1 <- getConfusionMatrix_SVM(set1, set2, set3, set4, set5)
table2 <- getConfusionMatrix_SVM(set2, set1, set3, set4, set5)
table3 <- getConfusionMatrix_SVM(set3, set2, set1, set4, set5)
table4 <- getConfusionMatrix_SVM(set4, set2, set3, set1, set5)
table5 <- getConfusionMatrix_SVM(set5, set2, set3, set4, set1)

# sum up the tables 
summary <- data.frame()
summary[1,1] <- table1[1,1] + table2[1,1] + table3[1,1] + table4[1,1] + table5[1,1]
summary[1,2] <- table1[1,2] + table2[1,2] + table3[1,2] + table4[1,2] + table5[1,2]
summary[2,1] <- table1[2,1] + table2[2,1] + table3[2,1] + table4[2,1] + table5[2,1]
summary[2,2] <- table1[2,2] + table2[2,2] + table3[2,2] + table4[2,2] + table5[2,2]
colnames(summary) <- c(0,1) 
rownames(summary) <- c(0,1)

TN_CV <- summary[1,1]
FN_CV <- summary[1,2]
FP_CV <- summary[2,1]
TP_CV <- summary[2,2]


#I'll treat 1 (comp) as positive 
specificity_CV <- TN_CV/(TN_CV+FP_CV)  # 0.9330706
sensitivity_CV <- TP_CV/(TP_CV+FN_CV)  # 0.4745451
