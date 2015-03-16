# Tutorial 6c , Nov 3 Monday
# Santina Lin 87325149 

set.seed(1)
library(SnowballC)
library(e1071)
library(tm)

# setwd("/Users/yashar/ML-R-CS340")
train<- read.table("epinions-train.csv", sep = ",", header = TRUE)
test<- read.table("epinions-test.csv", sep = ",", header = TRUE)

train_set <- train[sample(nrow(train), 200),]
test_set <- test[sample(nrow(test), 100),]

train_test_set <- rbind(train_set,test_set) 

# Preprocessing
train_test_corpus <- Corpus(VectorSource(train_test_set$text))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(tolower))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removePunctuation))
train_test_corpus <-  tm_map(train_test_corpus, content_transformer(removeWords) , c(stopwords('english')))
train_test_corpus <- tm_map(train_test_corpus, stripWhitespace)
DTM_train_test <- DocumentTermMatrix(train_test_corpus,control=list(stemming = TRUE,weighting = weightTfIdf))
train_test_dataframe <- as.data.frame(inspect( DTM_train_test ))
	# there's a lot of zeros. TA says it'd affect SVM 

classes <- c(train_test_set$class)
train_df_withoutclass <- head (train_test_dataframe,200)
test_df_withoutclass <- tail (train_test_dataframe,100)
train_classes <- factor(head (classes,200))
test_classes <- factor(tail(classes,100))



NB <- naiveBayes(train_df_withoutclass,train_classes,laplace = 1)
	# laplace: if a given attribute has a prob of 0, it won't affect naiveBayes (smoothen something)
PredictionNB <- predict(NB,test_df_withoutclass)
table(PredictionNB,test_classes)
# PredictionNB  1  2
#            1 50  3
#            2  1 46
test_classes
prop.table(table(test_classes==PredictionNB)) 
#FALSE  TRUE 
# 0.04  0.96 


#' SVM 
#' how it works: think of a graph with a bunch of dots representing data points
#' draw a line/plane which maximize distance between the surface and the two groups 


SVM <- svm(train_df_withoutclass,train_classes,kernel="polynomial", degree = 3)
	# gives a bunch of warning, because it's sensitive to outliner? 
summary(SVM)
PredictionSVM <- predict(SVM,test_df_withoutclass)
table(PredictionSVM,test_classes)
#					test_classes
# PredictionSVM  1  2
# 						1 51  0
# 						2  0 49

prop.table(table(test_classes==PredictionSVM)) # 100% true
summary(SVM)

#try:
#kernel="polynomial"
#degree =3,5

# kernel = "polynomial", degree = 3 
# 				test_classes
# PredictionSVM  1  2
# 						1 51 49
#							2  0  0

# kernel = "polynomial", degree = 5  
#					test_classes
#PredictionSVM  1  2
#							1 51 49
# 						2  0  0


