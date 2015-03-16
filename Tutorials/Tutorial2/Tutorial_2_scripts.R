#tutorial 2 , Sep 29 Monday 
#we're classifyig whether documents are talking about auto (2) or camera(1)

#install.packages("SnowballC")
#install.packages("rpart")
library(SnowballC)
library(tm)
library(rpart) #need this for classification 

train<- read.table("epinions-train.csv", sep = ",", header = TRUE)
test<- read.table("epinions-test.csv", sep = ",", header = TRUE)

#building the training and test set 
set.seed(123) #ensure reproducibility 
data_set  <- train[sample(nrow(train), 150), ] #randomy select 150 items from the training dataset 
test_set  <- data_set[100:150,] #same as rain_test_set  <-  data_set, 50 items for testing
train_set  <- data_set[1:100,] # get 100 items to train the model 

#train_set <- train[sample(nrow(train), 100),]
#test_set <- test[sample(nrow(test), 50),]

#now we combine the two datasets together 
train_test_set <- rbind(train_set,test_set) 
#train_test_set$class   #basically you have two components in this dataframe 
#train_test_set$text

#goal to predict via decision tree classifer whether given doc is about auto or camera 
#turn the data frame's text component into a corpus 
train_test_corpus <- Corpus(VectorSource(train_test_set$text))

#preprocess the corpus (content transformer is used to remove some error message whch idk why happened)
train_test_corpus <- tm_map(train_test_corpus, content_transformer(tolower))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removePunctuation))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removeWords) , c(stopwords('english')))
#train_test_corpus[[1]] ##checking to see if words are removed 
train_test_corpus <- tm_map(train_test_corpus, stripWhitespace) #spaces are removed too 
#train_test_corpus[[1]]

# steamming helps things like "say" and "says" are the same (only stem "say" is kept)
# DTM stands for? 
DTM_train_test <- DocumentTermMatrix(train_test_corpus,control=list(stemming = TRUE)) 
DTM_train_test  #sparsity: 96%, non-spase/spase entries: 24K versus 66K  

#for comparison purpose, below code would return 97% sparsity, 26K/938K 
#b  <-  DocumentTermMatrix(train_test_corpus) 

train_test_dataframe <- as.data.frame(inspect( DTM_train_test ))

#we basically transformed the original matrix to one with column of classes 
classification <- c(train_test_set$class)
train_test_df_withclass <- cbind( train_test_dataframe, classification)

#now we separate them again 
train_df_withclass <- head (train_test_df_withclass,100)
test_df_withclass <- tail (train_test_df_withclass,50)

# . means include all the terms in making the model
dt_model <- rpart(classification ~ .,train_df_withclass,method="class")
prediction <- predict(dt_model, test_df_withclass, type="class")
dt_table  <- table (test_df_withclass$classification,prediction)#this is the confusion matrix 
#row = actual classification, col is predicted 
#confusion matrix shows a table rbind(c(27,0), c(0, 23))

#building model based on only two terms : camera and car 
dt_model2 <- rpart(classification ~ camera+car,train_df_withclass,method="class")
prediction2 <- predict(dt_model2, test_df_withclass, type="class")
dt_table2 <- table (test_df_withclass$classification,prediction2) 
#same as dt_table

#based on term "also", probably not a good model 
dt_mode3 <- rpart(classification ~ also,train_df_withclass,method="class")
prediction3 <- predict(dt_model3, test_df_withclass,type="class")
db_table3 <- table (test_df_withclass$classification,prediction3)
# shows a table rbind(c(19,8), c(17, 6))


#extra testing for writeup 

#ANOVA 
dt_model_a1 <- rpart(classification ~ also,train_df_withclass,method="anova")
prediction_a1 <- predict(dt_model_a1, test_df_withclass,type="vector")
 table (test_df_withclass$classification,prediction_a1)
#8 of class 1 got classified to another, 6 of class 2 got into another group , 1.44, 1.6 

dt_model_a2 <- rpart(classification ~ camera+car,train_df_withclass,method="anova")
prediction_a2 <- predict(dt_model_a2, test_df_withclass,type="vector")
db_table_a2 <- table (test_df_withclass$classification,prediction_a2)
#27, 23 

dt_model_a3 <- rpart(classification ~ .,train_df_withclass,method="anova")
prediction_a3 <- predict(dt_model_a3, test_df_withclass,type="vector")
db_table_a3 <- table (test_df_withclass$classification,prediction_a3)
#27, 23 

#Poisson 

dt_model_p1 <- rpart(classification ~ also,train_df_withclass,method="poisson")
prediction_p1 <- predict(dt_model_p1, test_df_withclass,type="vector")
table (test_df_withclass$classification,prediction_p1)
#8 of class 1 got classified to another, 6 of class 2 got into another group 
#1.44, 1.6 

dt_model_p2 <- rpart(classification ~ camera+car,train_df_withclass,method="poisson")
prediction_p2 <- predict(dt_model_p2, test_df_withclass,type="vector")
table (test_df_withclass$classification,prediction_p2)
#27, 23 

dt_model_p3 <- rpart(classification ~ .,train_df_withclass,method="poisson")
prediction_p3 <- predict(dt_model_p3, test_df_withclass,type="vector")
table (test_df_withclass$classification,prediction_p3)



dt_model <- rpart(classification ~ .,train_df_withclass,method="class") 
prediction <- predict(dt_model, test_df_withclass, type="class") 
table (test_df_withclass$classification,prediction) 

#to plot the model 
par(mfrow = c(1,2), xpd = NA) 
plot(dt_model)
