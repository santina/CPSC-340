library(e1071)
library(randomForest)
library(tree)

# load data 
training<- read.table("LabNov10-train.csv", sep = ",", header = TRUE)
testing<- read.table("LabNov10-test.csv", sep = ",", header = TRUE)

#extra code: 
tr <- tree(y~x1+x2+x3, data = training) # a single decision tree
prediction.tr <- predict(tr, newdata = testing)
	# to take a look: 
	plot(tr) 
	text(tr)
error.tr <- sqrt(sum((testing$y - prediction.tr)^2)/nrow(testing))
error.tr  # 153.916 Now we have a benchmark and now compare this to random forest 


rf_10 <- randomForest(y~x1+x2+x3,data=training,ntree=10)
predictions.rf_10 <-predict(rf_10,newdata=testing)
error.rf_10 <-sqrt((sum((testing$y-predictions.rf_10)^2))/nrow(testing))
error.rf_10  #  146.7015

rf_100 <- randomForest(y~x1+x2+x3,data=training,ntree=100)
predictions.rf_100 <-predict(rf_100,newdata=testing)
error.rf_100<-sqrt((sum((testing$y-predictions.rf_100)^2))/nrow(testing))
error.rf_100  # 143.7546

# now compare linear model and support vector machine 

rm <- glm(y ~ x1 + x2 + x3, data = training)
predictions.rm<-predict(rm,newdata=testing)
error.rm<-sqrt((sum((testing$y-predictions.rm)^2))/nrow(testing))
error.rm #173.3578 

svm <- svm(y~x1+x2+x3,data=training)
predictions.svm<-predict(svm,newdata=testing)
error.svm<-sqrt((sum((testing$y-predictions.svm)^2))/nrow(testing))
error.svm #143.7986

# ensemble.  combine svm and linear regression together by averaging the predictions
predictions.svm_rm<-(predictions.svm + predictions.rm)/2  #we're averaging the prediction
error.svm_rm<-sqrt((sum((testing$y-predictions.svm_rm)^2))/nrow(testing))
error.svm_rm  #148.1048  lower than the average of errors (143.7986+173.3578)/2 = 158.6

# now we weight our prediction method, weight svm 0.9 and linear regression 0.1
predictions.svm_rm<-(predictions.svm * 9 + predictions.rm)/10
error.svm_rm<-sqrt((sum((testing$y-predictions.svm_rm)^2))/nrow(testing))
error.svm_rm  # 142.7605 , a bit lower 50/50 weight 

# combine svm and random forest together 
predictions.svm_rf<-(predictions.svm + predictions.rf_10)/2
error.svm_rf<-sqrt((sum((testing$y-predictions.svm_rf)^2))/nrow(testing))
error.svm_rf  #  140.0745  

#combine random forest and linear regression

predictions.rf_glm<-(predictions.rf_10 + predictions.rm)/2
error.rf_glm<-sqrt((sum((testing$y-predictions.rf_glm)^2))/nrow(testing))
error.rf_glm  #  151.3052

