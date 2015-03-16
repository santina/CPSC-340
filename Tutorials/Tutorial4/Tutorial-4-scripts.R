# the following code are from connect, and has been heavily commented by me. 

train<- read.table("admission-train.csv", sep = ",", header = TRUE)
test <- read.table("admission-test.csv", sep = ",", header = TRUE)
head(train)
summary(train)
#contingency table : makes sense more for categorial data
xtabs(~admit + rank, data = train) # categorical 
xtabs(~admit + gre, data = train)  # numerical (gre), doesn't make as much sense

#convert numerical to categorical
train$rank <- factor(train$rank)
head(train$rank)
test$rank <- factor(test$rank)
train$admit <- factor(train$admit)
test$admit <- factor(test$admit)

# training the first logistic regression model (glm for logistic regression)
model1 <- glm(admit ~ gre + gpa, data = train, family="binomial")
summary(model1) 
# P(gre, gpa) = (1 - e^-(-3.37+0.00266*gre + 0.278*gpa))^-1
# P(gre) = (1+e^(-b_0 + B_1*gre))^-1  a sigmoid function: gre better, closer to 1
# number of stars indicates how significantly influential they're


predict1 <- predict(model1, newdata = test, type = "response")
#look at the precict1 values and guess what are the values? 
predict1  # there are 100 cases 
# in order to change the probabilities to binary decision, we need to find a
# cutoff. We want a cutoff that gives us the best both sensitivity/specifity.
# wanna see how sensitivity and specificity change when we change this cutoff 
# gonna try <0.5 didn't get in, and >=0.5 gets in

perf = function(cut, mod, y)
   {
        yhat = (mod$fit>cut)
        w = which(y==1)
        sensitivity = mean( yhat[w] == 1 ) 
        specificity = mean( yhat[-w] == 0 ) 
        out = t(as.matrix(c(sensitivity, specificity)))
        colnames(out) = c("sensitivity", "specificity")
        return(out)
     }

perf(0.1,model1,train$admit) # completely sensitive but not specific 
perf(0.5,model1,train$admit)  
perf(0.8,model1,train$admit)

sensetivity=c()
specifity=c()

for(i in seq(0.1, 0.9, by=0.1)){
  sensetivity <- c(sensetivity,perf(i,model1,train$admit)[1])
  specifity <- c(specifity,perf(i,model1,train$admit)[2])
  }

#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensetivity , type="l", lty=1, col="blue", 
     xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)
# best cutoff at intercept 

#prediction
predict1 <- ifelse(predict(model1, newdata=test,type="response")>0.3, 1, 0)
# if prob greater than 0.3, we san it's 1 otherwise 0

confusionmatrix1 <- table(predict1, test$admit)
confusionmatrix1
#accuracy of model over the test data:
sum(diag(confusionmatrix1))/sum(confusionmatrix1)



##### model 2:
model2 <- glm(admit ~ gre + gpa +  rank, data = train, family="binomial")
summary(model2)
sensetivity=c()
specifity=c()
for(i in seq(0.1, 0.9, by=0.1)){
  sensetivity <- c(sensetivity,perf(i,model2,train$admit)[1])
  specifity <- c(specifity,perf(i,model2,train$admit)[2])
}



#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensetivity , type="l", lty=1, col="blue", xlab="cutoff points", 
     ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)

#prediction
predict2 <- ifelse(predict(model2, newdata=test,type="response")>0.3, 1, 0)
confusionmatrix2 <- table(predict2, test$admit)
confusionmatrix2

# columns 

#accuracy of model over the test data:
sum(diag(confusionmatrix2))/sum(confusionmatrix2)

# 0.66 ... better than model 1 by 2% 



