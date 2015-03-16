#install.packages('lm')
#library(lm)

#this file is heavily commented and modified with added new lines of code 
#after being downloaded from the course website 

#'build a  model for something (house price) as a function of the factors "area
#'and number of bedroom
#'differnce is that we're not catagorizigng prices 
#'we're building a numerical model = regression
#'then we can use logistic regression to classiy catagorial data, like last week 

#=============Part 1: age and heigth=============================

train<- read.table("age-height-train.csv", sep = ",", header = TRUE)
test <- read.table("age-height-test.csv", sep = ",", header = TRUE)
#train
plot(train$x, train$y,ylab='Height in meters',xlab='Age in years')

lr_model  <- lm(y ~x, data = train) #0.73 0.0663 
# H = 0.0663A + 0.73 

lr_model = lm(y~x,data=train) # equal sign? 
lr_model
summary(lr_model)
abline(lr_model,col="green") #draw a green line for linear regression 

#interval = "prediction" has to do with confidence intervals (lower and upper)
  #can go look up ?predict for more details 
prediction <- predict(lr_model,interval="prediction",newdata=test)
prediction  #what's lower and upper? 
prediction <- as.data.frame(prediction)

#plot test data points 
points(test$x,test$y,col="red")

#what does this line mean? : 
points(test$x,prediction$fit,col="blue",pch=17)

#bind the y of testing data with the "fit" from prediction
#if prediction is good, they shouldn't differ much 
cbind(test$y , prediction$fit)



########### PART 2 ##############
#look at residuals to compare unscale and scaled 
#which one is better, why? 

train <- read.table("housing-train.csv",sep = ",",header=TRUE)
test <- read.table("housing-test.csv",sep = ",",header=TRUE)

#optional plots 
plot(train$price, train$area,xlab='Price',ylab='Area')
plot(train$price, train$bedrooms,xlab='Price',ylab='Number of bedrooms')
#another nice optional plot 
pairs(~price +area+bedrooms, data=train)

model <- lm(price ~ area + bedrooms, data=train)
model  #P = 136.9A - 14085B + 108413 
summary(model)
plot(model) #don't need to undestand all the plots 

#The first plot gives an idea of whether there is any curvature in the data. 
# If the red line is strongly curved, a quadratic or other model may be better. 
# |Yp- Ye| second plot 
# Third: 
# Fourth: if you move one point would fit change drasticaly 
#  (less important at this stage) The second plot is to check whether the residuals (y-y^) are normally distributed. 
# The third plot is used to check if the variance is contant 
# (ie, if the standard deviation among the residuals appears to be about constant). 
# If the red line is strongly tilted up/down, that is a red flag.
# (less important at this stage) The last plot is used to check to see if there were any 
# overly influential points 


hist(resid(model)) #residue on training data 

prediction2 <- predict(model,interval="prediction",newdata=test)
prediction2 <- as.data.frame(prediction2)
# view the real and predicted prices
cbind(test$price , prediction2$fit)
cbind(test$price , prediction2$fit, abs(test$price - prediction2$fit))

#calculate residual sum 
sum(abs(test$price-prediction2$fit))/length(test$price)

#===============PART 2.2======================
# optional: use scale() function
feature.scale = function (dataset, columns) {
  for (column in columns) {
    sigma = sd(dataset[,column])
    mu = mean(dataset[,column])
    dataset[paste(names(dataset[column]), ".scale", sep = "")] = (dataset[,column] - mu)/sigma
  }
  return(dataset)
}

train.scaled <- feature.scale(train, c("area", "bedrooms"))
test.scaled <- feature.scale(test, c("area", "bedrooms"))
#model.scale <- lm(price ~ scale(area) + scale(bedrooms), data=train)
model.scale <- lm(price ~ area.scale + bedrooms.scale, data=train.scaled)
model.scale  # P = 114289A - 11079B + 342997 
plot(model.scale)
hist(resid(model.scale))
prediction.scale <- predict(model.scale,interval="prediction",newdata=test.scaled)
prediction.scale <- as.data.frame(prediction.scale)
# view the real and predicted prices
cbind(test$price , prediction.scale$fit)

# for scaled data: The residuals average:
sum(abs(test$price-prediction.scale$fit))/length(test$price)
# error = 1/n * summation(i=1, n){y_p - y_e}   y_p: prediction price, y_e acutal price for test

#or this is the right way to do it? specify "scaled" data 
sum(abs(test.scaled$price-prediction.scale$fit))/length(test.scaled$price)
#acutally both gives the same sum of residuals 39722.82 

cbind(test$price , prediction.scale$fit)

# try find residues in test (train is already looked at )