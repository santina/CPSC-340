
# helper functions for Assignment 3 

  # based on tutorial 4 code 
findOptimalCutOff = function(model, data){
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
  
  # Now we try different cutoff 
  sensitivity=c()
  specifity=c()
  for(i in seq(0.1, 0.9, by=0.1)){
    sensitivity <- c(sensitivity,perf(i,model,data$default10yr)[1])
    specifity <- c(specifity,perf(i,model,data$default10yr)[2])
  }
  
  # find the cutoff: the intersection of lines for sensitiivty and specificity 
  plot(seq(0.1, 0.9, by=0.1), sensitivity , type="l", lty=1, col="blue", 
       xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
  lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)
  
}

getConfusionMatrix = function(model, data, cutoff){
  # test the model on the testing data with 0.2 optimal cutoff 
  prediction_test <- ifelse(predict(model, newdata=data,type="response")>cutoff, 1, 0)
  confusionmatrix_test <- table(prediction_test, data$default10yr)
  
  confusionmatrix_test 
}

getAccuracy = function(matrix){
  sum(diag(matrix))/sum(matrix) 
}

getSensitivity = function(matrix){
  # TP/(TP+FN)
  ans <- matrix[1,1]/(matrix[1,1]+matrix[1,2])
  ans
}

getSpecificity = function(matrix){
  # TN/(TN+FP)
  ans <- matrix[2,2]/(matrix[2,2]+matrix[2,1])
  ans
}
getTotalError = function(client_ids, predicted_data_list, true_data){
  
  sumErrors <- aaply(client_ids, .margin = 1, function(id){
    trueVal <- true_data[true_data$clientid == id, ]$loan
    predictedVal  <- as.vector(predicted_data_list[as.character(id)])
    error_square <- (trueVal - predictedVal)^2
  })
  
  sum(as.vector(sumErrors))
}

