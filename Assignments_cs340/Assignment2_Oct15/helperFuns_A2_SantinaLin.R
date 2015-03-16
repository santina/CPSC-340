#helper functions for Assignment 2, Santina Lin

library(dplyr) 

#Summarize the table for confusion matrix to make the calculation clear 

summarizeMatrix  <- function(matrix){
  #row would be the actual 
  #column would be the predicted 
  
  d  <- rbind(matrix, c(sum(matrix[,1]), sum(matrix[,2])))
  d  <- cbind(d, c(sum(d[1,]), sum(d[2,]), sum(d[3,])))
              
  rownames(d) <- c("<=50K", ">50K" , "sum")
  colnames(d) <- c("<=50K Predicted", ">50K Predicted" , "total")

  d
  
}

# we will define positive as people make <=50K 
compute_Spec_Sen<- function(confusionMatrix){
  #sensitivity = TP / (TP + FN) = TP / total classified as positive 
  TP <- confusionMatrix[1,1]
  FN <- confusionMatrix[1,2]
  sensitivity <- TP / (TP + FN)
  
  #specificity = TN / (TN+FP)
  TN <- confusionMatrix[2,2]
  FP <- confusionMatrix[2,1]
  specificity <- TN/(TN+FP)
  
  ans <- data.frame()
  ans <- rbind(ans, c(specificity, sensitivity))
  colnames(ans) <- c("specificity", "sensitivity")
  ans
  
}

computeAllQualities <- function(treeList, testingData, nameList){
  qualityMatrix <- data.frame()
  
  for (i in 1:length(treeList)){
    prediction <- predict(treeList[[i]], testingData, type="class")
    matrix  <- table (testingData$class, prediction) %>% summarizeMatrix()
    qualityMatrix <- rbind(qualityMatrix, compute_Spec_Sen(matrix))
  }
  
  rownames(qualityMatrix) <- nameList
  
  qualityMatrix
  
}

