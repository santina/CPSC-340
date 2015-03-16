# Santina Lin (87325149) 

#===================================================================
#1 load the dataset into R 

#loading some useful libraries  
#install.packages("tm")
#install.packages("dplyr")
#install.packages("proxy")

library(tm) # for doing data processing 
library(dplyr) #for easier code inspection for the TAs :)
library(proxy) # for cosine similarity 

source('./helper_functions.R')


newsData <- VCorpus(DirSource(c("comp.os","comp.hardware", "sci.med") , encoding="UTF-8"),readerControl = list(language = "eng"))
#ID range: 
  # sci.med: 58061-59633 (100 documents), third 100 
  # comp.os: 9141 -10942 (100 documents), second 100 
  # comp.hardware: 50419-52404 (100 doc), first 100 
#(first, third, second, accroding to the order of how the data are organized in ) 
# names(newsData[1])
#====================================================================
#2 clean + processes + convert dataset to feature vectors (term freq)

#CLEANING DATA: remove punctuations 
  newsData <- tm_map(newsData, content_transformer(removePunctuation))
#remove numbers
  newsData <- tm_map(newsData, content_transformer(removeNumbers))
#to lower cases
  newsData <- tm_map(newsData, content_transformer(tolower))
#remove URL 
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  newsData <- tm_map(newsData, content_transformer(removeURL))
#remove stopwords, like redundant words such as "a", "the" 
  stopwords <- c(stopwords('english'))
  newsData <- tm_map(newsData, removeWords, stopwords)


#TERM FREQUENCIES 
termFreqVector <- TermDocumentMatrix(newsData, control=list(wordLengths=c(1,Inf)))


#TAKE OUT SPARSE TERMS (to avoid long runnning time for dist)
#findFreqTerms(termFreqVector, lowfreq=10) #test code 
termFreqVector2 <- removeSparseTerms(termFreqVector, sparse=0.95)

#===================================================================
#3 Perform hierarchal clustering (k=2 to k=6)

#convert to matrix
  freqMatrix <- as.matrix(termFreqVector2) 
#transpose the term-document matrix to a document-term one. 
#very important otherwise you end up classifying words 
  freqMatrix2 <- t(freqMatrix)
#calculate distance matrix, use stats:: to distinguish it from that of proxy library
  distFreqMatrix <- stats::dist(scale(freqMatrix2)) #calculate distance matrix

#set standard seed for this assignment, 340 
set.seed(340)

#running from K=1 to K=6, storing result at each K to allow easy inspection 
#vector with all classification results merged in order 1-300, 301-600, etc
cutgroupList  <- classificationResult(distFreqMatrix, 6)

#find the best K
entropy_list <- findMinEntropyK (cutgroupList, 300)
bestK  <- which.min(entropy_list)
bestK #return 6 
minEntropy <- entropy_list[bestK]
minEntropy #return 0.4667768

#===================================================================
# 4. Extend the feature to IDF using the function (weightTfIdf{tm})
# Wiki: it diminishes the weight of terms that occur very frequently in the document set and 
# increases the weight of terms that occur rarely. 
# on the assumption that rare terms give more information about the identity of the doc

#will use the tm with sparse terms removed: termFreqVector2 

termFreqVector_IDF <- weightTfIdf(termFreqVector2)

#perform entropy analysis all over again with this IDF 

#convert to matrix
freqMatrix_IDF <- as.matrix(termFreqVector_IDF) 
freqMatrix_IDF2 <- t(freqMatrix_IDF)
distFreqMatrix_IDF <- dist(scale(freqMatrix_IDF2)) #calculate distance matrix

#vector with all classification results merged in order 1-300, 301-600, etc
cutgroupList_IDF  <- classificationResult(distFreqMatrix_IDF, 6)

#===================================================================
#5. from #4 in IDF analysis, view the best K and lowest entropy 

entropy_list_IDF <- findMinEntropyK (cutgroupList_IDF, 300)
bestK_IDF  <- which.min(entropy_list_IDF)
bestK_IDF # return 5
minEntropy_IDF <- entropy_list_IDF[bestK_IDF]
minEntropy_IDF  # return 0.155134
entropy_list_IDF[3] #0.401543
entropy_list_IDF[1] #0.4771213

# looks like the best k is 5, with much smaller minimum entropy (0.155134) than 
# without using IDF (k = 6 and entropy was 0.4667768 ) in #3 

#===================================================================
#6 Try cosine similarity distance matrix and compare results 
# Cosine similarit is a way to measure similairty between distance by 
# taking the angle betwene two vectors, in which value of each dimension 
# corresponds to the number of times that term appears in the document
# dimension is defined by the terms in the documents 

#calculate distance matrix using proxy library distance method cosine
cosineDistM <- proxy::dist(scale(freqMatrix2), method="cosine") 

#running from K=1 to K=6, storing result at each K to allow easy inspection 
#vector with all classification results merged in order 1-300, 301-600, etc
cutgroupList_cos  <- classificationResult(cosineDistM, 6)

#find the best K
entropy_list_cos <- findMinEntropyK (cutgroupList_cos, 300)
bestK_cos  <- which.min(entropy_list_cos)
bestK_cos #returns 6
#view the lowest entropy
minEntropy_cos <- entropy_list_cos[bestK_cos]
minEntropy_cos  #returns 0.3738984
entropy_list_cos[3] #returns 0.4650263  




