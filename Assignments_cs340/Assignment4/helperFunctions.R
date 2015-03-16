
library(stringr) # for regular expression 
library(e1071)   # for svm 

processCorpusList <- function(corpusList){
	processedCorpuses <- llply(corpusList, function(myCorpus){
		# remove XML: already taken care of in the readerControl when read in the file
		# remove document headings, use regular expression 
		# From... Subject:.... Organization:... Lines... 
		
		# remove "From: " and following email address
		removeFrom<- function(x) gsub("From:\\s\\S+@\\S+", "", x)
		myCorpus <- tm_map(myCorpus, content_transformer(removeFrom))
		# remove all other email addresses 
		removeEmail<- function(x) gsub("\\S+@\\S+", "", x)
		myCorpus <- tm_map(myCorpus, content_transformer(removeEmail))
		# remove "Subject: " 
		removeSubject <- function(x) gsub("Subject:\\s|Re:\\s", "", x)
		myCorpus <- tm_map(myCorpus, content_transformer(removeSubject))
		# remove "Organization: " 
		removeOrg <- function(x) gsub("Organization:\\s", "", x)
		myCorpus <- tm_map(myCorpus, content_transformer(removeOrg))
		# remove Lines (and the following numbers)
		removeLines <- function(x) gsub("Lines:\\s\\d+", "", x)
		myCorpus <- tm_map(myCorpus, content_transformer(removeLines))
		
		# other processing: got those from tutorials 0 and 2 codes with my comments 
		# convert to lower case
		myCorpus <- tm_map(myCorpus, content_transformer(tolower))
		# remove punctuation
		myCorpus <- tm_map(myCorpus, removePunctuation)
		# remove numbers
		myCorpus <- tm_map(myCorpus, removeNumbers)
		# remove URLs (EXTRA)
		removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
		myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
		# remove stopwords (stop words are the list of words that 
		# are freqeunt and have less value in terms on content such as: a, the, is, ...)
		myStopwords <- c(stopwords('english'))
		myCorpus <- tm_map(myCorpus, content_transformer(removeWords), myStopwords)
		
		# remove whtie space 
		myCorpus <- tm_map(myCorpus, stripWhitespace)  
		
		#myCorpus <- tm_map(myCorpus, PlainTextDocument)
		
	})
  processedCorpuses
  
}

getClasses  <- function(files){
	classZeroOne <- numeric()
	for (f in files){
		if(grepl("comp", f)){
			classZeroOne <- c(classZeroOne, rep(1, times = length(list.files(f))))
		}
		else{
			classZeroOne <- c(classZeroOne, rep(0, times = length(list.files(f))))
		}
	}
	classZeroOne
}

getConfusionMatrix_SVM <- function(test, train1, train2, train3, train4){
	trainSet <- rbind(train1, train2, train3, train4)
	trainClass <- trainSet$class
	testClass <- test$class
	
	trainSet$class <- NULL
	test$class <- NULL
	
	SVM <- svm(trainSet,trainClass,kernel="linear") 
	PredictionSVM <- predict(SVM,test)
	t <- table(PredictionSVM,testClass)
	t
}