set.seed(1)
library(tm)
library(topicmodels) #for fitting topic models 
library(modeltools) # 

all_data<-read.table("recipes.csv",sep = ",",header=TRUE)
all_data = all_data[sample(nrow(all_data)),] 
data = all_data[1:10000,]
data[1,]
#' TA's quick tutorial on LDA and topic modeling 
#' Topic modeling:
#' 	 when start with a collection of documents, it assume there's a true collection
#' 	 of topics (ex: topic1: cats {"meow, "litter" } and topic2: dogs{"fetch"})
#' 	 but some documents can have multiple topics. (can have a list of topics covered 
#' 	 by it) 
#' 	 assume a true prob distribution for all k topics, choice of k is important
#' 	 assume eah topic has a true prob distribution of words covered by that topic 
#' 	 kinda like k clustering but we're not sorting the document,
#' 	 but instead pulling topics out of the documents 
#' a topic in Latent Dirichlet Allocation (LDA), I think... 


corpus = Corpus(VectorSource(data$Ingredients))
dtm = DocumentTermMatrix(corpus)

dict = findFreqTerms(dtm,10) # filter out those that don't appear at least 10 times..
dtm.filtered = DocumentTermMatrix(corpus, list(dictionary = dict))

recipes.m = as.matrix(  dtm.filtered )

popularity = sort(colSums(recipes.m), decreasing=TRUE)
popularity = data.frame(ingredients = names(popularity), num_recipes=popularity)
popularity$ingredients = reorder(popularity$ingredients, popularity$num_recipes)

library(ggplot2)

ggplot(popularity[1:25,], aes(x=ingredients, y=num_recipes)) + 
  geom_point(size=5, colour="red") + coord_flip() + 
  ggtitle("Recipe Popularity of Top 25 Ingredients") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), 
  axis.text.y=element_text(size=13,colour="black",
  face="bold"), axis.title.x=element_text(size=14, face="bold"), 
  axis.title.y=element_text(size=14,face="bold"),
  plot.title=element_text(size=24,face="bold"))

# we see that eggs wheat and butter are very frequent, almost like stop words 
# so we'll remove them 
corpus = tm_map(corpus, removeWords, c("wheat","egg","butter"))
# you can also try removing top 5 or whatever, and see how the result differs 

dtm.final = DocumentTermMatrix(corpus, control = list(dictionary = dict))
rowTotals <- apply(dtm.final , 1, sum) 
dtm.new   <- dtm.final[rowTotals> 0, ] # remove things that have no ingredients 

lda = LDA(dtm.new, 20) # choose 20 topics (arbitrarily guess 20)
t = terms(lda,5)
t

lda_10 = LDA(dtm.new, 10) # choose 10 topics (arbitrarily guess 20)
t_10 = terms(lda,5)
t_10

