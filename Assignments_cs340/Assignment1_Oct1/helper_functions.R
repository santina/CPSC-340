
#find the minimum entropy, 
#cutgroup_list, vecotor of all cutree combined
  # numDocs: total number of documents being classified 
findMinEntropyK  <- function(cutgroupList, numDocs=300){
  #not sure if entropy will calculate correctly for k = 1
  k  <- length(cutgroupList)/numDocs
  #minEntropy <- Inf 
  klist <- list()

  for (i in 1:k){
    currentGroup  <- cutgroupList[(1+ (numDocs*(i-1))):(numDocs*i)] #those parantheses are essential
    ent  <- entropy(currentGroup, i)
    klist <- append(klist, ent)
  }
  klist
}



# Return a list of cutree results 
classificationResult  <- function(distFreqMatrix, k = 6){
  cutgroup_list  <- vector()   #to be fill
  for (i in 1:k){
    cluster <- hclust(distFreqMatrix, method="ward.D")
    #plot(cluster,cex=0.5,hang=-1,main="Hclust dendogram of news, K=6") 
    #rect.hclust(cluster,k=6)  
    cutgroups  <- cutree(cluster, k=i)
    cutgroup_list  <- append(cutgroup_list, cutgroups)
  }
  cutgroup_list
}



#take in the integer vector (cutgroups2, cutgroups3...) returned by cutree() and return the entropy
# numGroups goes from 2 to 6 ... in our assignment 
entropy  <- function(cutGroups, k){
  
  #initizalize all vectors with specific length (all entries is zero) to 
  #make sure number of columns of dataframe is a multiple of vector length
  
  #store result of number of things in each class, class id is the index
  numItemVector  <- vector(length=k, mode="numeric")
  #store numeric vector, with index being the class id, and number is #docs in each class 
  compHardware  <- vector(length=k, mode="numeric")
  sciMed  <- vector(length=k, mode="numeric") 
  compOS  <- vector(length=k, mode="numeric")
  
  #store result of number of things in each class, class id is the index 
  #summarize result and extract the vector without names
  numItemVector  <- table(cutGroups) %>% as.vector() 
  
  #cutgroup is prearranged, for example: 
  #sum(as.numeric(names(cutgroups3[201:300])) >= 58061)   
  #returns 100, I checked other ranges and for other cutgroupsk and they're all good
  #so we can make use the fact that they're presorted : 
  
  
  v1  <- cutGroups[1:100] %>% table() %>% as.vector() 
  compHardware[1:length(v1)]  <-  v1
  
  v2  <- cutGroups[101:200] %>% table() %>% as.vector() 
  compOS[1:length(v2)]  <- v2
  
  v3  <- cutGroups[201:300] %>% table() %>% as.vector() 
  sciMed[1:length(v3)]  <- v3
  
  classResultsDF  <- rbind(compHardware, compOS, sciMed) 
  
  entropy  <-  calculateEntropy(numItemVector, classResultsDF, k)
  
  
}


# Entropy helper methods 
# entropy defined as sum of e_i where 
#i is 1:k 
#and e_i = -log(P_x_i)*P_x_i ,  P_x_i is ratio fo x items in class i 
# summing over all i 

calculateEntropy <- function(numItemVector, classResultsDF, k) {
  e_i  <- 0
  Entropy <- 0
  
  # to make log(0) default to return 0
  mylog10  <- function(x){
    answer  <- 0
    if(x > 0){
      answer  <- log10(x)
    }
    answer 
  }
  for (i in 1:k){
    P_xi <- (classResultsDF["compHardware", i] %>% as.vector()) / numItemVector[i]
    P_yi <- (classResultsDF["sciMed", i] %>% as.vector())/ numItemVector[i] 
    P_zi <- (classResultsDF["compOS", i] %>% as.vector())/ numItemVector[i]
    
    e_i  <- e_i -(mylog10(P_xi)*P_xi + mylog10(P_yi)*P_yi + mylog10(P_zi)*P_zi )
    
    Entropy  <- Entropy + e_i*numItemVector[i]/sum(numItemVector)
    e_i  <- 0
  }
  Entropy 
}