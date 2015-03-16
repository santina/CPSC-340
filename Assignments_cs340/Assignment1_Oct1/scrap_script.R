#K = 2 
hClusterResults2 <- hclust(distFreqMatrix, method="ward.D")
#optional plot to see the result of clustering 
plot(hClusterResults2,cex=0.5,hang=-1,main="Hclust dendogram of news, K=2") 
#draw the boxes on the clusters/classes on the graph 
rect.hclust(hClusterResults2,k=2)  
#cut a tree (result form hclust) into specificied number of groups
cutgroups2  <- cutree(hClusterResults2, k=2)
#make result into a dataframe so that cutgroupk["id", 1] returns cluster# 
#cutgroups2  <- data.frame(cutgroups2)

#K = 3
hClusterResults3 <- hclust(distFreqMatrix, method="ward.D")
plot(hClusterResults3,cex=0.5,hang=-1,main="Hclust dendogram of news, K=3") 
rect.hclust(hClusterResults3,k=3)  
cutgroups3 <- cutree(hClusterResults3, k=3)
#cutgroups3  <- data.frame(cutgroups3)

#K = 4
hClusterResults4 <- hclust(distFreqMatrix, method="ward.D")
plot(hClusterResults4,cex=0.5,hang=-1,main="Hclust dendogram of news, K=4") 
rect.hclust(hClusterResults4,k=4)  
cutgroups4 <- cutree(hClusterResults4, k=4)
#cutgroups4  <- data.frame(cutgroups4)

#K = 5
hClusterResults5 <- hclust(distFreqMatrix, method="ward.D")
plot(hClusterResults5,cex=0.5,hang=-1,main="Hclust dendogram of news, K=5") 
rect.hclust(hClusterResults5,k=5)  
cutgroups5 <- cutree(hClusterResults5, k=5)
#cutgroups5  <- data.frame(cutgroups5)

#K = 6
hClusterResults6 <- hclust(distFreqMatrix, method="ward.D")
plot(hClusterResults6,cex=0.5,hang=-1,main="Hclust dendogram of news, K=6") 
rect.hclust(hClusterResults6,k=6)  
cutgroups6 <- cutree(hClusterResults6, k=6)
#cutgroups6  <- data.frame(cutgroups6)