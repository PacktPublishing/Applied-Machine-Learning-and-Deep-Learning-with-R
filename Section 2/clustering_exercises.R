#Clustering applications.

#lets get the data from the github repo https://raw.githubusercontent.com/olgnaydn/R/master/dataset_tufe_vs_index.csv

url <- 'https://raw.githubusercontent.com/olgnaydn/R/master/dataset_tufe_vs_index.csv'
d <- read.csv(url,header = TRUE, sep = ',')
dc <- d[2:4]

#apply k-means
c <- kmeans(dc,5)

#merging cluster labels and dataset
dd<- data.frame(dc,c$cluster)

#apply hierarchical clustering 
ddist<- dist(dc, method = "euclidean")
hc <- hclust(ddist)
plot(hc, hang=-1, label=d$Tarih)

#apply DBSCAN 
eps <- kNNdistplot(as.matrix(dc), k =  5)

#apply dbscan
cdb<- dbscan(as.matrix(dc),eps=10, minPts=10)
cdb$cluster
dcdb<- data.frame(d,cdb$cluster)

