
#k-means applications.

#lets get the data from the github repo https://raw.githubusercontent.com/olgnaydn/R/master/nufus.csv

url <- 'https://raw.githubusercontent.com/olgnaydn/R/master/nufus.csv'
d <- read.csv(url,header = TRUE, sep = ',', encoding="UTF-8", stringsAsFactors=FALSE)

#get help page of kmeans function
help(kmeans)

# apply k-means
c<- kmeans(d$population,5)

c
c$cluster

#merge cluster ids and the dataset
cd<- data.frame(d, c$cluster)

#get the mean variables of each clusters
c$centers

#get the iteration number
c$iter

#get the cities which are in the cluster with id=5
cd5 <- cd[which(cd$c.cluster==5),]

#get the cities which are in the cluster with id=4
cd4 <- cd[which(cd$c.cluster==4),]

#visiulazition of clustering
plot(d$population, col=c$cluster)
points(c$centers, pch=19, cex=2)


