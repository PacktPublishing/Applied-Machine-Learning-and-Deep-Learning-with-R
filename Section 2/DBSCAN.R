#DBSCAN clustering applications.

#lets get the data from the github repo https://raw.githubusercontent.com/olgnaydn/R/master/nufus.csv

url <- 'https://raw.githubusercontent.com/olgnaydn/R/master/nufus.csv'
d <- read.csv(url,header = TRUE, sep = ',', encoding="UTF-8", stringsAsFactors=FALSE)

#install dbscan package
install.packages('dbscan')

#call the library
library(dbscan)

#predict eps variable
eps <- kNNdistplot(as.matrix(d$population), k =  5)

#apply dbscan
c <- dbscan(as.matrix(d$population),eps=50000, minPts=3)
c$cluster
c
dc<- data.frame(d,c$cluster)

