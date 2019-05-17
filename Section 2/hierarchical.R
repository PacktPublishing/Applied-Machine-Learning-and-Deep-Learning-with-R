#hierarchical clustering applications.

#lets get the data from the github repo https://raw.githubusercontent.com/olgnaydn/R/master/nufus.csv

url <- 'https://raw.githubusercontent.com/olgnaydn/R/master/nufus.csv'
d <- read.csv(url,header = TRUE, sep = ',', encoding="UTF-8", stringsAsFactors=FALSE)

#reach the manual of hcust
help("hclust")

# calculate distances

ddist<- dist(d$population, method="euclidean")

#applying hiearchical clustering
hc<- hclust(ddist)

#draw dendogram
plot(hc, hang=-1, label=d$city)

#draw more understandable dendogram
rect.hclust(hc, k=5, border="red")

