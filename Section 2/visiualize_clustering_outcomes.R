#Visiulaizing clustering algorithms.

#lets get the data from the github repo https://raw.githubusercontent.com/olgnaydn/R/master/dataset_tufe_vs_index.csv

url <- 'https://raw.githubusercontent.com/olgnaydn/R/master/dataset_tufe_vs_index.csv'
d <- read.csv(url,header = TRUE, sep = ',')

izmir_tufe<- data.frame(d$tcmb_izmir_2el,d$tufe)
istanbul_tufe <- data.frame(d$tcmb_istanbul_2el,d$tufe)

#apply k-means for izmir
cizm <- kmeans(izmir_tufe,5)

#visiualize k-means clustering 
plot(izmir_tufe$d.tcmb_izmir_2el, izmir_tufe$d.tufe, col=cizm$cluster)
points(cizm$centers[,c(1,2)], col=1:3, pch=19, cex=2)

#apply hierarchical clustering for izmir
dizm <- dist(izmir_tufe, method = "euclidean")
hcizm <- hclust(dizm)

#visiualize dendogram
plot(hcizm, hang=-1, label=d$Tarih)
rect.hclust(hcizm, k=5, border="red")
