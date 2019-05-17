#naive bayes application

install.packages("mlbench")
library(mlbench)

#getting data
data(HouseVotes84, package = "mlbench")

#building model 
model <- naiveBayes(Class ~ ., data = HouseVotes84)

#make predictions
prediction <- predict(model, HouseVotes84)

#get confusion matrix
table(prediction, HouseVotes84$Class)
