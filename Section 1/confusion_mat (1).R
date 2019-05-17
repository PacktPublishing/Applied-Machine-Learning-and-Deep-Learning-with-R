
library(caret)
data(iris)
# rename the dataset
dataset <- iris

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]

predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
