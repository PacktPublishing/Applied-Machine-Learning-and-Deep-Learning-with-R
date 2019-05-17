#logistic regression application

#get data from url= https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data

url<- "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
dd<- read.table(url,sep = ",")

#assigning colnames to matrix
colnames(dd)<- c('pregnant_not', 'glucose_tolerance', 'diastolic_blood_pressure', 'triceps_skin_fold_thickness',
                 'serum_insulin','bmi','diabetes_pedigree','age','test_diabet')

#we are getting summary labels
table(dd$test_diabet)

#setting row numbers in dd variable
r <- dim(dd)[1]

#setting col numbers in dd variable
c <- dim(dd)[2]

#calculating mean variables col by col
cm <- apply(dd[,1:8], 2, mean)

#calculating standart deviations col by col
csd <- apply(dd[,1:8], 2, sd)

#setting NULL matrix
std_dd <- matrix(nrow = r, ncol = c-1)

#calculating normalized variables col by col except last coloumn.
for (i in 1:(c-1))
{
  for(j in 1:r)
  {
    std <- (dd[j,i]-cm[i])/csd[i]
    std_dd[j,i] <- std
  }
}

#concating normalized variables and label
dd_std <- data.frame(std_dd,dd$test_diabet)

#getting summary of the normalized data
summary(std_dd)

#divide dataset into two subgroups train and test 
train_size <- round(dim(dd_std)[1]*0.7)
test_size <- dim(dd_std)[1]-train_size

train <- dd_std[1:train_size,]
test <- dd_std[(train_size+1):dim(dd_std)[1],]

#installing class package
install.packages("class")
library(class)

#train logistic regression
model <- glm(dd.test_diabet~.,family=binomial(link='logit'),data=train)

#getting predictions
predictions <- predict(model,newdata=test[,1:8],type='response')

#converting predicted labels  
fixed_predictions <- ifelse(predictions > 0.5,1,0)





#burasi classification ornekleri kismi icin...
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

0.8647186



