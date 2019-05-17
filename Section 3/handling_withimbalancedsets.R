# Dealing with imbalanced data

#get data from url= https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data

url<- "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
dd<- read.table(url,sep = ",")

#assigning colnames to matrix
colnames(dd)<- c('pregnant_not', 'glucose_tolerance', 'diastolic_blood_pressure', 'triceps_skin_fold_thickness',
                 'serum_insulin','bmi','diabetes_pedigree','age','test_diabet')

#getting summary for labels
table(dd$test_diabet)
prop.table(table(dd$test_diabet))

#installing the oversampling package
install.packages("ROSE")
library(ROSE)

over_sampled_dd <- ovun.sample(test_diabet ~ ., data = dd, method = "over",N = 1000)$data
table(over_sampled_dd$test_diabet)

#setting row numbers in dd variable
r <- dim(over_sampled_dd)[1]

#setting col numbers in dd variable
c <- dim(over_sampled_dd)[2]

#calculating mean variables col by col
cm <- apply(over_sampled_dd[,1:8], 2, mean)

#calculating standart deviations col by col
csd <- apply(over_sampled_dd[,1:8], 2, sd)

#setting NULL matrix
std_dd <- matrix(nrow = r, ncol = c-1)

#calculating normalized variables col by col except last coloumn.
for (i in 1:(c-1))
{
  for(j in 1:r)
  {
    std <- (over_sampled_dd[j,i]-cm[i])/csd[i]
    std_dd[j,i] <- std
  }
}

#concating normalized variables and label
dd_std <- data.frame(std_dd,over_sampled_dd$test_diabet)

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

#train knn model
knn_predict<- knn(train = train, test = test, cl = train$over_sampled_dd.test_diabet,k =5)
knn_predict
test$over_sampled_dd.test_diabet

#generate confusion matrix
prop.table(table(knn_predict, test$over_sampled_dd.test_diabet))
