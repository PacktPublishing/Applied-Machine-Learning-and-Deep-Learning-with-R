#decision tree application

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
install.packages("party")
library(party)

#train a tree
model <- ctree(dd.test_diabet~ .,data = train)

#draw a plot for tree
plot(model)





