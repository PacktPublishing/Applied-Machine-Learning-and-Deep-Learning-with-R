#Deep neueral networks application

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

#getting summary of the normalized data
summary(std_dd)

#divide dataset into two subgroups train and test 
train_size <- round(dim(std_dd)[1]*0.7)
test_size <- dim(std_dd)[1]-train_size

train <- std_dd[1:train_size,]
test <- std_dd[(train_size+1):dim(std_dd)[1],]

y_train <- to_categorical(dd[1:train_size,9], 2)
y_test <- to_categorical(dd$test_diabet[(train_size+1):dim(std_dd)[1]], 2)


# install keras library

library(keras)
install_keras()

#building keras model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = 8) %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'sigmoid')


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(train, y_train, epochs = 50, batch_size = 5, validation_split = 0.2)


