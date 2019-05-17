#classification applications

## Classification with kNN ##

library(class)
knn_predict<- knn(train = train, test = test, cl = train$dd.test_diabet,k =5)


## Classification with logistic regression ##

model <- glm(dd.test_diabet~.,family=binomial(link='logit'),data=train)
glm_predict <- predict(model,newdata=test[,1:8],type='response')
fixed_glm_predict <- ifelse(glm_predict > 0.5,1,0)
glm_pr <- prediction(fixed_glm_predict, test$dd.test_diabet)


## Classification with classification tree ##

model <- ctree(dd.test_diabet ~.,data = train)
cftree_predict <- predict(model, newdata=test[,1:8],type="response")
fixed_cftree_predict <- ifelse(cftree_predict > 0.5,1,0)
cftree_pr <- prediction(fixed_cftree_predict, test$dd.test_diabet)


## Comparing Results ##

# Create confusion matrix

#for kNN
table(knn_predict, test$dd.test_diabet)

#for glm
table(fixed_glm_predict, test$dd.test_diabet)

#for classification tree
table(fixed_cftree_predict, test$dd.test_diabet)


#Draw ROC curves and calculate AUC

library(ROCR)

glm_prf <- performance(glm_pr, measure = "tpr", x.measure = "fpr")
plot(glm_prf)
auc <- performance(glm_pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

cftree_prf <- performance(cftree_pr, measure = "tpr", x.measure = "fpr")
plot(cftree_prf)
auc <- performance(cftree_pr, measure = "auc")
auc <- auc@y.values[[1]]
auc




