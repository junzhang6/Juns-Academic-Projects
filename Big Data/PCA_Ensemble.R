setwd("E:/SFU/2019Fall/STAT440/Module3 Gene")

library(tidyverse)
library(glmnet)
library(caret)
library(caretEnsemble)
library(leaps)
library(ROCR)

load("train.Rdata")

test <- read.table('test.txt', sep=' ', row.names = NULL, header = TRUE)


#traning set
train_noNA <- filter(train, Level==0 | Level==1)

#Submission set
train_NA <- filter(train, is.na(Level)==T)

#Reponse y 
Level <- train_noNA$Level


##############################################################################

X.train <- train_noNA
X.train$Level <- as.factor(X.train$Level)
X.test <- train_NA

pc <- prcomp(X.train[,-ncol(X.train)], center = TRUE, scale. = TRUE)
summary(pc)

#New dataset using PCs
X.train.Pred <- predict(pc, X.train)
X.train.Pred <- data.frame(X.train.Pred, X.train[ncol(X.train)])
X.test.Pred <- predict(pc, X.test)
X.test.Pred <- as.data.frame(X.test.Pred)

#First 6 PCs
pc.train <- X.train.Pred[, c(1:6, ncol(X.train.Pred))]
pc.test <- X.test.Pred[, c(1:6)]

##############################################################################

levels(pc.train$Level) <- c("control", "schiz")

#Logistics
set.seed(123)
logi.fit <- train(Level~., data=pc.train, method="glm", family="binomial",
                  trControl=trainControl(method="LOOCV",
                                         savePredictions = 'final',
                                         classProbs = T), tuneLength=5)

logi.pred <- predict(logi.fit, newdata=pc.test, type="prob")


###############################

#KNN
set.seed(123)
knn.fit <- train(Level~., data=pc.train, method = "knn", 
                 trControl = trainControl("LOOCV", savePredictions = 'final',
                                          classProbs = T), tuneLength=10)
knn.pred <- predict(knn.fit, newdata=pc.test, type="prob")

###############################

#NN
set.seed(123)
nn.fit <- train(Level~., data=pc.train, method = "nnet", 
                trControl = trainControl("LOOCV", savePredictions = 'final',
                                         classProbs = T),
                tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
nn.pred <- predict(nn.fit, newdata=pc.test, type="prob")

###############################

#Random Forest
mtry <- floor(sqrt(ncol(pc.train[,-ncol(pc.train)])))

set.seed(123)
rf.fit <- train(Level~., data=pc.train, method = "rf", 
                trControl = trainControl("LOOCV", savePredictions = 'final',
                                         classProbs = T),
                tuneGrid=expand.grid(.mtry=mtry))

###############################

#SVM
set.seed(123)
svm.fit <- train(Level~., data=pc.train, method = "svmRadial", 
                 trControl = trainControl("LOOCV", savePredictions = 'final',
                                          classProbs = T))

##############################################################################

#Stacking
pc.train$logi_pred <- logi.fit$pred$schiz[order(logi.fit$pred$rowIndex)]
pc.train$knn_pred <- knn.fit$pred$schiz[order(knn.fit$pred$rowIndex)]
pc.train$nn_pred <- nn.fit$pred$schiz[order(nn.fit$pred$rowIndex)]
pc.train$rf_pred <- rf.fit$pred$schiz[order(rf.fit$pred$rowIndex)]
pc.train$svm_pred <- svm.fit$pred$schiz[order(svm.fit$pred$rowIndex)]


pc.test$logi_pred <- predict(logi.fit, pc.test[,1:6], 
                                  type="prob")$schiz
pc.test$knn_pred <- predict(knn.fit, pc.test[,1:6], 
                                type="prob")$schiz
pc.test$nn_pred <- predict(nn.fit, pc.test[,1:6], 
                               type="prob")$schiz
pc.test$rf_pred <- predict(rf.fit, pc.test[,1:6], 
                               type="prob")$schiz
pc.test$svm_pred <- predict(svm.fit, pc.test[,1:6], 
                                type="prob")$schiz


#Predictions by averaging all 5 results
pc.test <- pc.test %>% 
    mutate(stacked_avg_pred = (logi_pred+knn_pred+nn_pred+rf_pred+svm_pred)/5)

final.pred <- pc.test$stacked_avg_pred

final.pred <- 1 - final.pred

##############################################################################

ID <- as.numeric(gsub("\\,{1}.*", "", test$ID.Value))

result <- data.frame(ID=ID, Value=as.numeric(final.pred))

write.csv(result, file="result.csv", quote=T, row.names=FALSE)



