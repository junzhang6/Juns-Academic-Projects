setwd("E:/SFU/2019Fall/STAT440/Module3 Gene")

library(tidyverse)
library(glmnet)
library(caret)
library(caretEnsemble)
library(leaps)
library(ROCR)

# train <- read.table('train.txt', sep=' ', row.names = NULL, header = TRUE)
# save("train", file = "train.Rdata")

load("train.Rdata")

test <- read.table('test.txt', sep=' ', row.names = NULL, header = TRUE)

#traning set
train_noNA <- filter(train, Level==0 | Level==1)

#Submission set
train_NA <- filter(train, is.na(Level)==T)

#Reponse y 
Level <- train_noNA$Level


X1 <- subset(train_noNA, select = -c(Level))

##############################################################################

lambdas <- seq(10^(-6), 0.15, length=500)
set.seed(123)
cv.lasso <- cv.glmnet(x=as.matrix(X1), y=as.factor(Level), family="binomial", 
                      type.measure="mse", alpha=1, standardize=FALSE, lambda=lambdas)
lambda_min <- cv.lasso$lambda.min

#Returns nonzero coefs
rownames(coef(cv.lasso, s='lambda.min'))[coef(cv.lasso, s='lambda.min')[,1]!= 0] 

#Less predictors to use than below & Remove V28787
predictors <- c("V3329", "V3534", "V5717", "V5740", "V7087", "V8584", "V17815", 
                "V24931", "V26294", "V30960", "V33370", "V45335", 
                "V47683", "V51303", "V52927")


#################################


#Reduce the size of the dataset w/ selected predictors 
train.reduce <- subset(train_noNA, select = c(predictors, "Level"))
test.reduce <- subset(train_NA, select = c(predictors, "Level"))

#Check correlation b/w variables 
# cor(train.reduce[, 1:16])
#Remove V28787

train.reduce$Level <- as.factor(train.reduce$Level)
levels(train.reduce$Level) <- c("control", "schiz")


#Ridge
lambdas <- seq(10^(-6), 0.15, length=500)
set.seed(123)
ridge.fit <- train(Level~., data=train.reduce, method = "glmnet", 
                   trControl = trainControl("LOOCV", savePredictions = 'final',
                                            classProbs = T), 
                   tuneGrid = expand.grid(alpha = 0, lambda=lambdas),
                   family="binomial")

#KNN
set.seed(123)
knn.fit <- train(Level~., data=train.reduce, method = "knn", 
                 trControl = trainControl("LOOCV", savePredictions = 'final',
                                          classProbs = T), tuneLength=5)
# print(knn.fit)


#Random Forest
mtry <- floor(sqrt(ncol(train.reduce[,-ncol(train.reduce)])))

set.seed(123)
rf.fit <- train(Level~., data=train.reduce, method = "rf", 
                trControl = trainControl("LOOCV", savePredictions = 'final',
                                         classProbs = T),
                tuneGrid=expand.grid(.mtry=c(3:15)))
# print(rf.fit)


#NN
set.seed(123)
nn.fit <- train(Level~., data=train.reduce, method = "nnet", 
                trControl = trainControl("LOOCV", savePredictions = 'final',
                                         classProbs = T), 
                tuneGrid = expand.grid(size = c(3:10),
                            decay = c(0, 0.1)))
# print(nn.fit)


#SVM
set.seed(123)
svm.fit <- train(Level~., data=train.reduce, method = "svmRadial", 
                 trControl = trainControl("LOOCV", savePredictions = 'final',
                                          classProbs = T))
# print(svm.fit)


#################################

#Stacking
train.reduce$ridge_pred <- ridge.fit$pred$schiz[order(ridge.fit$pred$rowIndex)]
train.reduce$knn_pred <- knn.fit$pred$schiz[order(knn.fit$pred$rowIndex)]
train.reduce$rf_pred <- rf.fit$pred$schiz[order(rf.fit$pred$rowIndex)]
train.reduce$nn_pred <- nn.fit$pred$schiz[order(nn.fit$pred$rowIndex)]
train.reduce$svm_pred <- svm.fit$pred$schiz[order(svm.fit$pred$rowIndex)]


test.reduce$ridge_pred <- predict(ridge.fit, test.reduce[,predictors], 
                                    type="prob")$schiz
test.reduce$knn_pred <- predict(knn.fit, test.reduce[,predictors], 
                                  type="prob")$schiz
test.reduce$rf_pred <- predict(rf.fit, test.reduce[,predictors], 
                                 type="prob")$schiz
test.reduce$nn_pred <- predict(nn.fit, test.reduce[,predictors], 
                                 type="prob")$schiz
test.reduce$svm_pred <- predict(svm.fit, test.reduce[,predictors], 
                                  type="prob")$schiz


#Predictions by averaging all 5 results
test.reduce <- test.reduce %>% 
    mutate(stacked_avg_pred = (ridge_pred+knn_pred+rf_pred+nn_pred+svm_pred)/5)


final.pred <- test.reduce$stacked_avg_pred

#AUC .88 on public (Wrong)
final.pred <- 1 - final.pred

##############################################################################

final.pred <- predict(rf.fit, test.reduce[,predictors], type="prob")$schiz

final.pred <- predict(nn.fit, test.reduce[,predictors], type="prob")$schiz


##############################################################################

ID <- as.numeric(gsub("\\,{1}.*", "", test$ID.Value))

result <- data.frame(ID=ID, Value=as.numeric(final.pred))

write.csv(result, file="result.csv", quote=T, row.names=FALSE)


##############################################################################




#Predictors for top layer models 
predictors_top <- c("ridge_pred", "knn_pred", "rf_pred", "nn_pred", "svm_pred") 


#NN as top layer model 
model_nn <- train(x=train.reduce[,predictors_top], y=as.factor(train.reduce[,"Level"]), 
                  method = "nnet", 
                  trControl = trainControl("LOOCV", savePredictions = 'final',
                                           classProbs = T), tuneLength=5)

#predict using NN top layer model
test.reduce$nn_stacked <- predict(model_nn, test.reduce[,predictors_top])


#Predictions stacked w/ NN
nn_stacked.pred <- predict(model_nn, test.reduce[,predictors_top], type="prob")






#################################

#Lasso to reduce the size of the dataset (USE THE ONE ABOVE)
lambdas <- seq(10^(-6), 0.15, length=500)
set.seed(123)
lasso <- train(x=as.matrix(X1), y=as.factor(Level), method = "glmnet", 
               trControl = trainControl("LOOCV"), 
               tuneGrid = expand.grid(alpha = 1, lambda=lambdas),
               family="binomial")
# print(lasso)
lasso$bestTune$lambda
coef(lasso$finalModel, lasso$bestTune$lambda)[as.numeric(coef(lasso$finalModel, 
                                                              lasso$bestTune$lambda))!=0, ]

predictors <- c("V2158", "V3329", "V3534", "V4087", "V5717", "V6931",
                "V8236", "V8837", "V13727", "V15171", "V18937", "V19909", 
                "V24931", "V25690", "V26390", "V30453", "V32970", "V33327", 
                "V36546", "V39672", "V40009", "V40800", "V41027", "V41599", 
                "V42514", "V42618", "V42873", "V43578", "V44393", "V45750", 
                "V45750", "V47683", "V48722", "V49407", "V50133", "V51303", 
                "V52729", "V52927")