# LETS SMOTE


rm(list=ls())



source("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM/libraries.R")

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")


credit.card.data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(credit.card.data)
credit.card.data = credit.card.data[,-c(1,30)] 

set.seed(1337) 
train.test.split <- sample(2
                    , nrow(credit.card.data)
                    , replace = TRUE
                    , prob = c(0.7, 0.3))
train = credit.card.data[train.test.split == 1,]
test = credit.card.data[train.test.split == 2,]


library(DMwR)

#Over = ( (0.6 * COMMON_NO) - RARE_NO ) / RARE_NO
#Under = (0.4 * COMMON_NO) / (RARE_NO * Over)

over = ((0.6*199028)-337)/337
under = (0.4*199028)/(337*over)

over_perc = round(over,1)*100
under_perc = round(under,1)*100


train$Class = as.factor(train$Class)
train = SMOTE(Class ~ ., train, perc.over = over_perc, perc.under=under_perc)
train$Class = as.numeric(train$Class) # IS BEING REPLACE BY 2
train$Class[train$Class == 1] = 0
train$Class[train$Class == 2] = 1


prop.table(table(train$Class))
dim(train)


xgb.data.train <- xgb.DMatrix(as.matrix(train[, colnames(train) != "Class"]), label = train$Class)
xgb.data.test <- xgb.DMatrix(as.matrix(test[, colnames(test) != "Class"]), label = test$Class)


xgb.model <- xgb.train(data = xgb.data.train
		, params = list(objective = "binary:logistic"
			, eta = 0.1
			, max.depth = 3
			, min_child_weight = 100
			, subsample = 1
			, colsample_bytree = 1
			, nthread = 3
			, eval_metric = "auc"
			)
		, watchlist = list(train = xgb.data.train,test = xgb.data.test)
		, nrounds = 500
		, early_stopping_rounds = 40
		, print_every_n = 20
		)
print(xgb.model$best_score)


# Make predictions on test set for ROC curve
xgb.test = predict(xgb.model
                   , newdata = as.matrix(test[, colnames(test) != "Class"])
                   , ntreelimit = xgb.model$bestInd)
auc.xgb = roc(test$Class, xgb.test, plot = TRUE, col = "blue")
print(auc.xgb)



#### Evaluate The Model's Performance With A Confusion Matrix
pred.resp <- ifelse(xgb.test >= 0.5, 1, 0)
# Create the confusion matrix
credit.class.factor = factor(test$Class,c(0,1))
pred.resp = as.integer(pred.resp)
confusionMatrix(table(pred.resp, credit.class.factor), positive="1") #EHHH NOT GOOD


######################################################### Train a deeper xgboost model to compare accuarcy. ####################################
xgb.model.acc.deeper <- xgb.train(data = xgb.data.train
		, params = list(objective = "binary:logistic"
			, eta = 0.1
			, max.depth = 7
			, min_child_weight = 100
			, subsample = 1
			, colsample_bytree = 1
			, nthread = 3
			, eval_metric = "auc"
			)
		, watchlist = list(train = xgb.data.train, test = xgb.data.test)
		, nrounds = 500
		, early_stopping_rounds = 40
		, print_every_n = 20
		)

print(xgb.model.acc.deeper$best_score)



# Make predictions on test set for ROC curve
xgb.test.acc.deeper = predict(xgb.model.acc.deeper
                   , newdata = as.matrix(test[, colnames(test) != "Class"])
                   , ntreelimit = xgb.model.acc.deeper$bestInd)



auc.xgb.acc.deeper = roc(test$Class, xgb.test.acc.deeper, plot = TRUE, col = "blue")
print(auc.xgb.acc.deeper)


#### Evaluate The Model's Performance With A Confusion Matrix
pred.resp.acc.deeper <- ifelse(xgb.test.acc.deeper >= 0.5, 1, 0)
# Create the confusion matrix
credit.class.factor = factor(test$Class,c(0,1))
pred.resp.acc.deeper = as.integer(pred.resp.acc.deeper)
confusionMatrix(table(pred.resp.acc.deeper, credit.class.factor), positive="1") #EHHH NOT GOOD






############## SMOTE 1:1 ######################################



credit.card.data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(credit.card.data)
credit.card.data = credit.card.data[,-c(1,30)] 

set.seed(1337) 
train.test.split <- sample(2
                    , nrow(credit.card.data)
                    , replace = TRUE
                    , prob = c(0.7, 0.3))
train = credit.card.data[train.test.split == 1,]
test = credit.card.data[train.test.split == 2,]


library(DMwR)




train$Class = as.factor(train$Class)
train = SMOTE(Class ~ ., train, perc.over = 100)
train$Class = as.numeric(train$Class) # IS BEING REPLACE BY 2
train$Class[train$Class == 1] = 0
train$Class[train$Class == 2] = 1

prop.table(table(train$Class))
dim(train)


xgb.data.train <- xgb.DMatrix(as.matrix(train[, colnames(train) != "Class"]), label = train$Class)
xgb.data.test <- xgb.DMatrix(as.matrix(test[, colnames(test) != "Class"]), label = test$Class)


xgb.model <- xgb.train(data = xgb.data.train
		, params = list(objective = "binary:logistic"
			, eta = 0.1
			, max.depth = 3
			, min_child_weight = 100
			, subsample = 1
			, colsample_bytree = 1
			, nthread = 3
			, eval_metric = "auc"
			)
		, watchlist = list(train = xgb.data.train,test = xgb.data.test)
		, nrounds = 500
		, early_stopping_rounds = 40
		, print_every_n = 20
		)
print(xgb.model$best_score)


# Make predictions on test set for ROC curve
xgb.test = predict(xgb.model
                   , newdata = as.matrix(test[, colnames(test) != "Class"])
                   , ntreelimit = xgb.model$bestInd)
auc.xgb = roc(test$Class, xgb.test, plot = TRUE, col = "blue")
print(auc.xgb)



#### Evaluate The Model's Performance With A Confusion Matrix
pred.resp <- ifelse(xgb.test >= 0.5, 1, 0)
# Create the confusion matrix
credit.class.factor = factor(test$Class,c(0,1))
pred.resp = as.integer(pred.resp)
confusionMatrix(table(pred.resp, credit.class.factor), positive="1") #EHHH NOT GOOD


######################################################### Train a deeper xgboost model to compare accuarcy. ####################################
xgb.model.acc.deeper <- xgb.train(data = xgb.data.train
		, params = list(objective = "binary:logistic"
			, eta = 0.1
			, max.depth = 7
			, min_child_weight = 100
			, subsample = 1
			, colsample_bytree = 1
			, nthread = 3
			, eval_metric = "auc"
			)
		, watchlist = list(train = xgb.data.train, test = xgb.data.test)
		, nrounds = 500
		, early_stopping_rounds = 40
		, print_every_n = 20
		)

print(xgb.model.acc.deeper$best_score)



# Make predictions on test set for ROC curve
xgb.test.acc.deeper = predict(xgb.model.acc.deeper
                   , newdata = as.matrix(test[, colnames(test) != "Class"])
                   , ntreelimit = xgb.model.acc.deeper$bestInd)



auc.xgb.acc.deeper = roc(test$Class, xgb.test.acc.deeper, plot = TRUE, col = "blue")
print(auc.xgb.acc.deeper)


#### Evaluate The Model's Performance With A Confusion Matrix
pred.resp.acc.deeper <- ifelse(xgb.test.acc.deeper >= 0.5, 1, 0)
# Create the confusion matrix
credit.class.factor = factor(test$Class,c(0,1))
pred.resp.acc.deeper = as.integer(pred.resp.acc.deeper)
confusionMatrix(table(pred.resp.acc.deeper, credit.class.factor), positive="1") #EHHH NOT GOOD
















# kernel = linear : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    probability = TRUE) 
    system.time(svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    probability = TRUE) )
    summary(classifier)

    y_pred = predict(classifier, newdata = test_set[,-29], probability = TRUE)
    pred = predict(classifier,test_set,probability = TRUE)
    (CrossTable(test_set[,29],y_pred,prop.chisq = FALSE)) #CrossTable is shit for classification with unbalanced distribution.
    probs = head(attr(pred,"probabilities"))
    probs
fg <- probs[ , 2]
bg <- probs[ , 1]
# ROC Curve    
roc <- roc.curve(scores.class0 = probs[,1], scores.class1 = probs[,2], curve = T)
plot(roc)
# PR Curve
pr <- pr.curve(scores.class0 = -probs[,1], scores.class1 =  -probs[,2], curve = T, sorted = FALSE)
plot(pr)

