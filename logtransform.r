
## Log transformation:



rm(list=ls())

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")

library(ROCR)
library(microbenchmark)
library(caret)
library(plotROC)
library(ggplot2)
library(reshape2)
library(DMwR)
library(PRROC)
library(gmodels)
library(rpart)
library(installr)
library(caTools)
library(plyr)
library(e1071)
library(pROC)

library(tidyverse)
library(xgboost)
library(caret)


credit.card.data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))


attach(credit.card.data)
credit.card.data = credit.card.data[,-c(1,30)] 
Class = credit.card.data$Class 

credit.card.data = log10(abs(credit.card.data[,1:28]))
credit.card.data = data.frame(credit.card.data,Class)
summary(credit.card.data)



set.seed(1337) 
train.test.split <- sample(2
                    , nrow(credit.card.data)
                    , replace = TRUE
                    , prob = c(0.7, 0.3))
train = credit.card.data[train.test.split == 1,]
test = credit.card.data[train.test.split == 2,]


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


# Use ROCR package to plot ROC Curve
xgb.pred <- prediction(xgb.test, test$Class)
xgb.perf <- performance(xgb.pred, "tpr", "fpr")

plot(xgb.perf,
     avg="threshold",
     colorize=TRUE,
     lwd=1,
     main="ROC Curve w/ Thresholds",
     print.cutoffs.at=seq(0, 1, by=0.05),
     text.adj=c(-0.5, 0.5),
     text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")
