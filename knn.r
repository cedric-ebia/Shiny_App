# KNN

rm(list=ls())

source("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM/libraries.R")

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")



credit.card.data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))


attach(credit.card.data)
credit.card.data = credit.card.data[1:100000,-c(1,30)] 

set.seed(1337) 
train.test.split <- sample(2
                    , nrow(credit.card.data)
                    , replace = TRUE
                    , prob = c(0.7, 0.3))
train = credit.card.data[train.test.split == 1,]
test = credit.card.data[train.test.split == 2,]


nmiss <- function(x) {
  return(sum(is.na(x)))
}
apply(train,2,nmiss) # No missing value

mean=[]
auc = []
ks = 1:10
for (i in 1:10){
        pred = knn(train, test, cl=train$Class,k=i, prob = TRUE)
        mean=mean(test$Class != pred)
        tab = table(pred,test$Class)
        auc = roc(test$Class, attributes(pred)$prob)
            print(i)
            print(tab)
            print(mean)
            print(auc)
            plot(roc(test$Class, attributes(pred)$prob),
        print.thres = T,
        print.auc=T)
}

# KNN WITH SMOTTED DATA

library(DMwR)

#Over = ( (0.6 * COMMON_NO) - RARE_NO ) / RARE_NO
#Under = (0.4 * COMMON_NO) / (RARE_NO * Over)



train$Class = as.factor(train$Class)
train = SMOTE(Class ~ ., train, perc.over = 100)
train$Class = as.numeric(train$Class) # IS BEING REPLACE BY 2
train$Class[train$Class == 1] = 0
train$Class[train$Class == 2] = 1


nmiss <- function(x) {
  return(sum(is.na(x)))
}
apply(train,2,nmiss) # No missing value

mean=[]
auc = []
ks = 1:10
for (i in 1:10){
        pred = knn(train, test, cl=train$Class,k=i, prob = TRUE)
        mean=mean(test$Class != pred)
        tab = table(pred,test$Class)
        auc = roc(test$Class, attributes(pred)$prob)
            print(i)
            print(tab)
            print(mean)
            print(auc)
            plot(roc(test$Class, attributes(pred)$prob),
        print.thres = T,
        print.auc=T)
}

