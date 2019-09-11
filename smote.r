# LETS SMOTE


rm(list=ls())

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")



data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(data)
data = data[,-c(1,30)] 



data$Class[data$Class == 1] = 1
data$Class[data$Class == 0] = -1

cut =  data[1:50000,]
cut$Class = factor(cut$Class,level = c(1,-1))

set.seed(1337) 
split = sample.split(cut$Class, SplitRatio = 0.75) 
training_set = subset(cut, split == TRUE) 
test_set = subset(cut, split == FALSE)
head(training_set)


library(DMwR)


over <- ((0.6*199028)-337)/337
under <- (0.4*199028)/(337*over)

over_perc <- round(over,1)*100
under_perc <- round(under,1)*100


training_set$Class <- as.factor(training_set$Class)
training_set <- SMOTE(Class ~ ., training_set, perc.over = over_perc, perc.under=under_perc)
training_set$Class <- as.numeric(training_set$Class) #-1 IS BEING REPLACE BY 2


prop.table(table(training_set$Class))
dim(training_set)


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

