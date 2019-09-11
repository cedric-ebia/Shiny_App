## DOCUMENTATIONS:
  # https://stats.stackexchange.com/questions/10501/calculating-aupr-in-r
  # https://arxiv.org/pdf/1106.1813.pdf
 
  

rm(list=ls())

getwd()
#setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")

install.packages("DMwR", repos = "http://cran.us.r-project.org")
install.packages("PRROC", repos = "http://cran.us.r-project.org")
install.packages("gmodels", repos = "http://cran.us.r-project.org") 
install.packages("rpart", repos = "http://cran.us.r-project.org") 
install.packages("installr", repos = "http://cran.us.r-project.org")
install.packages("caTools", repos = "http://cran.us.r-project.org") 
install.packages("plyr", repos = "http://cran.us.r-project.org")
install.packages("e1071", repos = "http://cran.us.r-project.org")

library(DMwR)
library(PRROC)
library(gmodels)
library(rpart)
library(installr)
library(caTools)
library(plyr)
library(e1071)

#updateR()

data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
data = data[,-1]
str(data)


nmiss <- function(x) {
  return(sum(is.na(x)))
}
apply(data,2,nmiss) # No missing value


# WHAT TO DO WITH TIME ???
data$Class[data$Class == 1] = 1
data$Class[data$Class == 0] = -1

cut =  data[1:50000,]
cut$Class = factor(cut$Class,level = c(1,-1))

set.seed(1337) 
split = sample.split(cut$Class, SplitRatio = 0.75) 
training_set = subset(cut, split == TRUE) 
test_set = subset(cut, split == FALSE)

#Feature scaling: NOT SURE IF THIS IS OKAY TO DO IT 
#training_set[-31] = scale(training_set[-31]) 
#test_set[-31] = scale(test_set[-31]) 

newcard<-SMOTE(Class ~ .,training_set,perc.over=10000, perc.under=101)
table(newcard$Class)



# kernel = linear : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'linear',
                    probability = TRUE) 
    summary(classifier)
    y_pred = predict(classifier, newdata = test_set[,-30], probability = TRUE)
    pred = predict(classifier,test_set,probability = TRUE)
    (CrossTable(test_set[,31],y_pred,prop.chisq = FALSE)) #CrossTable is shit for classification with unbalanced distribution.
    probs = head(attr(pred,"probabilities"))
    probs
fg <- probs[ , 2]
bg <- probs[ , 1]
# ROC Curve    
roc <- roc.curve(scores.class0 = -probs[,1], scores.class1 = -probs[,2], curve = T)
plot(roc)
# PR Curve
pr <- pr.curve(scores.class0 = -probs[,1], scores.class1 =  -probs[,2], curve = T, sorted = FALSE)
plot(pr)


# Hyperparameters optimization
tuneResult <- tune(svm, Class ~ .,  data = training_set,
              ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)




# kernel = polynomial : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'polynomial') 


    y_pred = predict(classifier, newdata = test_set[,-30])
    (confusion = table(test_set[,30],y_pred))
    (exactitude = sum(diag(confusion))/sum(confusion))

# kernel = radial basis : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'radial basis')
    (exactitude = sum(diag(confusion))/sum(confusion)) 


    y_pred = predict(classifier, newdata = test_set[,-30])
    (confusion = table(test_set[,30],y_pred))
    (exactitude = sum(diag(confusion))/sum(confusion))

# kernel = sigmoid : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'sigmoid') 
    y_pred = predict(classifier, newdata = test_set[,-30])
    (confusion = table(test_set[,30],y_pred)) #Very bad 
    (exactitude = sum(diag(confusion))/sum(confusion))












