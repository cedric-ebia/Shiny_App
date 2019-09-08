rm(list=ls())

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")



install.packages("rpart", repos = "http://cran.us.r-project.org") 
install.packages("installr", repos = "http://cran.us.r-project.org")
install.packages("caTools", repos = "http://cran.us.r-project.org") 
install.packages("plyr", repos = "http://cran.us.r-project.org")
install.packages("e1071", repos = "http://cran.us.r-project.org")

library(rpart)
library(installr)
library(caTools)
library(plyr)
library(e1071)

#updateR()

data = data.frame(read.csv("creditcard.csv",header=TRUE,sep=","))


cut =  data[1:10000,]
cut$Class = factor(cut$Class,level = c(0,1))

set.seed(1337) 
split = sample.split(cut$Class, SplitRatio = 0.75) 
training_set = subset(cut, split == TRUE) 
test_set = subset(cut, split == FALSE)

#Feature scaling:
training_set[-31] = scale(training_set[-31]) 
test_set[-31] = scale(test_set[-31]) 



# kernel = linear : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'linear') 

    y_pred = predict(classifier, newdata = test_set[,-31])
    (confusion = table(test_set[,31],y_pred))
    (exactitude = sum(diag(confusion))/sum(confusion))



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


    y_pred = predict(classifier, newdata = test_set[,-31])
    (confusion = table(test_set[,31],y_pred))
    (exactitude = sum(diag(confusion))/sum(confusion))

# kernel = radial basis : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'radial basis')
    (exactitude = sum(diag(confusion))/sum(confusion)) 


    y_pred = predict(classifier, newdata = test_set[,-31])
    (confusion = table(test_set[,31],y_pred))
    (exactitude = sum(diag(confusion))/sum(confusion))

# kernel = sigmoid : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'sigmoid') 
    y_pred = predict(classifier, newdata = test_set[,-31])
    (confusion = table(test_set[,31],y_pred)) #Very bad 
    (exactitude = sum(diag(confusion))/sum(confusion))













cut =  data
cut$Class = factor(cut$Class,level = c(0,1))

set.seed(1337) 
split = sample.split(cut$Class, SplitRatio = 0.75) 
training_set = subset(cut, split == TRUE) 
test_set = subset(cut, split == FALSE)

#Feature scaling:
training_set[-31] = scale(training_set[-31]) 
test_set[-31] = scale(test_set[-31]) 

# kernel = linear : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'linear') 

    y_pred = predict(classifier, newdata = test_set[,-31])
    (confusion = table(test_set[,31],y_pred))
    (exactitude = sum(diag(confusion))/sum(confusion))


