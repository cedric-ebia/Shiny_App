## DOCUMENTATIONS:
  # https://stats.stackexchange.com/questions/10501/calculating-aupr-in-r
  # https://arxiv.org/pdf/1106.1813.pdf
 



rm(list=ls())

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")

install.packages("caret", repos = "http://cran.us.r-project.org")
install.packages("plotROC", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("reshape2", repos = "http://cran.us.r-project.org")
install.packages("DMwR", repos = "http://cran.us.r-project.org")
install.packages("PRROC", repos = "http://cran.us.r-project.org")
install.packages("gmodels", repos = "http://cran.us.r-project.org") 
install.packages("rpart", repos = "http://cran.us.r-project.org") 
install.packages("installr", repos = "http://cran.us.r-project.org")
install.packages("caTools", repos = "http://cran.us.r-project.org") 
install.packages("plyr", repos = "http://cran.us.r-project.org")
install.packages("e1071", repos = "http://cran.us.r-project.org")


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

#updateR()

data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(data)
data = data[,-c(1,30)] 
str(data)
summary(data) # Variable are standardised

print(table(data$Class))
print(prop.table(table(data$Class))) # At 5%, this is clearly a skewed data set, aka rare event.




nmiss <- function(x) {
  return(sum(is.na(x)))
}
apply(data,2,nmiss) # No missing value

#Correlation matrix
suppressMessages(library(reshape2))

corrmatrice<- round(cor(data[, c(1:29)]),2)
hc <- hclust(as.dist((1-corrmatrice)/2))
cormat <- corrmatrice[hc$order, hc$order]
cormat[lower.tri(cormat)]<-NA
melted_cormat <- melt(cormat, na.rm = TRUE)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1,1), space = "Lab",
    name="Pearson\nCorrelation") +
   theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
     size = 12, hjust = 1))+
  coord_fixed()
 
ggheatmap +
 geom_text(aes(Var2,Var1,label=value),color="black",size=4) +
 theme(
   axis.title.x = element_blank(),
   axis.title.y = element_blank(),
   panel.grid.major = element_blank(),
   panel.border = element_blank(),
   panel.background = element_blank(),
   axis.ticks = element_blank(),
   legend.justification = c(1, 0),
   legend.position = c(0.6, 0.7),
   legend.direction = "horizontal")+
   guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                 title.position = "top", title.hjust = 0.5))


# The Target Distribution
ggplot() + 
  geom_histogram(data = data, mapping = aes(Class) , color='red' , alpha=.4, stat="count")



# WHAT TO DO WITH TIME ???
data$Class[data$Class == 1] = 1
data$Class[data$Class == 0] = -1

cut =  data[1:50000,]
cut$Class = factor(cut$Class,level = c(1,-1))

set.seed(1337) 
split = sample.split(cut$Class, SplitRatio = 0.75) 
training_set = subset(cut, split == TRUE) 
test_set = subset(cut, split == FALSE)

print(prop.table(table(training_set$Class)))  #"We keep the same prop so it is fine"
print(prop.table(table(test_set$Class)))      #"We keep the same prop so it is fine"


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
                    kernel = 'polynomial',
                    probability = TRUE) 
    system.time(svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'polynomial',
                    probability = TRUE))

    y_pred = predict(classifier, newdata = test_set[,-29])
    (CrossTable(test_set[,29],y_pred,prop.chisq = FALSE)) #CrossTable is shit for classification with unbalanced distribution.
    pred = predict(classifier,test_set, probability = TRUE)

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

# kernel = radial basis : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    probability = TRUE)
    system.time(svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    probability = TRUE))


    y_pred = predict(classifier, newdata = test_set[,-29])
    (CrossTable(test_set[,29],y_pred,prop.chisq = FALSE)) #CrossTable is shit for classification with unbalanced distribution.

    pred = predict(classifier,test_set, probability = TRUE)
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

# kernel = sigmoid : 
    classifier = svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'sigmoid',
                    probability = TRUE) 
    system.time(svm(formula = Class ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'sigmoid',
                    probability = TRUE) )
     y_pred = predict(classifier, newdata = test_set[,-29], probability = TRUE)
    (CrossTable(test_set[,29],y_pred,prop.chisq = FALSE)) #CrossTable is shit for classification with unbalanced distribution.


    pred = predict(classifier,test_set, probability = TRUE)
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





##################################### TEST AUC #######################################


donneesModelROCALL <- NULL
MatriceConfusion <- data.frame()
fun.auc.ggplot <- function (ModelPred, ModelProb, ModelCible, Title = '', echantillon = '', df = donneesModelROCALL){

  donneesModel <- data.frame(Cible = as.integer(ModelCible), 
                             Cible.Nom = as.character(ModelCible),
                             ModelProb = ModelProb)
  
  basicplot <- ggplot(donneesModel, aes(d = Cible, m = ModelProb)) + 
                   geom_roc(n.cuts = 50, labelsize = 3, labelround = 4)

  basicplot<- basicplot  +
    style_roc(xlab='Le taux de faux Positifs(1 - Specificity)',
              ylab='Le taux de vrais positifs(Sensitivity)',
              theme = theme_grey)+
    annotate("text",x=0.5,y=0.5,
              label="AUC <= 0.5 prédiction pire qu'au hasard",
              color="red",size=6, angle=45) +
    ggtitle( paste('Surface sous courbe ROC (AUC) : ',round(calc_auc(basicplot)$AUC,8),"% --",Title)) + 
    coord_fixed(ratio = 1)

  donneesModel$Label <- rep(paste(Title," : ",round(calc_auc(basicplot)$AUC,4),"%"),length(ModelCible))
  donneesModel$Echantillon <- echantillon

  matconf <- caret::confusionMatrix(ModelPred, ModelCible,mode="everything")
  
  MatriceConfusion <<- rbind( MatriceConfusion,
                              data.frame(Nom=Title,
                                         AUC        =calc_auc(basicplot)$AUC,
                                         Accuracy   =matconf$overall[1],
                                         Kappa      =matconf$overall[2], 
                                         VP         =matconf$table[1,1],
                                         FP         =matconf$table[1,2],
                                         VN         =matconf$table[2,2],
                                         FN         =matconf$table[2,1],
                                         Sensitivity=matconf$byClass[1],  
                                         Specificity=matconf$byClass[2],
                                         Precision  =matconf$byClass[5],
                                         FScore1    =matconf$byClass[7],
                                         Prevalence =matconf$byClass[8],
                                         PPV        =matconf$byClass[3], 
                                         NPV        =matconf$byClass[4],
                                         row.names =NULL))

  donneesModelROCALL <<- rbind(df,donneesModel)

  basicplot
}

install.packages("kernlab", repos = "http://cran.us.r-project.org") 
library(kernlab)


model <- ksvm(Class~., data=training_set, prob.model=T , kernel = "rbfdot", C=2)
prediction <- predict(model, test_set[,-29], type="response")
prediction.probabilites <- predict(model, test_set[,-29], type="probabilities")
fun.auc.ggplot(prediction,
               prediction.probabilites[,2], 
               test_set$Class,
               paste('Support Vector Machine Radial-'))


################################################## LOGIT REG #############################################

