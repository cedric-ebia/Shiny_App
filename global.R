set.seed(1337)

library(pROC)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tibble)
library(shinycssloaders)
library(e1071)
library(precrec)
library(ggplot2)
library(dplyr)
library(class)
library(caret)
library(xgboost)
library(corrplot)
library(kernlab)

###### FUNCTIONS ######

customDownloadbutton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("accessible-icon"), label)
}


plot_confusion_matrix <- function(verset, sSubtitle) {
  tst <- data.frame(round(verset$predicted,0), verset$Class)
  opts <-  c("Predicted", "True")
  names(tst) <- opts
  cf <- plyr::count(tst)
  cf[opts][cf[opts]==0] <- "Not Fraud"
  cf[opts][cf[opts]==1] <- "Fraud"
  
  ggplot(data =  cf, mapping = aes(x = True, y = Predicted)) +
    labs(title = "Confusion matrix", subtitle = sSubtitle) +
    geom_tile(aes(fill = freq), colour = "grey") +
    geom_text(aes(label = sprintf("%1.0f", freq)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_bw() + theme(legend.position = "none")
}


########################################## 

df = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(df)


library(tibble)
my_data <- tribble(
  ~cat1,	~cat2,
  "PCA Variables",	"V1",
  "PCA Variables",	"V2",
  "PCA Variables",	"V3",
  "PCA Variables",	"V4",
  "PCA Variables",	"V5",
  "PCA Variables",	"V6",
  "PCA Variables",	"V7",
  "PCA Variables",	"V8",
  "PCA Variables",	"V9",
  "PCA Variables",	"V10",
  "PCA Variables",	"V11",
  "PCA Variables",	"V12",
  "PCA Variables",	"V13",
  "PCA Variables",	"V14",
  "PCA Variables",	"V15",
  "PCA Variables",	"V16",
  "PCA Variables",	"V17",
  "PCA Variables",	"V18",
  "PCA Variables",	"V20",
  "PCA Variables",	"V21",
  "PCA Variables",	"V22",
  "PCA Variables",	"V23",
  "PCA Variables",	"V24",
  "PCA Variables",	"V25",
  "PCA Variables",	"V26",
  "PCA Variables",	"V27",
  "PCA Variables",	"V28",
  "NON-PCA Variables",	"Time",
  "NON-PCA Variables",	"Amount",
  "Target", "Class"
)

lapply(split(my_data$cat2, my_data$cat1), as.list)




#### RUS scratch ######


train.test.split <- sample(2
                           , nrow(df)
                           , replace = TRUE
                           , prob = c(0.7, 0.3))
train = df[train.test.split == 1,]
attach(train)
test = df[train.test.split == 2,]
attach(test)



train_smote_1 = train %>%
  filter(Class == 1)

train_smote_0 = train %>%
  filter(Class == 0)

train_smote_0 = train_smote_0 %>%
  sample_n(8*nrow(train_smote_1), replace = FALSE)

train_smote_maison = rbind(train_smote_0,train_smote_1)


train_smote_maison$Class = factor(train_smote_maison$Class)


costs <- table(train_smote_maison$Class)  # the weight vector must be named with the classes names
costs[1] <- 0.1 # a class 0 mismatch not so much...
costs[2] <- 0.9 #a class -1 mismatch has a terrible cost
costs


test_svm_plot = df[train.test.split == 2,]


#train.test.split <- sample(2
                           #, nrow(df)
                           #, replace = TRUE
                           #, prob = c(0.7, 0.3))
#train_xgb = df[train.test.split == 1,]
#test_xgb = df[train.test.split == 2,]
#xgb.data.train <- xgb.DMatrix(as.matrix(train_xgb[, colnames(train_xgb) != "Class"]), label = train_xgb$Class)
#xgb.data.test <- xgb.DMatrix(as.matrix(test_xgb[, colnames(test_xgb) != "Class"]), label = test_xgb$Class)


df_xgb = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
df_xgb$Class <- factor(df_xgb$Class)
set.seed(1337) 
train.test.split <- sample(2
                           , nrow(df_xgb)
                           , replace = TRUE
                           , prob = c(0.7, 0.3))
train_xgb = df_xgb[train.test.split == 1,]
test_xgb = df_xgb[train.test.split == 2,]


set.seed(1337)
train_smote_1 = train_xgb %>%
  filter(Class == 1)

train_smote_0 = train_xgb %>%
  filter(Class == 0)

train_smote_0 = train_smote_0 %>%
  sample_n(8*nrow(train_smote_1), replace = FALSE)

train_smote_maison = rbind(train_smote_0,train_smote_1)
train_smote_maison$Class = factor(train_smote_maison$Class)


train_smote_maison_xgb = train_smote_maison
train_smote_maison_xgb$Class = as.integer(train_smote_maison_xgb$Class)
test_xgb$Class = as.integer(test_xgb$Class)


train_smote_maison_xgb$Class[train_smote_maison_xgb$Class == 1] = 0
train_smote_maison_xgb$Class[train_smote_maison_xgb$Class == 2] = 1

test_xgb$Class[test_xgb$Class == 1] = 0
test_xgb$Class[test_xgb$Class == 2] = 1

xgb.data.train <- xgb.DMatrix(as.matrix(train_smote_maison_xgb[, colnames(train_smote_maison_xgb) != "Class"]), label = train_smote_maison_xgb$Class)
xgb.data.test1 <- xgb.DMatrix(as.matrix(test_xgb1[, colnames(test_xgb) != "Class"]), label = test_xgb$Class)



test_glm = test



