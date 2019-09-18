### XGBOOST 


rm(list=ls())

calculate_roc <- function(verset, cost_of_fp, cost_of_fn, n=100) {
  
  tp <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 1)
  }
  
  fp <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0)
  }
  
  tn <- function(verset, threshold) {
    sum(verset$predicted < threshold & verset$Class == 0)
  }
  
  fn <- function(verset, threshold) {
    sum(verset$predicted < threshold & verset$Class == 1)
  }
  
  tpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 1) / sum(verset$Class == 1)
  }
  
  fpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)
  }
  
  cost <- function(verset, threshold, cost_of_fp, cost_of_fn) {
    sum(verset$predicted >= threshold & verset$Class == 0) * cost_of_fp + 
      sum(verset$predicted < threshold & verset$Class == 1) * cost_of_fn
  }
  fpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)
  }
  
  threshold_round <- function(value, threshold)
  {
    return (as.integer(!(value < threshold)))
  }
  #calculate AUC (https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve)
  auc_ <- function(verset, threshold) {
    auc(verset$Class, threshold_round(verset$predicted,threshold))
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tp <- sapply(roc$threshold, function(th) tp(verset, th))
  roc$fp <- sapply(roc$threshold, function(th) fp(verset, th))
  roc$tn <- sapply(roc$threshold, function(th) tn(verset, th))
  roc$fn <- sapply(roc$threshold, function(th) fn(verset, th))
  roc$tpr <- sapply(roc$threshold, function(th) tpr(verset, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(verset, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(verset, th, cost_of_fp, cost_of_fn))
  roc$auc <-  sapply(roc$threshold, function(th) auc_(verset, th))
  
  return(roc)
}

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_auc <- ggplot(roc, aes(threshold, auc)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("AUC")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  # 
  grid.arrange(p_roc, p_auc, p_cost, ncol=2,sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
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
    scale_fill_gradient(low = "lightblue", high = "blue") +
    theme_bw() + theme(legend.position = "none")
  
}

source("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM/libraries.R")

getwd()
setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")



credit.card.data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(credit.card.data)
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
			, eval_metric = "aucpr"
			)
		, watchlist = list(train = xgb.data.train,test = xgb.data.test)
		, nrounds = 500
		, early_stopping_rounds = 40
		, print_every_n = 20
		)


# Make predictions on test set for ROC curve
test$predicted= predict(xgb.model
                   , newdata = as.matrix(test[, colnames(test) != "Class"])
                   , ntreelimit = xgb.model$bestInd)

plot_confusion_matrix(test, "XGBoost")

auc.xgb = roc(test$Class, xgb.test, plot = TRUE, col = "blue")
print(auc.xgb)



#### Evaluate The Model's Performance With A Confusion Matrix
pred.resp <- ifelse(xgb.test >= 0.5, 1, 0)
# Create the confusion matrix
credit.class.factor = factor(test$Class,c(0,1))
pred.resp = as.integer(pred.resp)
confusionMatrix(table(pred.resp, credit.class.factor), positive="1") #EHHH NOT GOOD

# Get the trained model
model <- xgb.dump(xgb.model, with_stats=TRUE)
# Get the feature real names
names = dimnames(train)[[2]]
# Compute feature importance matrix
(importance_matrix <- xgb.importance(names, model=xgb.model)[0:5]) 
# Plot
xgb.plot.importance(importance_matrix)


varimp <- data.frame(trainset.rf$importance)

vi1 <- ggplot(importance_matrix, aes(x=reorder(rownames(importance_matrix),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

vi2 <- ggplot(importance_matrix, aes(x=reorder(rownames(importance_matrix),X.IncMSE), y=X.IncMSE)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (%IncMSE)", x="Variable", y="Variable importance (%IncMSE)")

grid.arrange(vi1, vi2, ncol=2)


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

#Get feature importance
xgb.feature.imp = performance(model = xgb.model.acc.deeper)

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





# Get the trained model
model.deeper <- xgb.dump(xgb.model.acc.deeper, with_stats=TRUE)
# Get the feature real names
names = dimnames(train)[[2]]
# Compute feature importance matrix
(importance_matrix <- xgb.importance(names, model=xgb.model.acc.deeper)[0:5]) 

# Plot
xgb.plot.importance(importance_matrix)


  # Use ROCR package to plot ROC Curve
xgb.pred <- prediction(xgb.test.acc.deeper, test$Class)
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

