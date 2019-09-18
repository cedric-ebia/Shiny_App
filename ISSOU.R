df = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
attach(df)

set.seed(1337) 
train.test.split <- sample(2
                           , nrow(df)
                           , replace = TRUE
                           , prob = c(0.1, 0.9))
train = df[train.test.split == 1,]
test = df[train.test.split == 2,]

classifier = svm(formula = Class ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear',
                 probability = TRUE,
                 #class.weights = costs,
                 scale = FALSE,
                 cross = 5) 

correlations <- cor(df,method="pearson") 
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")