# KNN

rm(list=ls())

library(reshape2)
df = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
df$Class <- factor(df$Class)
df <- df[,2:31]

df_summarised <- df %>% 
  group_by(Class) %>% 
  summarise(amount_mean = mean(Amount), amount_median = median(Amount)) #medias y medianas no concuerdan
df_summarised <- melt(df_summarised,id.vars = "Class", measure.vars = c("amount_mean","amount_median"))

# Same as before but for Principal Component variables
df_reshaped <- melt(df,id.vars = "Class", measure.vars = colnames(df)[c(-29,-30)])

df_pc_summary <- df_reshaped %>%
  group_by(Class,variable) %>% 
  summarise(mean = mean(value), median = median(value))
df_pc_summary <- melt(df_pc_summary,id.vars = c("Class","variable"), measure.vars = c("mean","median"))
colnames(df_pc_summary) <- c("Class","variable","Stat","Value")

# Distribution of the Amount variable
ggplot(df, aes(Amount, fill = Class) ) + 
  geom_density(alpha = 0.5,  col = "black") +
  geom_vline( 
    data = df_summarised,
    aes(colour = Class,linetype = variable, xintercept=value),
    show.legend = TRUE
  ) +
  scale_fill_discrete(labels = c("Regular", "Fraud"))+
  scale_linetype_discrete(labels = c(amount_mean = "mean", amount_median = "median") ) +
  scale_color_discrete(breaks = NULL) +
  xlim(0,400) +
  labs(linetype = "Stats",
       title = "Density distribution of both classes",
       caption = "*x axis limited at 400\nfor better visualization",
       subtitle = "both classes come from diferent distributions")  +
  theme(
    axis.title.y = element_blank(),
    plot.caption = element_text(margin = margin(t = 15), color = "gray30", size = 10)
  )


# Plotting the distribution of PC variables 
ggplot(df_reshaped, aes(x = value, fill = Class) ) + 
  geom_density(alpha = 0.5,  col = "black") +
  geom_vline( 
    data = df_pc_summary,
    aes(colour = Class,linetype = Stat, xintercept=Value),
    show.legend = TRUE
  ) +
  facet_wrap("variable", ncol = 4, nrow = 7, scales = "free_y") +
  xlim(-5,5) +
  scale_fill_discrete(labels = c("Regular", "Fraud")) +
  scale_color_discrete(breaks = NULL) +
  labs(title = "Density Distribution for each PC variable",
       subtitle = "Mostly, they come from different distributions")+
  theme (
    axis.title.y = element_blank()
  )

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

