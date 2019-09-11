rm(list=ls())



#source("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM/libraries.R")
source("/Users/PALMALOIC/Desktop/Shiny_App/libraries.r")

getwd()
#setwd("C:/Users/theloloboss/Desktop/M2 ESA/Projet_SVM")

#updateR()

data = data.frame(read.csv("creditcard.csv",header=TRUE,sep="," ,quote = "\""))
data %<>%
  mutate(id = 1:nrow(data)) %>%
  mutate(Class = as.integer(Class))

names(data) <- gsub('V', 'Feat', names(data))

numeric_interesting_features <- c(paste0('Feat', 1:28),'Amount') 
# "Class", the target, is not used to compute the 2D coordinates


data <- data[ apply(data, MARGIN = 1, FUN = function(x) !any(is.na(x))), ]

df <- (as.data.frame(data[numeric_interesting_features]))
# "Class", the target, is not used to compute the 2D coordinates

df_normalised <- apply(df, 
                       MARGIN = 2, 
                       FUN = function(x) {
                         scale(x, center = T, scale = T)
                       } )
df_normalised %<>%
  as.data.frame() %>%
  cbind(select(data, id))


data_fraud <- df_normalised %>%
  semi_join(filter(data, Class == 1), by = 'id')

data_sub <- df_normalised %>%
  sample_n(20000) %>% # sample of data
  rbind(data_fraud)

rtsne_out <- Rtsne(as.matrix(select(data_sub, -id)), pca = FALSE, verbose = TRUE,
                   theta = 0.3, max_iter = 1300, Y_init = NULL)
# "Class", the target, is not used to compute the 2D coordinates


# merge 2D coordinates with original features
tsne_coord <- as.data.frame(rtsne_out$Y) %>%
  cbind(select(data_sub, id)) %>%
  left_join(data, by = 'id')


gg <- ggplot() +
  labs(title = "All fraud (white dots) within the 10% dataset") +
  scale_fill_gradient(low = 'darkblue', high = 'red', name="Proportion\nof fraud per\nhexagon") +
  coord_fixed(ratio = 1) +
  theme_void() +
  stat_summary_hex(data = tsne_coord, aes(x=V1, y=V2, z = Class), bins=10, fun = mean, alpha = 0.9) +
  geom_point(data = filter(tsne_coord, Class == 0), aes(x = V1, y = V2), alpha = 0.3, size = 1, col = 'black') +
  geom_point(data = filter(tsne_coord, Class == 1), aes(x = V1, y = V2), alpha = 0.9, size = 0.8, col = 'white') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Arial'),
        legend.title.align=0.5)

quartz()
gg
#On about 10% of the data
