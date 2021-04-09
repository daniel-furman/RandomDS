#' ---
#' title: "Feature Selection Random Variable Test"
#' author: "Daniel Furman"
#' date: "Apr. 6 2021"
#' 

library(ggthemes)
library(caret)
library(randomForest)

options(warn = -1)
options(scipen = 1e5)

theme_df <- theme(legend.position = "right", legend.direction = "vertical", axis.text = element_text(size = 11), 
                  plot.caption = element_text(color = "black", size = 11), legend.text = element_text(size = 11), 
                  axis.title = element_text(size = 11, face = "bold", color = "black"), legend.title = element_text(size = 11), 
                  axis.line = element_line(size = 0.4), plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12))

## ------------------------------------------------------------------------

RandomVariableTest <- function(seed, data_dir){
  
  # seed: int, the Random State seed for the run
  # data_dir: string, directory with your training features and target
  # target: string, colname of target class for model

  
  # first steps
  set.seed(seed)
  train <- read.csv(data_dir)
  print(dim(train))

  # add the Random noise
  train$Random <- runif(nrow(train))

  model <- randomForest(as.factor(train[,1])~., data = train[,2:length(train[1,])],
                        ntree = 180)
  print(model)

  imp <- as.data.frame(model$importance)
  imp$variable <- rownames(imp)
  imp$isReal <- ifelse(imp$variable=="Random", "Noise", "Feature")

  ggplot(imp, aes(reorder(variable, +MeanDecreaseGini), MeanDecreaseGini, fill = isReal))+
    geom_bar(stat = "identity", width = 0.65)+
    scale_fill_manual(values = c("gray65", "red"))+
    coord_flip()+
    labs(title = "Variable importance", fill = "",
       x = "Variable", y = "Importance (mean decrease Gini)")+
    theme_fivethirtyeight()+
    theme_df
}

# Example use-case:

RandomVariableTest(192, 'data/env_train/env_train_xv_194.csv')




