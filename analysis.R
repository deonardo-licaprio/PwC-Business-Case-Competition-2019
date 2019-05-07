rm(list = ls()) gc()
set.seed(0)

#Load packages
library(tidyverse)
library(magrittr)
library(fastDummies)
library(xgboost)
library(caret)
library(moments)
library(glmnet)
library(doParallel)

#Load functions
source("R/gini_function.R")
source("R/utilities.R")

#Load data
file_name <- "final_data.csv"
df <- read_csv(file = paste0("data/",file_name))

#Peek at the data
glimpse(df) 

##Divorce_number_1000 is incorrectly assigned as string
if(is.character(df$Divorce_number_1000)) {
    df$Divorce_number_1000 <- as.numeric(sub(",", ".", df$Divorce_number_1000))
}

#Check missing data
NA_count <- apply(is.na(df), 2, sum)
NA_DF <- data.frame(variable = names(df), missing = NA_count)
NA_DF$share <- round(((NA_DF$missing/nrow(df))*100),1)
NA_DF %>%
    ggplot(aes(x = variable, y = share)) +
    geom_bar(stat='identity') + coord_flip(y=c(0,110)) +
    labs(x = "", y = "Percentage of missing data") +
    geom_text(aes(label=paste0(NA_DF$share, "%"), hjust=-0.1))

#Check features counts
#Display 10 variables with the least amount of unique values
var_names <- names(df)[-length(df)]
cnt_per_var <- data.frame(var = var_names, cnt = apply(df[,var_names], 2, get_uniqe_counts))
cnt_per_var %<>% arrange(cnt)
cnt_per_var[1:10,] %>%
    ggplot(aes(x = reorder(var, cnt), y = cnt)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = cnt), vjust = -1, color = "black", size=3.5) +
    labs(x = "10 variables with the least amount of unique values", y = "Count") +
    theme_minimal()

#Create train set
df_train <- df %>%
    filter(!is.na(DefFlag))

#Check data unbalance
df_train %>%
    ggplot(aes(x = factor(DefFlag))) +
    geom_bar(fill = "steelblue") +
    geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "white") +
    ylab("Ilość") +
    xlab("") +
    theme_minimal()
##Unbalance to be adressed during modeling phase

#Feature engineering


