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

#Loading functions
source("R/gini_function.R")
source("R/utilities.R")

#Loading data
file_name <- "final_data.csv"
df <- read_csv(file = paste0("data/",file_name))

#Peek at the data
glimpse(df) 

##Divorce_number_1000 is incorrectly assigned as string
if(is.character(df$Divorce_number_1000)) {
    df$Divorce_number_1000 <- as.numeric(sub(",", ".", df$Divorce_number_1000))
}

#Checking missing data
NA_count <- apply(is.na(df), 2, sum)
NA_DF <- data.frame(variable = names(df), missing = NA_count)
NA_DF$share <- round(((NA_DF$missing/nrow(df))*100),1)
NA_DF %>%
    ggplot(aes(x = variable, y = share)) +
    geom_bar(stat='identity') + coord_flip(y=c(0,110)) +
    labs(x = "", y = "Percentage of missing data") +
    geom_text(aes(label=paste0(NA_DF$share, "%"), hjust=-0.1))

#Checking features counts
#Displaying 10 variables with the least amount of unique values
var_names <- names(df)[-length(df)]
cnt_per_var <- data.frame(var = var_names, cnt = apply(df[,var_names], 2, get_uniqe_counts))
cnt_per_var %<>% arrange(cnt)
cnt_per_var[1:10,] %>%
    ggplot(aes(x = reorder(var, cnt), y = cnt)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = cnt), vjust = -1, color = "black", size=3.5) +
    labs(x = "10 variables with the least amount of unique values", y = "Count") +
    theme_minimal()

#Creating train set
df_train <- df %>%
    filter(!is.na(DefFlag))

#Checking data unbalance
df_train %>%
    ggplot(aes(x = factor(DefFlag))) +
    geom_bar(fill = "steelblue") +
    geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "white") +
    ylab("Ilość") +
    xlab("") +
    theme_minimal()
##Unbalance to be adressed during modeling phase

#Feature engineering
#TODO: convert to one function
value_var_names <- df %>% 
    select(starts_with("NotionalValue_")) %>% 
    names()

dpd_var_names <- df %>% 
    select(starts_with("DPD_")) %>% 
    names() 

overdue_var_names <- df %>% 
    select(starts_with("NotionalOverdue_")) %>% 
    names()

spending_var_names <- df %>% 
    select(starts_with("Spending_")) %>% 
    names()

cat("Can take ~30 seconds...\n")
df_train_fe <- df_train %>% 
    create_features()

#Preparing data
df_model <- df_train_fe 

y <- df_model$DefFlag
var_to_factor <- c("Job_type", "Marital_status", "Home_status", "Car_status", "Credit_purpose")
df_model[,var_to_factor] <- lapply(df_model[,var_to_factor], as.factor)
df_model %<>% select(-Application_ID, -DefFlag)

var_fac <- select_if(df_model, is.factor)
var_num <- select_if(df_model, is.numeric)

##Dummy variables
dummy_fac <- fastDummies::dummy_cols(var_fac, remove_first_dummy = TRUE)
var_dummy <- dummy_fac %>% select(-Job_type, -Credit_purpose, -Car_status, -Home_status, -Marital_status)

df_model <- cbind(var_dummy, var_num)

#Training model













