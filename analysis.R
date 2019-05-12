rm(list = ls()) gc()
set.seed(0)

#Load packages
library(tidyverse)
library(magrittr)
library(fastDummies)
library(xgboost)
library(caret)
library(moments)
library(lime)

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

X <- xgb.DMatrix(data = as.matrix(df_model), label = y)
cols <- colnames(X)
#rm(df_model); gc()

#Training model
p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 4,
          eta = 0.05, 
          max_depth = 4, 
          min_child_weight = 30, 
          gamma = 0,
          subsample = 0.8, 
          colsample_bytree = 0.75, 
          colsample_bylevel = 0.85, 
          alpha = 0,
          lambda = 1, 
          nrounds = 2000,
          scale_pos_weight = 9
)

cv_xgb <- xgb.cv(p, X, p$nrounds, nfold = 10, print_every_n = 30, early_stopping_rounds = 100)

min_auc_index <- cv_xgb$best_iteration
min_auc <- cv_xgb$evaluation_log[min_auc_index]$test_auc_mean

m_xgb <- xgb.train(p, X, nrounds = min_auc_index)

#Saving model
xgb.save(m_xgb, 'models/m_xgb_0512')

#Variable importance plot
xgb.importance(cols, model = m_xgb) %>% 
    xgb.plot.importance(top_n = 15, rel_to_first = T)

#Preparing test data
df_test <- df %>%
    filter(is.na(DefFlag)) %>%
    create_features()

df_test[,var_to_factor] <- lapply(df_test[,var_to_factor], as.factor)
df_test %<>% select(-Application_ID, -DefFlag)

var_fac_test <- select_if(df_test, is.factor)
var_num_test <- select_if(df_test, is.numeric)

##Dummy variables
dummy_fac_test <- fastDummies::dummy_cols(var_fac_test, remove_first_dummy = TRUE)
var_dummy_test <- dummy_fac_test %>% select(-Job_type, -Credit_purpose, -Car_status, -Home_status, -Marital_status)

df_test <- cbind(var_dummy_test, var_num_test)
y_test <- df %>% filter(is.na(DefFlag)) %>% select(DefFlag) %>% as.matrix()
X_test <- xgb.DMatrix(data = as.matrix(df_test), label = y_test)

#LIME
explainer <- lime(df_model, m_xgb)

explanation <- explain(df_test[1:floor(nrow(df_test)/2),], explainer, n_features = 5, n_labels = 1)
plot_features(explanation)

#Prediction

##Submission

