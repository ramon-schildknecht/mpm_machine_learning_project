library(tidyverse)
library(DataExplorer) # https://github.com/boxuancui/DataExplorer
library(glmnet)
library(magrittr)
library(janitor) # https://github.com/sfirke/janitor
library(plotly)
library(GGally)
library(corrplot)
library(PerformanceAnalytics)
library(caret)


# set printing preferences
options(scipen = 99) # penalty for displaying scientific notation
options(digits = 4) # suggested number of digits to display

######################## skip this ... ##################################################################

# preparation step: loading data from https://www.kaggle.com/wendykan/lending-club-loan-data
# load data
data_temp <- read_csv("data/loan.csv")

# select years 2007 - 2015
data_temp %<>% 
  mutate(year_temp = as.integer(str_sub(issue_d,-4,-1))) %>%
  filter(year_temp >= 2007,
         year_temp <= 2015)


# subset our group subset
# d <- data_temp[which(data_temp$id%%8+1==6),] # not working: no id values

data_temp_2 <- cbind(id_2 = rownames(data_temp), data_temp)
rownames(data_temp_2) <- 1:nrow(data_temp_2)
data_temp_2$id_2 <- as.numeric(data_temp_2$id_2)
dataset_3 <- data_temp_2[which(data_temp_2$id_2%%8+1==3),]

# we continue working with d for easier coding reasons
d <- dataset_3 

# transform data frame into much nicer tibble format for working with
# huge datasets
d <- as_tibble(d)

# remove big data file for memory reasons
rm(data_temp)
rm(data_temp_2)
rm(dataset_3)

# write copy of group 3 data
path_file <- "./data/data_group_3"
if (!file.exists(path_file)) {
  write_csv(d, path = "data/copy_data_group_3.csv")
}
 ######################## 

######################## start from here ... ############################################################

# import already generated file for group 3 
d <- read_csv("data/data_group_3.csv") #, n_max = 2000) 
d <- as_tibble(d)


# experimenting with janitor package functions #
compare_df_cols(d) %>% arrange(d, column_name)
compare_df_cols(d) %>% arrange(d, column_name) %>% count(d) %>% arrange(desc(n))
d %>% remove_empty(c("rows", "cols")) # shows 15 empty columns
d %>% remove_constant() # shows no column with just single values
# d %>% clean_names() # not necessary -> already clean column names available
d %>% get_dupes() # there are no duplicates
d %>% tabyl(int_rate) %>% arrange(desc(n)) %>% plot_ly(x = ~n, type = "histogram")
d %>% tabyl(int_rate) %>% arrange(desc(n)) %>% plot_ly(x = ~n, type = "box")
d %>% tabyl(emp_length, term, home_ownership, show_missing_levels = FALSE)
d %>%
  tabyl(emp_length, home_ownership) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined")
# insights for later data preprocessing
## Missing observations/values: 6,496,539
## Total observations/values: 16,306,710 (about 40%)
## columns with NAs: 14 (about 10%)
## 
## 




######################################
#
# Conclusion Part 1 (regression):
# tbd, interpretation of results
# 
######################################

# goal: We aim at finding a regression for estimating the interest rate applied
# to a particular lending request.

# create copy of dataset

d_copy <- d

# data preparation --> check for NAs in int_rate and remove them if there are any
d %>%
  select(int_rate) %>%  
  summarise_all(funs(sum(is.na(.))))
# there are no NAs --> nothing to do

#further NAs investigation
create_report(d) 
## check generated file!
## too much variables to see all NAs attributes clearly
## split variables
plot_missing(d[1:75])
plot_missing(d[76:147])
## there are a lot of variables with an NA-Quota bigger than 5%

# generate columns where NA quota is bigger or equal than 5% of rows
d_na_quoate_bigger_equals_0.05 <- d %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% 
  as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
  select(count = V1, everything()) %>% arrange(desc(count)) %>% 
  mutate(quote = count/length(d$id)) %>% filter(quote >= 0.05)
(d_count_columns_na_quoate_bigger_equals_0.05 <- length(d_na_quoate_bigger_equals_0.05$column_names))
# 93 columns with NA quote >= 5%

# remove 93 columns with NA quota >= 0.05%
d_filter_temp <- d %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% 
  as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
  select(count = V1, everything()) %>% arrange(desc(count)) %>% 
  mutate(quote = count/length(d$id)) %>% filter(quote < 0.05)

d_filter_temp %<>% select(column_names)
d %<>% select(d_filter_temp$column_names)
# success: only 54 columns left!

# create new data frame for part 2 (classification)
d_na_less_5_percent <- d

# inspect remaining variables
plot_missing(d) # success!
introduce(d) %>%  t()
plot_intro(d)
plot_bar(d) # 8 columns with more than 50 categories;
# emp_title: 50492 categories
# last_pymnt_d: 133 categories
# title: 10437 categories
# earliest_cr_line: 625 categories
# last_credit_pull_d: 132 categories
# issue_d: 103 categories
# zip_code: 876 categories
# addr_state: 51 categories

# generate a list of unique counts per column
(d_unique_counts_per_column <- d %>% summarise_all(list(~n_distinct(.))) %>% t() %>% as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
    select(count = V1, everything()) %>% arrange(desc(count)))

# assumption: select most important columns according to subjective evaluation
## filter e. g. columns with just one unique value, not understandable meaning, 
## in general columns <= 10 distinct values, 

d %<>% select(int_rate, loan_amnt, revol_bal, revol_util, funded_amnt_inv, annual_inc, pub_rec_bankruptcies, term, pymnt_plan,
             initial_list_status, application_type, hardship_flag, debt_settlement_flag, 
             verification_status, home_ownership, acc_now_delinq, grade, 
             collections_12_mths_ex_med, loan_status, chargeoff_within_12_mths, 
             year_temp)


# write_csv(d, path = "data/data_group_3_selected_variables.csv")
# starting from here for regression task
d <- read_csv("data/data_group_3_selected_variables.csv")

ggpairs(d[, 1:3]) 
# ggpairs(d) # not really working for all data

# show scatterplots, distributions & correlations
set.seed(22)
d %>%
  sample_n(1000) %>% 
  select(c(1:7, 16, 18, 20, 21)) %>% 
  na.omit() %>% 
  chart.Correlation(histogram = TRUE, pch = 19) # better than cor() %>%corrplot()
  

### show patterns ###
plot_bar(d, with = "int_rate")
plot_boxplot(d, by = "int_rate")
plot_histogram(d)
plot_scatterplot(split_columns(d)$continuous, by = "int_rate", sampled_rows = 1000L)



# to do: variable casting to adequate data types
## example
d <- update_columns(d, c("term", "application_type", "home_ownership", "grade"), as.factor)




### Regression analysis ###

# points to consider:
## 1. transform categorical variables into dummy variables
## 2. ...

# validation set approach
# Approach: http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

# remove NONE values of attribute home_ownership
d %<>% filter(home_ownership != "NONE")

# Split the data into training and test set
set.seed(22)
training_samples <- d$int_rate %>%
  createDataPartition(p = 0.67, list = FALSE)
train_data  <- d[training_samples, ]
test_data <- d[-training_samples, ]


# Build the model first try
lm_model <- lm(int_rate~., data = train_data)
summary(lm_model)

# remove not significant predictors
train_data %>% select(-pymnt_plan, -collections_12_mths_ex_med)
test_data %<>% select(--pymnt_plan, -collections_12_mths_ex_med)

# second try
lm_model <- lm(int_rate~., data = train_data)
summary(lm_model)

# Make predictions and compute the R2, RMSE and MAE
lm_predictions <- lm_model %>% predict(test_data)
summary(lm_predictions)
na_filter <- lm_predictions[!is.na(lm_predictions)]
# show measures for model quality
(lm_evaluation_vsa <-
  data.frame(
    R2 = R2(lm_predictions[na_filter], test_data$int_rate[na_filter]),
    RMSE = RMSE(lm_predictions[na_filter], test_data$int_rate[na_filter]),
    MAE = MAE(lm_predictions[na_filter], test_data$int_rate[na_filter])
  ))
# todo: interpretation, R2 higher = better, RMSE lower = better, MAE lower = better

# generate prediction error rate
RMSE(lm_predictions[na_filter], test_data$int_rate[na_filter])/mean(test_data$int_rate[na_filter])
## todo: interpretation


# Leave one out cross validation - LOOCV
# Define training control
train_control <- trainControl(method = "LOOCV")
# Train the model
set.seed(22)
d_sample <- d %>% sample_n(2000) %>% na.omit()
# check if there is just one unique value per column
d_sample %>% summarise_all(list(~n_distinct(.))) %>% t() %>% as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
  select(count = V1, everything()) %>% arrange(count)
d_sample %<>% select(-c(pymnt_plan, hardship_flag))
lm_model_loocv <- train(int_rate ~ ., data = d_sample, method = "lm",
               trControl = train_control)
# Summarize the results
(lm_evaluation_loocv <- lm_model_loocv)
# todo: analyze warnings
# comparison to validation set approach: slightly worse at R2 and RMSE but better MAE






# todo: further regression models like ridge or lasso regression

# model selection including predictor reduction
# using three different methods to perform regression including
# cross-validation

# variable selection / feature selection
# "... start with approaches that conserve the original predictor space."
## One would use the shrinkeage method "ridge regression" in this case
## start using only numeric values --> todo: include factors as well
d_n <- dplyr::select_if(d, is.numeric)


# ## solution option 1 from lecturer failed ##
# options(na.action="na.action.default")
# options(na.action="na.pass")
# options(na.action="na.fail")
# predictors <- model.matrix(int_rate ~ ., d_n)[,c(-1)]
# outputs <- d_n$int_rate
# 
# # glmnet performas by default standardization of predictors
# m_ridge <- glmnet(predictors, outputs, alpha = 0)
# dim(coef(m_ridge))
# #this gives us 13 columns (12 predictors + 1 intercept) and 71 lines, one for each LAMBDA value analyzed
# m_ridge
# plot(m_ridge)

## try solution option https://www.r-bloggers.com/ridge-regression-and-the-lasso/
View(swiss) <- datasets::swiss
x <- model.matrix(Fertility~., swiss)[,-1]
y <- swiss$Fertility
lambda <- 10^seq(10, -2, length = 100)
#create test and training sets
library(glmnet)
set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
#ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, exact = T, type = 'coefficients')[1:6,]
swisslm <- lm(Fertility~., data = swiss, subset = train)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
#make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
s.pred <- predict(swisslm, newdata = swiss[test,])
#check MSE
mean((s.pred-ytest)^2)
mean((ridge.pred-ytest)^2)
#a look at the coefficients
out = glmnet(x[train,],y[train],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)[1:6,]

## lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)
lasso.coef  <- predict(lasso.mod, type = "coefficients", s = bestlam)[1:6,]





## hopefully better alternative to "ridge regression": lasso






## alternative to shrinkeage: perform forward stepwise selection











# model evaluation by using correlation matrix for the predictors and
# the output variable. Use further train and CV error rate as well as test error.


















########################################
#
# Conclusion Part 2 (classification):
# tbd, interpretation of results
# 
########################################

# load data
d2 <- d_na_less_5_percent




############################ Code Ramon ################################


d <- read_csv("data/data_group_3_selected_variables.csv")
# remove NONE values of attribute home_ownership
d %<>% filter(home_ownership != "NONE")

# Split the data into training and test set
set.seed(22)
training_samples <- d$int_rate %>%
  createDataPartition(p = 0.67, list = FALSE)
train_data  <- d[training_samples, ]
test_data <- d[-training_samples, ]


