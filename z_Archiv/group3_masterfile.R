#! important !#
# check & use insights from forum:
# https://elearning.hslu.ch/ilias/ilias.php?ref_id=3996781&cmd=showThreads&cmdClass=ilrepositorygui&cmdNode=vs&baseClass=ilrepositorygui

library(tidyverse)
library(DataExplorer) # https://github.com/boxuancui/DataExplorer
library("glmnet")
library(magrittr)
library(DataExplorer)

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

# transform data frame into much nicher tibble format for working with
# huge datasets
d <- as_tibble(d)

# remove big data file for memory reasons
rm(data_temp)
rm(data_temp_2)
rm(dataset_3)

# write copy of group 3 data
path_file <- "./data/copy_data_group_3"
if (!file.exists(path_file)) {
  write_csv(d, path = "data/copy_data_group_3.csv")
}

######################## start from here ... ############################################################

# import already generated file for group 3
d <- read_csv("data/copy_data_group_3.csv") 
d <- as_tibble(d)


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

# create backup dataset
d_preclean <- d
d <- d_preclean

# get NAs count of all columns
d_missing_count <- d %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% 
  as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
  select(count = V1, everything()) %>% arrange(desc(count)) %>% 
  mutate(quote = count/length(d$id)) %>% filter(quote >= 0.5)

# remove columns with NA quoate >= 50%
d_filter_temp <- d %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% 
  as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
  select(count = V1, everything()) %>% arrange(desc(count)) %>% 
  mutate(quote = count/length(d$id)) %>% filter(quote < 0.5)

d_filter_temp %<>% select(column_names)

d %<>% select(d_filter_temp$column_names)

# to do variable casting to adequate data types
## to factor
update_columns(d, c("Month", "Day"), as.factor)


# removed 58 columns
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
plot_bar(d, with = "int_rate")
plot_boxplot(d)
plot_boxplot(d, by = "int_rate")
plot_scatterplot(split_columns(d)$continuous, by = "int_rate", sampled_rows = 1000L)
split_columns(d)$continuous
plot_correlation(d)

(d_unique_counts_per_column <- d %>% summarise_all(funs(n_distinct(.))) %>% t() %>% as.data.frame() %>% rownames_to_column("column_names") %>% as_tibble() %>% 
  select(count = V1, everything()) %>% arrange(desc(count)))

## check columns

# remove columns with just one value
d <- drop_columns(d, c("policy_code", "disbursement_method"))
d_just_chr_columns <- d[,sapply(d,is.character)]
## todo: check frequency of character variables & how to handle them


# points to consider:
## 1. transform categorical variables into dummy variables
## 2. ...

# validation set approach
summary(d$int_rate)
hist(d$int_rate)

# index between 1 & max_num
max_num <- length(d$int_rate)
set <- 1:max_num

#generate train(ing) set (2/3) and a val(idation) set (1/3) , without replacement (no repetitions)
set.seed(22)
head((d_val <- sample(set, max_num / 3)), 10)
head(d_train <- set[-d_val], 10)

# check if it worked correctly --> success
# head(sort(d_val), 10)


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