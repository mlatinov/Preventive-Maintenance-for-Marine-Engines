
#### Libraries ####
library(tidymodels)
library(tidyverse)

# Load the data 
marine_data <- read.csv("marine_engine_data.csv",stringsAsFactors = TRUE)


#### Feature Engineering ####

# Split the data 
split_a <- initial_split(data = marine_data,prop = 0.8,strata = failure_mode)

# Define training and testing data 
training_data_a <- training(split_a)
testing_data_a <- testing(split_a)

# Make a coltrol recipe
recipe_control <-recipe(failure_mode ~ .,data = training_data_a)

## Recipe one Standart Features
recipe_standart <- recipe(failure_mode ~ .,data = training_data_a) %>%
  
  # Remove nzv Features
  step_nzv(all_nominal_predictors()) %>%
  
  # Apply Transformation Yeo-Jonhson
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Standartize the variables 
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  
  # Dim Reduction with PLS
  step_pls(all_of(c("running_period","fuel_consumption","engine_temp","coolant_temp","oil_pressure","engine_load")),
           outcome = "failure_mode",
           num_comp = tune(),
           predictor_prop = tune()) %>%

  # Dummy encode nominal predictors
  step_dummy(all_nominal_predictors())


##### Define models ####

## Random Forest

# Control Random Forest
control_rf <- rand_forest() %>%
  set_engine("ranger")%>%
  set_mode("classification")
  
# Tuned Random Forest
tuned_rf <- rand_forest(mtry = tune(),trees = 300,min_n = tune())%>%
  set_engine("ranger") %>%
  set_mode("classification")
  
## XGB 

# Control XGB 
control_xgb <- boost_tree()%>%
  set_engine("xgboost")%>%
  set_mode("classification")
  
# Tuned XGB
tuned_xgb <- boost_tree(
  mtry = tune(),
  min_n = tune(),
  trees = tune(),learn_rate = tune(),
  sample_size = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## SVM

# Control SVM radial
control_svm <- svm_rbf()%>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Tuned SVM Radial
tuned_svm <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune(),
  margin = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

## Tune the Standart Recipe 



# Create a workflow set to compare diffrent recipes
workflow_recipe <- workflow_set(
  preproc = list(simple = recipe_standart, control = control_recipe),
  models = list(c_rf = control_rf, c_xgb = control_xgb, c_svm = control_svm),
  cross = TRUE)

# Evaluate the performance for differnt recipes
recipe_result <- workflow_recipe %>%
  workflow_map("fit_resamples")

  
  
