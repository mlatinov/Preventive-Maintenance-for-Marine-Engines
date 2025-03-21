
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

# Make a control recipe
control_recipe <-recipe(failure_mode ~ .,data = training_data_a)%>%
  # Dummy encode nominal predictors to prevent errors
  step_dummy(all_nominal_predictors())
  
## Recipe one Standard Features
recipe_standard <- recipe(failure_mode ~ .,data = training_data_a) %>%
  
  # Remove nzv Features
  step_nzv(all_nominal_predictors()) %>%
  
  # Apply Transformation Yeo-Jonhson
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Standardize the variables 
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

## Tune the Standard Recipe

# Workflow_set
workflow_recipe_tune_standard <- 
  workflow_set(
    preproc = list(standard = recipe_standard),
    models = list(c_rf = control_rf, c_xgb = control_xgb, c_svm = control_svm))

# Params and Grid
resample_recipe <- vfold_cv(data = training_data_a,v = 10) 

# Grid recipe Standard LHC
tune_grid_recipe_standard <- 
  grid_space_filling(
    num_comp(c(1,10)),
    predictor_prop(c(0,1)))

# Tuning 
recipe_standard_tune <- 
  workflow_recipe_tune_standard %>%
  workflow_map(
    fn = "tune_grid",
    grid = tune_grid_recipe_standard,
    resamples = resample_recipe)

# Select best params
best_standard_params <- recipe_standard_tune %>%
  extract_workflow_set_result("standard_c_rf")%>%
  select_best(metric = "accuracy")

# Update the recipe standard
final_recipe_standard <- finalize_recipe(recipe_standard, best_standard_params)

# Create a workflow set to compare different recipes
workflow_recipe <- workflow_set(
  preproc = list(simple = final_recipe_standard, control = control_recipe),
  models = list(c_rf = control_rf, c_xgb = control_xgb, c_svm = control_svm),
  cross = TRUE)

# Evaluate the performance for different recipes
recipe_result <- workflow_recipe %>%
  workflow_map("fit_resamples",verbose = TRUE,resamples = resample_recipe)

# Collect the results
all_metrics <- recipe_result %>%
  select(wflow_id, result) %>%  # Keep workflow IDs and results
  mutate(metrics = map(result, collect_metrics)) %>%  # Extract metrics
  select(wflow_id, metrics) %>% 
  unnest(metrics) 

# Plot the results
all_metrics %>%
  filter(.metric != "brier_class") %>%
  mutate(
    wflow_id = fct_reorder(as.factor(wflow_id), mean, .fun = sum)  # Reorder by mean
  ) %>%
  ggplot(aes(x = mean, y = wflow_id, fill = .metric)) + 
  geom_col() +
  facet_wrap(~.metric, scales = "free_y") +
  scale_fill_manual(values = c("accuracy" = "green", "roc_auc" = "blue")) +
  theme_minimal() +
  coord_cartesian(xlim = c(0.01,0.6),ylim = c(6,1),expand = TRUE)+
  labs(
    title = "Compare Different Models with Different Recipes ",
    x = "Mean",
    y = "Models",
    subtitle = "cr = Control, simple = Standard",
    fill = "Metric"
  ) +
  theme(
    title = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 10)
  )




