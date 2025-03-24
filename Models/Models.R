
#### Libraries ####
library(tidymodels)
library(tidyverse)
library(baguette)
library(themis)
library(pROC)

tidymodels_prefer()
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
  step_dummy(all_nominal_predictors()) %>%
  # SMOTE
  step_smote(failure_mode,over_ratio = 1)
  
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
  step_dummy(all_nominal_predictors()) %>%

## Recipe two New Features
recipe_new_features <- recipe(failure_mode ~ .,data = training_data_a)%>%
     
     # Step mutate adding new features
    step_mutate(
         temp_diff = exhaust_temp - coolant_temp,
         fuel_efficiency = fuel_consumption / rpm,
         vibration_ratio = vibration_level / engine_load
         ) %>%
  
  #Remove original features that were used to create the new ones
  step_rm(exhaust_temp, coolant_temp, fuel_consumption, rpm, vibration_level, engine_load) %>%
   
   # Remove nzv Features
  step_nzv(all_nominal_predictors()) %>%
     
   # Apply Transformation Yeo-Jonhson
  step_YeoJohnson(all_numeric_predictors()) %>%
    
  # Standardize the variables 
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
     
     # Dim Reduction with PLS
  step_pls(
    all_numeric_predictors(),
    outcome = "failure_mode",
    num_comp = tune(),
    predictor_prop = tune()) %>%
    
  # Dummy encode nominal predictors
  step_dummy(all_nominal_predictors()) %>%
  
  # SMOTE
  step_smote(failure_mode,over_ratio = 1)

##### Define models ####

## Random Forest

# Control Random Forest
control_rf <- rand_forest() %>%
  set_engine("ranger")%>%
  set_mode("classification")
  
## XGB 

# Control XGB 
control_xgb <- boost_tree()%>%
  set_engine("xgboost")%>%
  set_mode("classification")

## SVM

# Control SVM radial
control_svm <- svm_rbf()%>%
  set_engine("kernlab") %>%
  set_mode("classification")

## MLP

# Multiple Layer Neural Network Model
slnn <- mlp() %>%
  set_engine("nnet")%>%
  set_mode("classification")

## BNN

# Bagged Neural Network Model Specification
bnn <- bag_mlp()%>%
  set_engine("nnet")%>%
  set_mode("classification")

# Null Model 
null_model <- null_model()%>%
  set_engine("parsnip")%>%
  set_mode("classification")

## Tune the Standard Recipe

# Workflow_set
workflow_recipe_tune_standard <- 
  workflow_set(
    preproc = list(standard = recipe_standard),
    models = list(c_rf = control_rf, c_xgb = control_xgb, c_svm = control_svm,c_slnn = slnn,bnn = bnn,null_model=null_model))


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

# Create a workflow_set for new features recipe 
workflow_recipe_tune_newfeatures <-
  workflow_set(
    preproc = list(recipe = recipe_new_features),
    models = list(c_rf = control_rf, c_xgb = control_xgb, c_svm = control_svm,c_slnn = slnn,bnn = bnn,null_model=null_model))

# Tune grid new features recipe
tune_grid_recipe_new_features <-
  grid_space_filling(
    num_comp(c(1,10)),
    predictor_prop(c(0,1)))

# Tune new features recipe
recipe_new_features_tune <-
  workflow_recipe_tune_newfeatures %>%
  workflow_map(
    fn = "tune_grid",
    resamples = resample_recipe,
    grid = tune_grid_recipe_new_features,
    verbose = TRUE)

# Select Best params
best_new_features_params <-
  recipe_new_features_tune %>%
  extract_workflow_set_result("recipe_bnn")%>%
  select_best(metric = "accuracy")

# Update 
finalize_new_feature_recipe <- finalize_recipe(recipe_new_features,best_new_features_params)

# Create a workflow set to compare different recipes
workflow_recipe <- workflow_set(
  preproc = list(simple = final_recipe_standard, control = control_recipe,new_f_recipe = finalize_new_feature_recipe),
  models = list(c_rf = control_rf, c_xgb = control_xgb, c_svm = control_svm,slnn = slnn,bnn = bnn,null_model = null_model),
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
  coord_cartesian(xlim = c(0.01,0.6),ylim = c(18,1),expand = TRUE)+
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

#### Model Tuning ####

# Resample
resample_models <- vfold_cv(data = training_data_a,strata = "failure_mode",v = 5 )

# Metric
class_and_probs_metrics <- metric_set(roc_auc, pr_auc, accuracy)

## Multiple Layer Neural Network Model
mlp_tuned <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune())%>%
  set_engine("brulee") %>%
  set_mode("classification")

# Workflow 
mlp_workflow <- 
  workflow() %>%
  add_recipe(final_recipe_standard) %>%
  add_model(mlp_tuned)

# MLP LHC Grid
mlp_tune_grid <- 
  grid_space_filling(
    hidden_units(range = c(1,10)),
    penalty(range = c(-10,0)),
    epochs(range = c(10L,100L)))

# Tune
mlp_tuning <- 
  mlp_workflow %>%
  tune_grid(
    resamples = resample_models,
    grid = mlp_tune_grid,
    metrics = class_and_probs_metrics)

# Extract the best performance
best_mlp <- mlp_tuning %>% select_best(metric = "roc_auc")

# Finalize the workflow
mlp_tuned <- finalize_workflow(mlp_workflow,parameters = best_mlp)

## Bagged Neural Network Model 
bnn_tuned <- bag_mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune())%>%
  set_engine("nnet") %>%
  set_mode("classification")

# BNN  LHC Grid
bnn_tune_grid <- 
  grid_space_filling(
    hidden_units(c(1L,10L)),
    penalty(c(-10,0)),
    epochs(c(10L,100L)))

# BNN Workflow 
bnn_workflow <- 
  workflow() %>%
  add_model(bnn_tuned) %>%
  add_recipe(control_recipe)

# Tune BNN
bnn_tunning <- 
  bnn_workflow %>%
  tune_grid(
    resamples = resample_models,
    metrics = class_and_probs_metrics,
    grid = bnn_tune_grid )

# Extract the best result
bnn_best <- bnn_tunning %>% select_best(metric = "roc_auc")

# Finalize the workflow
bnn_tuned <- finalize_workflow(bnn_workflow,parameters = bnn_best)


## XGB 
tuned_xgb <- boost_tree(
  mtry = tune(),
  min_n = tune(),
  trees = tune(),
  learn_rate = tune(),
  sample_size = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# XGB  LHC Grid
xgb_tune_grid <- 
  grid_space_filling(
    mtry(c(1,15)),
    min_n(c(5,40)),
    trees(c(1000,2000)),
    learn_rate(c(-10,-1)),
    sample_size(c(0,1)))

# XGB Workflow
xgb_workflow <- 
  workflow() %>%
  add_model(tuned_xgb)%>%
  add_recipe(final_recipe_standard)

# Tune XGB 
xgb_tuning <- 
  xgb_workflow %>%
  tune_grid(
    resamples = resample_models,
    grid = xgb_tune_grid,
    metrics =class_and_probs_metrics)

# Extract best result
xgb_best <- xgb_tuning %>% select_best(metric = "roc_auc")

# Finalize the workflow
xgb_tuned <- finalize_workflow(x = xgb_workflow,parameters = xgb_best)
  
## Results 

model_tune_f <- function(tuning, model_name, metric_1) {
  
  # Extract the metrics data
  collect <- tuning %>%
    collect_metrics() %>%
    mutate(model = model_name)  
  
  # Extract the best result 
  best <- tuning %>% select_best(metric = {{metric_1}})
  config <- best$.config
  
  # Extract the final result 
  results <- collect %>%
    filter(.config == config)
   
  return(results)
}
# Collect the results
mlp_results <- model_tune_f(tuning = mlp_tuning,model_name = "MLP",metric = "accuracy")
bnn_results <- model_tune_f(tuning = bnn_tunning,model_name = "BNN",metric = "accuracy")
xgb_results <- model_tune_f(tuning = xgb_tuning,model_name = "XGB",metric = "accuracy")

# Combine the results
results <- bind_rows(mlp_results,bnn_results,xgb_results)

# Plot the results
ggplot(data = results,aes(x = fct_reorder(as.factor(model),.fun = sum,.x = mean ),y = mean,fill = model))+
  geom_col()+
  facet_wrap(~.metric)+
  theme_minimal()+
  scale_fill_manual(values = c("XGB" = "red","BNN" = "yellow","MLP" = "black"))+
  labs(
    title = "Tuned Models Comparison",
    x = "Models",
    y = "Mean",
    fill = "Models"
  )+
  theme(
    title = element_text(size = 12,face = "bold"),
    strip.text = element_text(size = 10,face = "italic")
  )

#### Evaluate Models ####

## Evaluation Function ##
model_eval <- function(training_data, testing_data, tuned_model_workflow){
  
  # Fit the final model
  model_fit <- fit(tuned_model_workflow, training_data)
  
  # Make predictions
  model_prediction <- predict(model_fit, testing_data, type = "prob") %>%
    bind_cols(predict(model_fit, testing_data, type = "class")) %>%
    bind_cols(testing_data)
  
  # Ensure `failure_mode` is a factor
  model_prediction$failure_mode <- as.factor(model_prediction$failure_mode)
  
  # Get the exact class levels
  class_levels <- levels(model_prediction$failure_mode)
  
  # Select only the probability columns that match the class levels
  prob_cols <- paste0(".pred_", class_levels)
  
  # Ensure that `prob_cols` exists in `model_prediction`
  prob_cols <- intersect(prob_cols, colnames(model_prediction))
  
  # Compute Multi-Class ROC AUC
  auc <- roc_auc(data = model_prediction, truth = failure_mode, 
                 all_of(prob_cols), estimator = "macro")$.estimate
  
  # Compute Multi-Class PR AUC
  pr_auc <- pr_auc(data = model_prediction, truth = failure_mode, 
                   all_of(prob_cols), estimator = "macro")$.estimate
  
  # Compute Accuracy
  acc <- accuracy(data = model_prediction, truth = failure_mode, 
                  estimate = .pred_class)$.estimate
  
  # Generate a Confusion Matrix
  conf_matrix <- conf_mat(data = model_prediction, truth = failure_mode, 
                          estimate = .pred_class)
  
  # Plot the Confusion Matrix
  conf_plot <- conf_matrix %>%
    autoplot(type = "heatmap") + ggtitle("Confusion Matrix")
  
  # Plot ROC curves for all classes
  roc_curve <- roc_curve(data = model_prediction, truth = failure_mode, 
                         all_of(prob_cols)) %>%
    autoplot() + ggtitle("ROC Curves for All Classes")
  
  # Return results
  return(list(
    auc = auc,
    pr_auc = pr_auc,
    accuracy = acc,
    confusion_matrix = conf_plot,
    roc_curve = roc_curve
  ))
}

## Evaluate models

# MLP 
model_eval(training_data = training_data_a,testing_data = testing_data_a,tuned_model_workflow = mlp_tuned)

# BNN
model_eval(training_data = training_data_a,testing_data = testing_data_a,tuned_model_workflow = bnn_tuned)

# XGB
model_eval(training_data = training_data_a,testing_data = testing_data_a,tuned_model_workflow = xgb_tuned)














