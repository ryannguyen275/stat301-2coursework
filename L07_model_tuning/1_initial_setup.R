
# load packages
library(tidyverse)
library(tidymodels)
library(janitor)
library(kableExtra)
library(xgboost)
# library(vip)

set.seed(25)

# Task 1
carseats <- read_csv("data/carseats.csv") %>% 
  janitor::clean_names()

ggplot(carseats, aes(x = sales)) + 
  geom_histogram(color = "white", bins = 20)

ggplot(carseats, aes(x = shelve_loc)) +
  geom_bar()

# Task 2

skimr::skim_without_charts(carseats)

# Task 3

## Split the data! Use stratified sampling.10 folds. 5 repeats.
carseats_split <- initial_split(carseats, 
                                prop = 0.8, 
                                strata = sales)

carseats_train <- training(carseats_split)

carseats_test <- testing(carseats_split)

carseats_folds <- vfold_cv(carseats_train, v = 10, repeats = 5,
                     strata = sales)

## Save out training/testing data for when ready for final results
save(carseats_train, carseats_test, file = "results/carseats_split.rda")

# Task 4
## recipe: one hot encode categorical, center and scale all predictors
carseats_recipe <- recipe(sales ~., data = carseats_train) %>% 
  # one-hot encode
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  # center all predictors
  step_normalize(all_predictors())

## without one hot encoding, one variable gets taken into the intercept

## prep and bake
carseats_recipe %>% 
  # sets recipe up on training set to get info
  prep() %>% 
  bake(new_data = NULL) %>% 
  view()


# Task 5
## rf model
rf_model <- rand_forest(mode = "regression",
                        min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger")

## boosted tree
bt_model <- boost_tree(mode = "regression",
                       min_n = tune(),
                       mtry = tune(), 
                       learn_rate = tune()) %>% 
  set_engine("xgboost")

## nearest neighbor
knn_model <- nearest_neighbor(mode = "regression",
                              neighbors = tune()) %>% 
  set_engine("kknn")

### about the parameter
neighbors()

# Task 6
## create our grid

### rf
rf_params <- parameters(rf_model) %>% 
  # N := maximum number of random predictor columns we want to try 
  # should be less than the number of available columns
  update(mtry = mtry(c(1, 10))) 

rf_grid <- grid_regular(rf_params, levels = 5)

### boosted tree
bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(c(1, 10)),
         learn_rate = learn_rate(range = c(-5, -0.2)))

bt_grid <- grid_regular(bt_params, levels = 5)

### knn
knn_params <- parameters(knn_model)

knn_grid <- grid_regular(knn_params, levels = 5)

# Task 8: create workflow

## rf 
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(carseats_recipe)

### save necessary parts to run in separate script
save(rf_workflow, rf_grid, carseats_folds, 
     file = "results/info_rf.rda")

## bt
bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(carseats_recipe)

### save necessary parts to run in separate script
save(bt_workflow, bt_grid, carseats_folds,
     file = "results/info_bt.rda")

## knn
knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(carseats_recipe)

### save necessary parts to run in separate script
save(knn_workflow, knn_grid, carseats_folds,
     file = "results/info_knn.rda")
