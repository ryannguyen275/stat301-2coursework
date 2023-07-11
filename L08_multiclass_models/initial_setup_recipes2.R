

# Loading packages
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(skimr)
library(xgboost)
set.seed(25)

## READ IN DATA

pokemon <- read.csv("data/Pokemon.csv") %>% 
  janitor::clean_names() %>% 
  select(-c(x))

ggplot(pokemon, aes(x = type_1)) +
  geom_bar()

pokemon <- pokemon %>% 
  filter(type_1 %in% c("Bug", "Fire", "Grass", "Normal", "Water", "Psychic")) %>% 
  mutate(type_1 = factor(type_1)) %>% 
  mutate(type_2 = ifelse(is.na(type_2), "None", type_2)) %>% 
  step_rm(all_nominal_precictors()) %>% 
  select(-c(name, total, steps))

View(pokemon)

## SPLITTING THE DATA 
pokemon_split <- initial_split(pokemon, prop = 0.8, strata = type_1)

pokemon_train <- training(pokemon_split)

pokemon_test <- testing(pokemon_split)

pokemon_folds <- vfold_cv(pokemon_train, v = 3, repeats = 4, strata = type_1)

dim(pokemon_train)
dim(pokemon_test)

## CREATE RECIPE 1
pokemon_recipe1 <- recipe(type_1 ~., data = pokemon_train) %>% 
  step_rm(type_2) %>% 
  step_normalize(all_predictors()) %>% 
  step_zv(all_predictors())

# check recipe is working okay
prep(pokemon_recipe1) %>% 
  bake(new_data = NULL)

## CREATE RECIPE 2
pokemon_recipe2 <- recipe(type_1 ~., data = pokemon_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_zv(all_predictors())

# check recipe is working okay
prep(pokemon_recipe2) %>% 
  bake(new_data = NULL)

###################################################################

# Get a baseline null model
null_mod <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("classification")

null_workflow <- workflow() %>% 
  add_model(null_mod) %>% 
  add_recipe(pokemon_recipe1)

null_fit <- fit_resamples(null_workflow,
                          resamples = pokemon_folds,
                          control = control_resamples(save_pred = TRUE))

null_fit %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  mutate(wflow_id = "null")

######################################################################

## SET ENGINE

### Elastic net
elastic_spec <- multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

elastic_param <- extract_parameter_set_dials(elastic_spec)

elastic_grid <- grid_regular(elastic_param, levels = 5)

### Random forest
rf_spec <- rand_forest(min_n = tune(),
                       mtry = tune(),
                       trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

rf_param <- extract_parameter_set_dials(rf_spec) %>% 
  update(mtry = mtry(range = c(2,6)))

rf_grid <- grid_regular(rf_param, levels = 5)

### Boosted trees
bt_spec <- bt_model <- boost_tree(mode = "classification",
                                  min_n = tune(),
                                  mtry = tune(), 
                                  learn_rate = tune()) %>% 
  set_engine("xgboost", importance = "impurity")

bt_param <- extract_parameter_set_dials(bt_spec) %>% 
  update(mtry = mtry(range = c(2,6)),
         learn_rate = learn_rate(range = c(.3, .8),
                                 trans = scales::identity_trans()))

bt_grid <- grid_regular(bt_param, levels = 5)

### KNN
knn_spec <- nearest_neighbor(mode = "classification",
                             neighbors = tune()) %>% 
  set_engine("kknn")

knn_param <- extract_parameter_set_dials(knn_spec) %>% 
  update(neighbors = neighbors(range = c(1,30)))


knn_grid <- grid_regular(knn_param, levels = 5)

######################################################################
# workflow set

all_workflows <- workflow_set(
  preproc = list(recipe1 = pokemon_recipe1, recipe2 = pokemon_recipe2),
  models = list(en = elastic_spec, knn = knn_spec, rf = rf_spec, bt = bt_spec)
)

all_workflows

# next, add grids
all_workflows <- all_workflows %>% 
  option_add(grid = elastic_grid, id = c("recipe1_en", "recipe2_en")) %>% 
  option_add(grid = knn_grid, id = c("recipe1_knn", "recipe2_knn")) %>% 
  option_add(grid = rf_grid, id = c("recipe1_rf", "recipe2_rf")) %>% 
  option_add(grid = bt_grid, id = c("recipe1_bt", "recipe2_bt"))


all_tuned <- all_workflows %>% 
  workflow_map(
    resamples = pokemon_folds,
    verbose = TRUE, 
    control = control_grid(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  )

