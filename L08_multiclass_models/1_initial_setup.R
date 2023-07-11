
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
  mutate(type_1 = factor(type_1))


skim(pokemon)

pokemon <- pokemon %>% 
  mutate(legendary = factor(legendary)) %>% 
  select(-c(name, type_2, total))


## SPLITTING THE DATA 
pokemon_split <- initial_split(pokemon, prop = 0.8, strata = type_1)

pokemon_train <- training(pokemon_split)

pokemon_test <- testing(pokemon_split)

pokemon_folds <- vfold_cv(pokemon_train, v = 3, repeats = 4, strata = type_1)

dim(pokemon_train)
dim(pokemon_test)

## CREATE RECIPE
pokemon_recipe <- recipe(type_1 ~., data = pokemon_train) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_normalize(all_predictors()) %>% 
  step_zv(all_predictors())

# check recipe is working okay
prep(pokemon_recipe) %>% 
  bake(new_data = NULL)
  

## SET ENGINE

### Elastic net
elastic_spec <- multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

penalty()
mixture()

elastic_param <- extract_parameter_set_dials(elastic_spec)

elastic_workflow <- workflow() %>% 
  add_model(elastic_spec) %>% 
  add_recipe(pokemon_recipe)

elastic_grid <- grid_regular(elastic_param, levels = 5)

save(elastic_workflow, elastic_grid, pokemon_folds, file = "results/info_elastic.rda")


### Random forest
rf_spec <- rand_forest(min_n = tune(),
                       mtry = tune(),
                       trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

mtry()

rf_param <- extract_parameter_set_dials(rf_spec) %>% 
  update(mtry = mtry(range = c(2,6)))

rf_workflow <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(pokemon_recipe)

rf_grid <- grid_regular(rf_param, levels = 5)

save(rf_workflow, rf_grid, pokemon_folds, file = "results/info_rf.rda")

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

bt_workflow <- workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(pokemon_recipe)

bt_grid <- grid_regular(bt_param, levels = 5)

save(bt_workflow, bt_grid, pokemon_folds, file = "results/info_bt.rda")


### KNN
knn_spec <- nearest_neighbor(mode = "classification",
                              neighbors = tune()) %>% 
  set_engine("kknn")

knn_param <- extract_parameter_set_dials(knn_spec) %>% 
  update(neighbors = neighbors(range = c(1,30)))

knn_workflow <- workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe (pokemon_recipe)

knn_grid <- grid_regular(knn_param, levels = 5)

save(knn_workflow, knn_grid, pokemon_folds, file = "results/info_knn.rda")
