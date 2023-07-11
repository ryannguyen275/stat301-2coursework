# Loading packages
library(tidymodels)
library(tidyverse)
library(parsnip)
library(rstanarm)
library(splines)
set.seed(1254)

# Task 1
abalone_data <- read_csv("data/abalone.csv") %>% 
  janitor::clean_names() %>% 
  mutate(age = rings + 1.5)

ggplot(abalone_data, aes(x = age)) +
  geom_histogram()

# Task 2
abalone_split <- initial_split(abalone_data, prob = 0.8, strata = age)

abalone_train <- training(abalone_split)
abalone_test <- testing(abalone_split)

# Task 3
# Steps for your recipe:
#   
#   1.  dummy code any categorical predictors
# 
# 2.  create interactions between
# # 
# -   `type` and `shucked_weight`,
# -   `longest_shell` and `diameter`,
# -   `shucked_weight` and `shell_weight`
# 
# 3.  center all predictors, and
# 
# 4.  scale all predictors.

abalone_recipe <- recipe(age ~., data = abalone_train) %>% 
  # remove rings
  step_rm(rings) %>% 
  # dummy code
  step_dummy(all_nominal_predictors()) %>% 
  # interactions
  step_interact(~ type:shucked_weight,
                ~ longest_shell:diameter,
                ~ shucked_weight:shell_weight) %>% 
  # center and scale all predictors
  step_normalize(all_numeric_predictors())


# Task 4

# define model type
lm_spec <- linear_reg() %>% 
  set_engine("lm")

# create workflow
abalone_wkflow_lm <- workflow() %>% 
  # add model
  add_model(lm_spec) %>% 
  # add recipe
  add_recipe(abalone_recipe)

# Task 5
abalone_fit_lm <- fit(abalone_wkflow_lm, abalone_train)

broom::tidy(abalone_fit_lm)


# Task 6

# create metric set
abalone_metrics <- metric_set(rmse, rsq, mae)

# create tibble of predicted values
abalone_test_res <- abalone_test %>% 
  bind_cols(predict(abalone_fit_lm, abalone_test)) %>% 
  select(age, .pred)

# apply metric set to tibble 
abalone_metrics(abalone_test_res, truth = age, estimate = .pred)

# create a plot
ggplot(abalone_test_res, aes(x = age, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Age", x = "Actual Age") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()


# Task 7

# define random forest model
# don't worry about hyperparameters (mtry and trees) -- we will cover later
rf_model <- 
  rand_forest(mtry = 6, trees = 500) %>%
  set_engine("ranger") %>% 
  set_mode("regression")

# define workflow 
abalone_wkflow_rf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(abalone_recipe)

# fit workflow
abalone_fit_rf <- fit(abalone_wkflow_rf, abalone_train)

# assess performance
abalone_metrics_rf <- metric_set(rmse, rsq, mae)
abalone_test_rf <- abalone_test %>% 
  bind_cols(predict(abalone_fit_rf, abalone_test)) %>% 
  select(age, .pred)
abalone_metrics_rf(abalone_test_rf, truth = age, estimate = .pred)



# define lasso model
# don't worry about penalty -- we will cover later
# mixture = 1 specifies lasso; mixture = 0 for ridge
lasso_model <- 
  linear_reg(penalty = 0.001, mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# define workflow 
abalone_wkflow_lasso <- workflow() %>% 
  add_model(lasso_model) %>% 
  add_recipe(abalone_recipe)

# fit workflow
abalone_fit_lasso <- fit(abalone_wkflow_lasso, abalone_train)

# assess performance
abalone_metrics_lasso <- metric_set(rmse, rsq, mae)
abalone_test_lasso <- abalone_test %>% 
  bind_cols(predict(abalone_fit_lasso, abalone_test)) %>% 
  select(age, .pred)
abalone_metrics_lasso(abalone_test_lasso, truth = age, estimate = .pred)


# define ridge model
# don't worry about penalty -- we will cover later
# mixture = 0 specifies ridge; mixture = 1 for lasso
ridge_model <- 
  linear_reg(penalty = 0.001, mixture = 0) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# define workflow 
abalone_wkflow_ridge <- workflow() %>% 
  add_model(ridge_model) %>% 
  add_recipe(abalone_recipe)

# fit workflow
abalone_fit_ridge <- fit(abalone_wkflow_ridge, abalone_train)

# assess performance
abalone_metrics_ridge <- metric_set(rmse, rsq, mae)
abalone_test_ridge <- abalone_test %>% 
  bind_cols(predict(abalone_fit_ridge, abalone_test)) %>% 
  select(age, .pred)
abalone_metrics_ridge(abalone_test_ridge, truth = age, estimate = .pred)


### Exercise 2

# Task 1
titanic_data <- read_csv("data/titanic.csv") %>% 
  janitor::clean_names() %>% 
  mutate(survived = factor(survived, levels = c("Yes", "No")),
         pclass = factor(pclass))

# Task 2

ggplot(titanic_data, aes(x = survived)) +
  geom_histogram(stat = "count") +
  labs()


# Task 3

titanic_split <- initial_split(titanic_data, prob = 0.8, strata = survived)

titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)

titanic_train %>% 
  skim_without_charts()

View(titanic_test)


# Task 4
titanic_recipe_log <- recipe(survived ~ pclass + sex + age + sib_sp + parch + fare, data = titanic_train) %>% 
  # imputation step 
  step_impute_linear(age, impute_with = imp_vars(pclass, age, sib_sp, parch, fare)) %>% 
  # dummy encode categorical predictors
  step_dummy(all_nominal_predictors()) %>% 
  # interactions
  step_interact(~ starts_with("sex"):fare) %>% 
  step_interact(~ starts_with("age"):fare)


titanic_recipe_rf <- recipe(survived ~ pclass + sex + age + sib_sp + parch + fare, data = titanic_train) %>% 
  # imputation step 
  step_impute_linear(age, impute_with = imp_vars(pclass, age, sib_sp, parch, fare)) %>% 
  # one-hot encode
  step_dummy(one_hot = TRUE)

# Task 5
logistic_mod <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

titanic_wkflow_log <- workflow() %>% 
  add_model(logistic_mod) %>% 
  add_recipe(titanic_recipe_log)

titanic_fit_log <- fit(titanic_wkflow_log, titanic_train)



# Task 6
rf_mod <- rand_forest() %>%
  set_engine("ranger") %>% 
  set_mode("classification")

titanic_wkflow_rf <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(titanic_recipe_rf)

titanic_fit_rf <- fit(titanic_wkflow_rf, titanic_train)

# Task 7

rf_mod_cust <- rand_forest(
  mtry = 4, 
  trees = 600,
  min_n = 12
) %>%
  set_engine("ranger") %>% 
  set_mode("classification")

titanic_wkflow_rf_cust <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(titanic_recipe_rf)

titanic_fit_rf_cust <- fit(titanic_wkflow_rf_cust, titanic_train)

# Task 8
titanic_test_log <- titanic_test %>% 
  select(survived) %>% 
  bind_cols(predict(titanic_fit_log, titanic_test, type = "class")) %>% 
  bind_cols(predict(titanic_fit_log, titanic_test, type = "prob"))

titanic_test_rf <- titanic_test %>% 
  select(survived) %>% 
  bind_cols(predict(titanic_fit_rf, titanic_test, type = "class")) %>% 
  bind_cols(predict(titanic_fit_rf, titanic_test, type = "prob"))

titanic_test_rf_cust <- titanic_test %>% 
  select(survived) %>% 
  bind_cols(predict(titanic_fit_rf_cust, titanic_test, type = "class")) %>% 
  bind_cols(predict(titanic_fit_rf_cust, titanic_test, type = "prob"))

accuracy(titanic_test_log, survived, .pred_class)
accuracy(titanic_test_rf, survived, .pred_class)
accuracy(titanic_test_rf_cust, survived, .pred_class)


# Task 9
conf_mat(titanic_test_rf_cust, survived, .pred_class)


# Task 10
titanic_test_rf_cust_tib <- titanic_test %>% 
  select(survived) %>% 
  bind_cols(predict(titanic_fit_rf_cust, titanic_test, type = "prob"))

titanic_test_rf_cust_tib

# Task 11
titanic_curve <- roc_curve(titanic_test_rf_cust, survived, .pred_Yes)

autoplot(titanic_curve)
