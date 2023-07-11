# Loading Package & Setting Seed
library(tidymodels)
library(tidyverse)
library(parsnip)
library(rstanarm)
library(splines)
set.seed(1254)


# Loading in Data

kc_data <- read_csv("data/kc_house_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(waterfront = as.factor(waterfront),
         view = as.factor(view),
         condition = as.factor(condition),
         grade = as.factor(grade),
         log_price = log10(price))

## Exercise 2
### OLS
lm_spec <- linear_reg() %>% 
  set_engine("lm")

### Lasso
lasso_spec <- linear_reg(penalty = 0.01, mixture = 1) %>% 
  set_engine("glmnet")

### Ridge
ridge_spec <- linear_reg(penalty = 0.01, mixture = 0) %>% 
  set_engine("glmnet")

### Random forest spec
rf_spec <- rand_forest(trees = 500, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

## Exercise 3

## Task 1
kc_rec <- recipe(log_price ~ waterfront + sqft_living + yr_built, data = kc_training) %>% 
  step_dummy(all_nominal_predictors())

## Task 2
## OLS
kc_wkflow_lm <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(kc_rec)

## Lasso
kc_wkflow_lasso <- workflow() %>% 
  add_model(lasso_spec) %>% 
  add_recipe(kc_rec)

## Ridge
kc_wkflow_ridge <- workflow() %>% 
  add_model(ridge_spec) %>% 
  add_recipe(kc_rec)

## Random forest spec
kc_wkflow_rf <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(kc_rec)

## Task 3
# OLS
kc_fit_lm <- fit(kc_wkflow_lm, kc_training)
broom::tidy(kc_fit_lm)

# lasso
kc_fit_lasso <- fit(kc_wkflow_lasso, kc_training)
broom::tidy(kc_fit_lasso)

# ridge
kc_fit_ridge <- fit(kc_wkflow_ridge, kc_training)
broom::tidy(kc_fit_ridge)

# random forest
kc_fit_rf <- fit(kc_wkflow_rf, kc_training)

## Task 4
# OLS
kc_pred_lm <- kc_testing %>% 
  bind_cols(predict(kc_fit_lm, kc_testing)) %>% 
  select(price, log_price, .pred)

kc_pred_lm %>% 
  rmse(log_price, .pred)

# lasso
kc_pred_lasso <- kc_testing %>% 
  bind_cols(predict(kc_fit_lasso, kc_testing)) %>% 
  select(price, log_price, .pred)

kc_pred_lasso %>% 
  rmse(log_price, .pred)

# ridge
kc_pred_ridge <- kc_testing %>% 
  bind_cols(predict(kc_fit_ridge, kc_testing)) %>% 
  select(price, log_price, .pred)

kc_pred_ridge %>% 
  rmse(log_price, .pred)

# random forest 
kc_pred_rf <- kc_testing %>% 
  bind_cols(predict(kc_fit_rf, kc_testing)) %>% 
  select(price, log_price, .pred)

kc_pred_rf %>% 
  rmse(log_price, .pred)

## Exercise 4

### Task 1
# very funnel shaped
ggplot(kc_training, aes(x = sqft_lot, y = log_price)) + 
  geom_point()

# log is pretty good
ggplot(kc_training, aes(x = log(sqft_lot), y = log_price)) + 
  geom_point()

# sqrt not great
ggplot(kc_training, aes(x = sqrt(sqft_lot), y = log_price)) + 
  geom_point()

# visualize lat, clear curve
ggplot(kc_training, aes(x = (lat), y = log_price)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ ns(x, df = 5),
              se = FALSE)

### Task 2

# Define a recipe that uses `waterfront`, `sqft_living`, `sqft_lot`, `bedrooms`,
# `lat`, and `date` to predict the target/outcome variable. 
# 
# - Add a step to transform dummy variables
# - Add a step that does an appropriate transformation on `lat`
# - Add a step that does an appropriate transformation on `sqft_lot`
# - Add a step that does a date transformation that extracts the `features` of "month" 
# 
# If you complete only these tasks there will be a potential problem with our recipe. 
# What is the problem and how do we remedy it? You may need to look at the model fit to help see the issue.

recipe_2_kc <- recipe(log_price ~ waterfront + sqft_living + sqft_lot 
                      + bedrooms + lat + date, data = kc_training) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_ns(lat, deg_free = 5) %>% 
  step_log(sqft_lot) %>% 
  step_date(date, features = c("month")) %>% 
  update_role(date, new_role = "id")

wkflow_new <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(recipe_2_kc)

fit_new <- fit(wkflow_new, kc_training)

tidy(fit_new)

# step date keeps both date and month, so you're basically inputting it in twice. 
# date is pretty much an identifier, when it got sold.
# date is not actually contributing predictor, now we need to take the date out

