
# load libraries
library(tidymodels)
library(tidyverse)

# read in our results
lm_fit_folds <- read_rds("results/lm_fit_folds.rds")
lasso_fit_folds <- read_rds("results/lasso_fit_folds.rds")
ridge_fit_folds <- read_rds("results/ridge_fit_folds.rds")
rf_fit_folds <- read_rds("results/rf_fit_folds.rds")

lm_fit_folds %>% 
  collect_metrics() %>% 
  mutate(model = "lm") %>% 
  bind_rows(lasso_fit_folds %>% 
              collect_metrics() %>% 
              mutate(model = "lasso")) %>% 
  bind_rows(ridge_fit_folds %>% 
              collect_metrics() %>% 
              mutate(model = "ridge")) %>% 
  bind_rows(rf_fit_folds %>% 
              collect_metrics() %>% 
              mutate(model = "rf"))

# rf wins since it has the lowest RMSE and highest RSQ. 
# it's RMSE has the second lowest standard error out of all RMSE
# its RSQ has lowest standard error out of all RSQ