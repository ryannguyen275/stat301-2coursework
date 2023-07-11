
library(tidyverse)
library(tidymodels)

load("results/fit_lm.rda")

# fit lm folds
lm_fit_folds <- fit_resamples(lm_workflow,
                              resamples = kc_folds,
                              control = keep_pred)

write_rds(lm_fit_folds, file = "results/lm_fit_folds.rds")

# fit lasso folds
lasso_fit_folds <- fit_resamples(lasso_workflow,
                                 resamples = kc_folds,
                                 control = keep_pred)

write_rds(lasso_fit_folds, file = "results/lasso_fit_folds.rds")

# fit ridge folds
ridge_fit_folds <- fit_resamples(ridge_workflow,
                                 resamples = kc_folds,
                                 control = keep_pred)

write_rds(ridge_fit_folds, file = "results/ridge_fit_folds.rds")

