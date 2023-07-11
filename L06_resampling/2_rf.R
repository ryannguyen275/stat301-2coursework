
library(tidyverse)
library(tidymodels)

# load workflows, folds, controls
load("results/fit_rf.rda")


# fit folds
rf_fit_folds <- fit_resamples (rf_workflow,
                               resamples = kc_folds,
                               control = keep_pred)

write_rds(rf_fit_folds, file = "results/rf_fit_folds.rds")