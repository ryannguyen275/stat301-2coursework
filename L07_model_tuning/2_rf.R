
# load packages
library(tidyverse)
library(tidymodels)
library(kknn)
library(vip)

set.seed(25)

# load workflows, folds, controls
load("results/info_rf.rda")

# tuning
rf_tuned <- rf_workflow %>% 
  tune_grid(carseats_folds, grid = rf_grid)

write_rds(rf_tuned, 
          file = "results/rf_tuned.rds")

