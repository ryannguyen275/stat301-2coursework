
# load packages
library(tidyverse)
library(tidymodels)
library(kknn)
library(vip)

set.seed(25)

# load workflows, folds, controls
load("results/info_bt.rda")

# tuning
bt_tuned <- bt_workflow %>% 
  tune_grid(carseats_folds, grid = bt_grid)

write_rds(bt_tuned, 
          file = "results/bt_tuned.rds")

