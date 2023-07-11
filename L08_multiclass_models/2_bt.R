
library(tidymodels)
library(tidyverse)

load("results/info_bt.rda")

tuned_bt <- tune_grid(
  bt_workflow,
  pokemon_folds, 
  bt_grid
)

write_rds(tuned_bt, file = "results/tuned_bt.rds")
