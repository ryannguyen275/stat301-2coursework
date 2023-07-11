
library(tidymodels)
library(tidyverse)

load("results/info_rf.rda")

tuned_rf <- tune_grid(
  rf_workflow,
  pokemon_folds, 
  rf_grid
)

write_rds(tuned_rf, file = "results/tuned_rf.rds")
