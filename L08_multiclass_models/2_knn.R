
library(tidymodels)
library(tidyverse)

load("results/info_knn.rda")

tuned_knn <- tune_grid(
  knn_workflow,
  pokemon_folds, 
  knn_grid
)

write_rds(tuned_knn, file = "results/tuned_knn.rds")