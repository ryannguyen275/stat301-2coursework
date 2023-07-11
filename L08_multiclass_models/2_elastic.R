
library(tidymodels)
library(tidyverse)

load("results/info_elastic.rda")

tuned_elastic <- tune_grid(
  elastic_workflow,
  pokemon_folds, 
  elastic_grid
)

write_rds(tuned_elastic, file = "results/tuned_elastic.rds")
