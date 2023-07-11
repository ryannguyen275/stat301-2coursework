
# load packages
library(tidyverse)
library(tidymodels)
library(kknn)
library(vip)

set.seed(25)

# load workflows, folds, controls
load("results/info_knn.rda")

# tuning
knn_tuned <- knn_workflow %>% 
  tune_grid(carseats_folds, grid = knn_grid)

write_rds(knn_tuned, 
          file = "results/knn_tuned.rds")

# visualize results
autoplot(knn_tuned)
