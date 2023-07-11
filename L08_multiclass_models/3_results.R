
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(skimr)
library(vip)
set.seed(25)

# read in the tuned data
tuned_elastic <- read_rds("results/tuned_elastic.rds")
tuned_rf <- read_rds("results/tuned_rf.rds")
tuned_bt <- read_rds("results/tuned_bt.rds")
tuned_knn <- read_rds("results/tuned_knn.rds")

# autoplot all of the tuned data
autoplot(tuned_elastic)
autoplot(tuned_rf)
autoplot(tuned_bt)
autoplot(tuned_knn)

# find the best of each
best_elastic <- show_best(tuned_elastic, metric = "roc_auc")[1,]
best_rf <- show_best(tuned_rf, metric = "roc_auc")[1,]
best_bt <- show_best(tuned_bt, metric = "roc_auc")[1,]
best_knn <- show_best(tuned_knn, metric = "roc_auc")[1,]

# print the best of each 
best_elastic
best_rf
best_bt
best_knn

# create and print a tibble with the best of each model
best_rmse <- tibble(model = c("ELASTIC", "RF", "BT", "KNN"),
                    roc_auc = c(best_elastic$mean, best_rf$mean, best_bt$mean, best_knn$mean),
                    se = c(best_elastic$std_err, best_rf$std_err, best_bt$std_err, best_knn$std_err))
best_rmse

ggplot(best_rmse, aes(x = model, y = roc_auc)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = roc_auc - 1.96*se,
                    ymax = roc_auc + 1.96*se), width = 0.2) + 
  theme_minimal()
