library(tidymodels)
library(tidyverse)


set.seed(25)

bt_tuned <- read_rds("results/bt_tuned.rds")

rf_tuned <- read_rds("results/rf_tuned.rds")

knn_tuned <- read_rds("results/knn_tuned.rds")

load("results/carseats_split.rda")

load("results/info_bt.rda")

load("results/info_knn.rda")

# random forest 
autoplot(rf_tuned, metric = "rmse")

best_rf <- show_best(rf_tuned, metric = "rmse")[1,]
# best is mtry = 14, min_n = 2

# k nearest neighbor 
autoplot(knn_tuned, metric = "rmse")

best_knn <-show_best(knn_tuned, metric = "rmse")[1,]
# best had neighbors = 15

# boosted tree 
autoplot(bt_tuned, metric = "rmse")

best_bt <-show_best(bt_tuned, metric = "rmse")[1,]
# mtry = 10, min_n = 30, learn_rate = .631 


best_rmse <- tibble(model = c("BT", "KNN", "RF"),
                    RMSE = c(best_bt$mean, best_knn$mean, best_rf$mean),
                    se = c(best_bt$std_err,best_knn$std_err, best_rf$std_err))
best_rmse
# bt won, rmse = 1.49, se = .0258, learn_rate = .631 

ggplot(best_rmse, aes(x = model, y = RMSE)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = RMSE - 1.96*se,
                    ymax = RMSE + 1.96*se), width = 0.2) + 
  theme_minimal()

#####finalize workflow for winning model
bt_workflow <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tuned, metric = "rmse"))

# fit whole training set to workflow 
fit_final <- fit(bt_workflow, carseats_train)

carseats_metric <- metric_set(rmse, rsq)

carseat_pred <- predict(fit_final, carseats_test) %>% 
  bind_cols(carseats_test %>% select(sales))

carseat_pred %>% 
  carseats_metric(truth = sales, estimate = .pred)


# visualize the results 
ggplot(carseat_pred, aes(x = sales, y = .pred)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

# visualize residuals 
carseat_pred <- carseat_pred %>% 
  mutate(residuals = sales - .pred)

ggplot(carseat_pred, aes(x = .pred, y = residuals)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 0)
