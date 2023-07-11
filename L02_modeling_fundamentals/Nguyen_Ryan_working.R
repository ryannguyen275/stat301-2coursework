### loading data sets

library(tidyverse)

auto <- read_csv("data/Auto.csv") %>% 
  janitor::clean_names()

college <- read_csv("data/College.csv")%>% 
  janitor::clean_names()

# cleaning up auto
auto_clean <- auto %>% 
  mutate(horsepower = as.numeric(horsepower), 
         origin = as_factor(origin),
         cylinders = as_factor(cylinders))
# saving auto as an rds
write_rds(auto_clean, "data/processed/auto_clean.rds")

college_clean <- college %>% 
  janitor::clean_names() %>% 
  mutate(private = as_factor(private))

write_rds(college_clean, "data/processed/college_clean.rds")

### Exercise 1
model_1 <- lm(grad_rate ~ accept + outstate + private, college_clean)
summary(model_1)



### Exercise 2

model_2 <- lm(grad_rate ~ accept * outstate * private, college_clean)
summary(model_2)

### Exercise 7

plot(model_6, which = 1)

plot(model_6, which = 2)

model_7 <-lm(sqrt(mpg) ~ horsepower + displacement + weight, auto_training)
plot(model_7, which = 1)
plot(model_7, which = 2)

### Exercies 8
modelr:: rmse(model_6, auto_testing)

pred_log <- (predict(model_7, auto_testing))^(2)
error <- auto_testing$mpg - pred_log
sqrt(mean(error^2))

### Exercise 9

model_9 <- lm(log(mpg) ~ horsepower + displacement + weight, auto_training)
plot(model_9, which = 1)
plot(model_9, which = 2)
pred_9 <- exp(predict(model_9, auto_testing))
error_9 <- auto_testing$mpg - pred_9
sqrt(mean(error_9^2))


model_10 <- lm(log(mpg) ~ horsepower * displacement * weight, auto_training)
plot(model_10, which = 1)
plot(model_10, which = 2)
pred_log_10 <- exp(predict(model_10, auto_testing))
error_10 <- auto_testing$mpg - pred_log_10
sqrt(mean(error_10^2))