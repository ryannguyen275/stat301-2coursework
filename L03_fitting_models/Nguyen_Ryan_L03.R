library(tidymodels)
library(tidyverse)
library(parsnip)

set.seed(1254)

kc_data <- read_csv("data/kc_house_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(waterfront = as.factor(waterfront),
         view = as.factor(view),
         condition = as.factor(condition),
         grade = as.factor(grade),
         price_log = log(price))


### Exercise 1 
ggplot(kc_data, aes(x = price)) + 
  geom_histogram()

### Exercise 3
### OLS
lm_mod <- linear_reg() %>% 
  set_engine("lm")

OLS_3 <- lm_mod %>%
  fit(price_log ~ waterfront + sqft_living + yr_built + bedrooms , data = kc_training)

### Lasso
lasso_mod <- linear_reg(penalty = 0.01, mixture = 1) %>% 
  set_engine("glmnet")

lasso_3 <- lasso_mod %>% 
  fit(price_log ~ waterfront + sqft_living + yr_built + bedrooms , data = kc_training)


### Ridge
ridge_mod <- linear_reg(penalty = 0.01, mixture = 0) %>% 
  set_engine("glmnet")

ridge_3 <- ridge_mod %>% 
  fit(price_log ~ waterfront + sqft_living + yr_built + bedrooms , data = kc_training)

### Exercise 5
kc_testing %>% 
  select(price_log) %>% 
  bind_cols(predict(OLS_3, kc_testing)) %>% 
  # add 95% prediction intervals to results
  bind_cols(predict(OLS_3, kc_testing, type = "pred_int")) 

kc_testing %>% 
  select(price_log) %>% 
  bind_cols(predict(lasso_3, kc_testing))

kc_testing %>% 
  select(price_log) %>% 
  bind_cols(predict(ridge_3, kc_testing))

### Exercise 6
### OLS
OLS_6 <- lm_mod %>%
  fit(price ~ waterfront + sqft_living + yr_built + bedrooms , data = kc_training)

### Lasso
lasso_6 <- lasso_mod %>% 
  fit(price ~ waterfront + sqft_living + yr_built + bedrooms , data = kc_training)

### Ridge
ridge_6 <- ridge_mod %>% 
  fit(price ~ waterfront + sqft_living + yr_built + bedrooms , data = kc_training)

kc_testing %>% 
  select(price) %>% 
  bind_cols(predict(OLS_6, kc_testing)) %>% 
  # add 95% prediction intervals to results
  bind_cols(predict(OLS_6, kc_testing, type = "pred_int")) 

kc_testing %>% 
  select(price) %>% 
  bind_cols(predict(lasso_6, kc_testing))

kc_testing %>% 
  select(price) %>% 
  bind_cols(predict(ridge_6, kc_testing))