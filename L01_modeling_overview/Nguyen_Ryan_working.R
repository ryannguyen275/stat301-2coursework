# wage data cleaning
library(tidyverse)
library(skimr)
wage <- read_csv("data/wage.csv") %>% 
  janitor::clean_names()

skim(wage)

# change to factors and order alphabetically
# if wanted to sort differently, need to write out each individual level
wage_clean <- wage %>% 
  mutate(maritl = fct_relevel(maritl, sort))
levels(wage_clean$maritl)

# convert factors with for loop
factor_var <- c("maritl", "race", "education", "region", "jobclass", "health", "health_ins")

for(var in factor_var){
  wage[[var]] <- fct_relevel(wage[[var]], sort)
}

# use write_csv not write.csv

# see if a folder exists; if it doesn't, create one
if(!dir.exists("data/processed")){
  dir.create("data/processed")
}

# create processed folder
dir.create("data/processed")

write_rds(wage, "data/processed/wage_clean.rds")


### Exercise 3
skim(auto)
skim(college)

# cleaning up auto
auto <- auto %>% 
  mutate(horsepower = as.numeric(horsepower), 
         origin = as_factor(origin),
         cylinders = as_factor(cylinders))
# saving auto as an rds
write_rds(auto, "data/processed/auto.rds")

college <- college %>% 
  janitor::clean_names() %>% 
  mutate(private = as_factor(private))

write_rds(auto, "data/processed/college.rds")

write_rds(college, "data/processed/college_clean.rds")

### Exercise 4
auto %>%
  ggplot(aes(x = mpg, y = origin)) +
  geom_boxplot()

auto %>%
  ggplot(aes(x = displacement, y = origin)) +
  geom_boxplot()

auto %>%
  ggplot(aes(x = weight, y = origin)) +
  geom_boxplot()

auto %>%
  ggplot(aes(x = acceleration, y = origin)) +
  geom_boxplot()

auto %>%
  ggplot(aes(x = year, y = origin)) +
  geom_boxplot()

auto %>%
  ggplot(aes(x = horsepower, y = origin)) +
  geom_boxplot()

### Exercise 6

college %>% 
  ggplot(aes(x = accept, y = top10perc)) + 
  geom_point()

college %>% 
  ggplot(aes(x = apps, y = top10perc)) + 
  geom_point()

college %>% 
  ggplot(aes(x = outstate, y = top10perc)) + 
  geom_point()

college %>% 
  ggplot(aes(x = ph_d, y = top10perc)) + 
  geom_point()

college %>% 
  ggplot(aes(x = enroll, y = top10perc)) + 
  geom_point()

college %>% 
  ggplot(aes(x = private, y = top10perc)) + 
  geom_boxplot()
