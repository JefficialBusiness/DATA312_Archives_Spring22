# Jeffrey Williams
# DATA-312
# Dr. Robinson
# 11 April 2022

# Cleaning workspace / loading libraries
rm(list = ls())
gc()

library(tidyverse)
library(modelr)

# Load dataset
raw_data <- read_csv('data312_ml_michaelr.csv')

# Sampling
raw_data_samplingframe <- raw_data %>% 
  mutate(snum = sample.int(n(), n()) / n())

training <- raw_data_samplingframe %>% filter(snum < 0.6) %>% select(-snum)

query <- raw_data_samplingframe %>% filter(snum >= 0.6, snum < 0.8) %>%
  select(-snum)

test <- raw_data_samplingframe %>% filter(snum > 0.8) %>% select(-snum)

training %>% write_csv('michaelr_training.csv')
query %>% write_csv('michaelr_query.csv')
test %>% write_csv('michaelr_test.csv')

# Exploration
training %>% ggplot(aes(x, y, color = Group)) + geom_point() + 
  facet_grid(vars(s), vars(t))

training %>% filter(Group == 'B') %>% ggplot(aes(x, y)) + geom_point()

training %>% filter(t == 1) %>% ggplot(aes(x, y)) + geom_point()

training %>% count(Group)

training %>% count(Group, t) %>% pivot_wider(names_from = t, values_from = n)

training_B <- training %>% filter(Group == "B")

training_B %>% ggplot(aes(x, y)) + geom_point()

lfit <- lm(y~x, training_B)

training_lfit <- training_B %>% add_predictions(lfit, var = 'lfit_y')

training_lfit %>% ggplot() + geom_point(aes(x, y)) + 
  geom_line(aes(x, lfit_y), color = 'red') 

training_lfit <- training_lfit %>% add_residuals(lfit, var = 'lfit_y')

training_lfit <- training_B %>% add_predictions(lfit, var = 'lfit_y') %>% 
  add_residuals(lfit, var = 'lfit_resid')

training_lfit %>% ggplot(aes(x, lfit_resid)) + geom_point()

polyfit <- lm(y~poly(x, 2), training_B)

training_both <- training_B %>% add_predictions(polyfit, var = 'polyfit_y') %>%
  add_residuals(polyfit, var = 'polyfit_resid')

training_both %>% ggplot(aes(x, polyfit_resid)) + geom_point()

query_B <- query %>% filter(Group == 'B') %>% 
  add_predictions(lfit, var = 'lfit_pred') %>%
  add_residuals(lfit, var = 'lfit_resid') %>%
  add_predictions(polyfit, var = 'polyfit_pred') %>%
  add_residuals(polyfit, var = 'polyfit_resid')

query_B %>% ggplot() + geom_point(aes(x, y)) + 
  geom_line(aes(x, polyfit_pred), color = 'red') +
  geom_line(aes(x, lfit_pred), color = 'blue')

test_B <- test %>% filter(Group == 'B') %>%
  add_predictions(polyfit, var = 'polyfit_pred') %>% 
  add_residuals(polyfit, var = 'polyfit_resid')

test_B %>% ggplot(aes(polyfit_resid)) + geom_histogram()

# Shifting to use of my own data

# Loading raw data
my_data <- read_csv('5030143_HW4A.csv')

# Sampling
my_samplingframe <- my_data %>% 
  mutate(snum = sample.int(n(), n()) / n())

my_training <- my_samplingframe %>% filter(snum < 0.6) %>% select(-snum)

my_query <- my_samplingframe %>% filter(snum >= 0.6, snum < 0.8) %>%
  select(-snum)

my_test <- my_samplingframe %>% filter(snum > 0.8) %>% select(-snum)

my_training %>% write_csv('5030143_training.csv')
my_query %>% write_csv('5030143_query.csv')
my_test %>% write_csv('5030143_test.csv')

# Exploration / visualization

my_training %>% filter(t == 1) %>% ggplot(aes(x, y)) + geom_point()

my_training %>% filter(t == 0) %>% ggplot(aes(x, y)) + geom_point()

my_training %>% count(Group) %>% select(n) %>% chisq.test()

my_training %>% ggplot(aes(x, y, color = Group)) + geom_point() +
  facet_grid(vars(s), vars(t))

my_training %>% filter(Group == 'F') %>% 
  ggplot(aes(x, y, color = Group)) + geom_point() +
  facet_grid(vars(s), vars(t))

my_training %>% filter(Group == 'F') %>% ggplot(aes(x, y)) + geom_point()

my_training %>% filter(t == 1) %>% ggplot(aes(x, y)) + geom_point()

# I have chosen Group F, as it looks rather outlying. Immensely apparent is a 
# long and narrow series of plots jutting out to the right.
