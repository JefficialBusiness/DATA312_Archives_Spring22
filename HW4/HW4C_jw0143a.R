rm(list=ls())
gc()

library(tidyverse)
library(modelr)
library(e1071)
library(usmap)

megadata <- read_csv('EduUnempPovPopCovidVoting_StatCrunchV2.csv')

temporary <- megadata %>% mutate(fips = countyFIPS) 

plot_usmap(regions = 'counties', data = temporary, values = 'gopPercent', 
           include = c('DC','MD','VA'))

# Cleaning / Sorting quantitative from categorical variables
megadata <- megadata %>% mutate(countyFIPS = as.factor(countyFIPS)) %>%
  mutate(StateFIPS = as.factor(StateFIPS)) %>%
  mutate(FIPSnumber = as.factor(FIPSnumber)) %>%
  mutate(Rural.urban_Continuum_Code_2003 = 
           as.factor(Rural.urban_Continuum_Code_2003)) %>%
  mutate(Rural_urban_continuum_code_2013 = 
           as.factor(Rural_urban_continuum_code_2013)) %>%
  mutate(PoliticsGroup = as.factor(PoliticsGroup))

raw_data_samplingframe <- megadata %>% mutate(snum = sample.int(n(), n()) / n())

training <- raw_data_samplingframe %>% filter(snum < 0.6) %>% select(-snum)

query <- raw_data_samplingframe %>% filter(snum >= 0.6, snum < 0.8) %>% 
  select(-snum)

test <- raw_data_samplingframe %>% filter(snum >= 0.8) %>% select(-snum)

pennsylvania <- training %>% filter(PostalCode == 'PA')

training %>% ggplot(aes(x = PCTPOVALL_2019, y = TOT_POP)) + geom_point()

training %>% ggplot(aes(TOT_POP)) + geom_histogram() + scale_x_log10()

# Gathering quantitative variables
quantitatives <- training %>% select(where(is.numeric))

categoricals <- training %>% select(where(is.factor))

training_pca <- quantitatives %>% prcomp()

training_pca$x %>% as_tibble() %>%
  mutate(PostalCode=training$PostalCode) %>%
  ggplot(aes(PC1, PC2, color = PostalCode)) + geom_point()

training_pca <- training %>% filter(PostalCode == 'MI') %>%
  select(where(is.numeric)) %>% prcomp()

training %>% filter(PostalCode == 'MI'|PostalCode == 'MS') %>% 
  ggplot(aes(TOT_POP, white, color = Rural_urban_continuum_code_2013)) +
  geom_point() +
  facet_wrap(~PostalCode, scale = 'free_x')

# Exploration
training %>% mutate(employment_rate = Civilian_labor_force_2019 / TOT_POP) %>%
  ggplot(aes(CDPerThou.04.2021, hispanic)) +
  geom_point()

lfit <- lm(CDPerThou.04.2021~hispanic, data = training)

polyfit <-lm(CDPerThou.04.2021~poly(hispanic, 2), data = training)

# Training lfit model
training_lfit_P1 <- training %>%
  add_predictions(lfit, var = 'P1tr_lfit_pred') %>%
  add_residuals(lfit, var = 'P1tr_lfit_resid')

training_lfit_P1 %>%
  ggplot(aes(hispanic, CDPerThou.04.2021)) +
  geom_point() + geom_line(aes(hispanic, P1tr_lfit_pred), color = 'blue')

# Training polyfit model
training_polyfit_P1 <- training %>%
  add_predictions(polyfit, var = 'P1tr_polyfit_pred') %>%
  add_residuals(polyfit, var = 'P1tr_polyfit_resid')

training_polyfit_P1 %>%
  ggplot(aes(hispanic, CDPerThou.04.2021)) +
  geom_point() + geom_line(aes(hispanic, P1tr_polyfit_pred), color = 'green')

# Comparison with residuals
training_lfit_P1 %>% 
  ggplot(aes(P1tr_lfit_resid, Rural_urban_continuum_code_2013)) + 
  geom_boxplot()

training_polyfit_P1 %>% 
  ggplot(aes(P1tr_polyfit_resid, Rural_urban_continuum_code_2013)) + 
  geom_boxplot()

training_lfit_P1 %>% summarize(mean = mean(P1tr_lfit_resid), 
                               sd = sd(P1tr_lfit_resid))

training_polyfit_P1 %>% 
  summarize(mean = mean(P1tr_polyfit_resid), sd = sd(P1tr_polyfit_resid))

# Query
query_P1 <- query %>% 
  add_predictions(lfit, var = 'P1q_lfit_pred') %>%
  add_residuals(lfit, var = 'P1q_lfit_resid') %>%
  add_predictions(polyfit, var = 'P1q_polyfit_pred') %>%
  add_residuals(polyfit, var = 'P1q_polyfit_resid')

query_P1 %>% ggplot() + geom_point(aes(hispanic, CDPerThou.04.2021)) +
  geom_line(aes(hispanic, P1q_lfit_pred), color = 'blue') +
  geom_line(aes(hispanic, P1q_polyfit_pred), color = 'orange')

query_P1 %>% ggplot(aes(P1q_lfit_resid)) + 
  geom_histogram()

query_P1 %>% 
  ggplot(aes(P1q_polyfit_resid, Rural_urban_continuum_code_2013)) + 
  geom_boxplot()

query_P1 %>% summarize(mean = mean(P1q_lfit_resid), sd = sd(P1q_lfit_resid))

query_P1 %>%
  summarize(mean = mean(P1q_polyfit_resid), sd = sd(P1q_polyfit_resid))

# Testing
test_P1 <- test %>% 
  add_predictions(polyfit, var = 'P1t_polyfit_pred') %>%
  add_residuals(polyfit, var = 'P1t_polyfit_resid')

test_P1 %>% ggplot() + geom_point(aes(hispanic, CDPerThou.04.2021)) +
  geom_line(aes(hispanic, P1t_polyfit_pred), color = 'orange') +
  labs(title = 'Predicting Trends in Hispanic COVID Death Rate by April 2021 
  (Polynomial Model)')

# -- Problem 2 --

# Visualization

P2_training_input <- training %>% select(-Rural_urban_continuum_code_2013)
P2_training_truth <- training$Rural_urban_continuum_code_2013

P2_query_input <- query %>% select(-Rural_urban_continuum_code_2013)
P2_query_truth <- query$Rural_urban_continuum_code_2013

P2_test_input <- test %>% select(-Rural_urban_continuum_code_2013)
P2_test_truth <- test$Rural_urban_continuum_code_2013

# Training

# Polynomial SVM
P2tr_svm_poly <- P2_training_input %>% select(TOT_POP,
                                              gopPercent,
                                              demPercent,
                                              black,
                                              white,
                                              hispanic,
                                              votes_gop,
                                              votes_dem) %>%
  svm(y = P2_training_truth, kernel = 'poly')

P2tr_poly_pred <- training %>% 
  mutate(predicted_RUCC = predict(P2tr_svm_poly, training %>% select(TOT_POP,
                                                                   gopPercent,
                                                                   demPercent,
                                                                   black,
                                                                   white,
                                                                   hispanic,
                                                                   votes_gop,
                                                                   votes_dem)))

P2tr_poly_scores <- P2tr_poly_pred %>% 
  mutate(correct_guess = (predicted_RUCC == 
                            Rural_urban_continuum_code_2013)) %>% 
  count(PostalCode, correct_guess)

P2tr_poly_scores %>% mutate(PostalCode = reorder(PostalCode, desc(n))) %>% 
  ggplot(aes(n, PostalCode, fill = correct_guess)) + geom_col() +
  labs(title = "Predicting Rural-Urban Continuum Code (Polynomial SVM)")

# Sigmoid SVM
P2tr_svm_radial <- P2_training_input %>% select(TOT_POP,
                                            gopPercent,
                                            demPercent,
                                            black,
                                            white,
                                            hispanic,
                                            votes_gop,
                                            votes_dem) %>%
  svm(y = P2_training_truth, kernel = 'radial')

P2tr_radial_pred <- training %>% 
  mutate(predicted_RUCC = predict(P2tr_svm_radial, training %>% select(TOT_POP,
                                                                   gopPercent,
                                                                   demPercent,
                                                                   black,
                                                                   white,
                                                                   hispanic,
                                                                   votes_gop,
                                                                   votes_dem)))

P2tr_radial_scores <- P2tr_radial_pred %>% 
  mutate(correct_guess = (predicted_RUCC == 
                            Rural_urban_continuum_code_2013)) %>% 
  count(PostalCode, correct_guess)

P2tr_radial_scores %>% mutate(PostalCode = reorder(PostalCode, desc(n))) %>% 
  ggplot(aes(n, PostalCode, fill = correct_guess)) + geom_col() +
  labs(title = "Predicting Rural-Urban Continuum Code (Radial SVM)")

# Polynomial
P2q_svm_poly <- P2_query_input %>% select(TOT_POP,
                                            gopPercent,
                                            demPercent,
                                            black,
                                            white,
                                            hispanic,
                                            votes_gop,
                                            votes_dem) %>%
  svm(y = P2_query_truth, kernel = 'polynomial')

P2q_poly_pred <- query %>% 
  mutate(predicted_RUCC = predict(P2q_svm_poly, query %>% select(TOT_POP,
                                                                   gopPercent,
                                                                   demPercent,
                                                                   black,
                                                                   white,
                                                                   hispanic,
                                                                   votes_gop,
                                                                   votes_dem)))

P2q_poly_scores <- P2q_poly_pred %>% 
  mutate(correct_guess = (predicted_RUCC == 
                            Rural_urban_continuum_code_2013)) %>% 
  count(PostalCode, correct_guess)

P2q_poly_scores %>% mutate(PostalCode = reorder(PostalCode, desc(n))) %>% 
  ggplot(aes(n, PostalCode, fill = correct_guess)) + geom_col() +
  labs(title = "Predicting Rural-Urban Continuum Code (Polynomial SVM)")

# Radial 
P2q_svm_radial <- P2_query_input %>% select(TOT_POP,
                                                gopPercent,
                                                demPercent,
                                                black,
                                                white,
                                                hispanic,
                                                votes_gop,
                                                votes_dem) %>%
  svm(y = P2_query_truth, kernel = 'radial')

P2q_radial_pred <- query %>% 
  mutate(predicted_RUCC = predict(P2q_svm_radial, query %>% select(TOT_POP,
                                                                       gopPercent,
                                                                       demPercent,
                                                                       black,
                                                                       white,
                                                                       hispanic,
                                                                       votes_gop,
                                                                       votes_dem)))

P2q_radial_scores <- P2q_radial_pred %>% 
  mutate(correct_guess = (predicted_RUCC == 
                            Rural_urban_continuum_code_2013)) %>% 
  count(PostalCode, correct_guess)

P2q_radial_scores %>% mutate(PostalCode = reorder(PostalCode, desc(n))) %>% 
  ggplot(aes(n, PostalCode, fill = correct_guess)) + geom_col() +
  labs(title = "Predicting Rural-Urban Continuum Code (Radial SVM)")

