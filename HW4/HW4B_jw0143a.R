# Jeffrey Williams
# Homework 4B
# Dr. Robinson
# DATA-312

# Clearing Workspace / Loading libraries
rm(list = ls())
gc()

library(tidyverse)
library(dplyr)
library(e1071)
library(stevedata) # For my dataset

data("spam", package = 'kernlab')

raw_data_samplingframe <- spam %>% select(-george, -num650) %>% 
  mutate(snum = sample.int(n(), n()) / n())

training <- raw_data_samplingframe %>% filter(snum < 0.6) %>% select(-snum)

query <- raw_data_samplingframe %>% filter(snum >= 0.6, snum < 0.8) %>%
  select(-snum)

test <- raw_data_samplingframe %>% filter(snum > 0.8) %>% select(-snum)

training %>% write_csv('spam_training.csv')
query %>% write_csv('spam_query.csv')
test %>% write_csv('spam_test.csv')

training_input <- training %>% select(-type)
training_truth <- training$type

query_input <- query %>% select(-type)
query_truth <- query$type

test_input <- test %>% select(-type)
test_truth <- test$type

# Training
spam_pca <- training_input %>% prcomp()

spam_pca$x %>% as_tibble() %>% mutate(type = training_truth) %>%
  ggplot(aes(PC1, PC2, color = type)) + geom_point()

# Attempting k-means analysis
spam_kmeans <- training_input %>% kmeans(2)

kmeans_results <- tibble(training_truth, km = spam_kmeans$cluster)

kmeans_results %>% count(training_truth, km) %>% 
  pivot_wider(names_from = km, values_from = n)

ct <- kmeans_results %>% count(training_truth, km) %>%
  pivot_wider(names_from = km, values_from = n) %>%
  column_to_rownames('training_truth')

chisq.test(ct)

spam_pca$x %>% as_tibble() %>% 
  ggplot(aes(PC1, PC2, color = spam_kmeans$cluster)) + geom_point()

# Trying SVM training
svm_linear <- training_input %>% svm(y = training_truth, kernel = 'linear')
svm_poly <- training_input %>% svm(y = training_truth, kernel = 'polynomial')
svm_radial <- training_input %>% svm(y = training_truth, kernel = 'radial')
svm_sigmoid <- training_input %>% svm(y = training_truth, kernel = 'sigmoid')

training_pca <- spam_pca$x %>% as_tibble() %>% mutate(type = training_truth)

sl <- svm(type~., data = training_pca, kernel = 'linear')
sp <- svm(type~., data = training_pca, kernel = 'polynomial')
sr <- svm(type~., data = training_pca, kernel = 'radial')
ss <- svm(type~., data = training_pca, kernel = 'sigmoid')

plot(sl, training_pca, PC1~PC2)
plot(sp, training_pca, PC1~PC2)
plot(sr, training_pca, PC1~PC2)
plot(ss, training_pca, PC1~PC2)

# SVM Query
predict(svm_linear, query_input)

query_results <- tibble(query_truth, 
                        linear = predict(svm_linear, query_input),
                        poly = predict(svm_poly, query_input),
                        radial = predict(svm_radial, query_input),
                        sigmoid = predict(svm_sigmoid, query_input))

query_results1 <- query_results %>% pivot_longer(cols=!query_truth)

query_results2 <- query_results1 %>%
  mutate(tp = (query_truth == 'spam' & value == 'spam'),
         tn = (query_truth == 'nonspam' & value == 'nonspam'),
         fp = (query_truth == 'nonspam' & value == 'spam'),
         fn = (query_truth == 'spam' & value == 'nonspam'))

query_results3 <- query_results2 %>% group_by(name) %>%
  summarize(tp = sum(tp),
            tn = sum(tn),
            fp = sum(fp),
            fn = sum(fn))

query_results3 %>% mutate(accuracy = (tp + tn) / (tp + tn + fp + fn),
                          sensitivity = tp / (tp + fn),
                          specificity = tn / (tn + fp),
                          ppv = tp / (tp + fp),
                          npv = fn / (tn + fn),
                          f1 = (2 * tp) / (2 * tp + fp + fn))

# SVM Test
test_results <- tibble(test_truth, 
                        linear = predict(svm_linear, test_input))

test_results1 <- test_results %>% pivot_longer(cols=!test_truth)

test_results2 <- test_results1 %>%
  mutate(tp = (test_truth == 'spam' & value == 'spam'),
         tn = (test_truth == 'nonspam' & value == 'nonspam'),
         fp = (test_truth == 'nonspam' & value == 'spam'),
         fn = (test_truth == 'spam' & value == 'nonspam'))

test_results3 <- test_results2 %>% group_by(name) %>%
  summarize(tp = sum(tp),
            tn = sum(tn),
            fp = sum(fp),
            fn = sum(fn))

test_results3 %>% mutate(accuracy = (tp + tn) / (tp + tn + fp + fn),
                          sensitivity = tp / (tp + fn),
                          specificity = tn / (tn + fp),
                          ppv = tp / (tp + fp),
                          npv = fn / (tn + fn),
                          f1 = (2 * tp) / (2 * tp + fp + fn))

# -- Using my data set --

# Using the TV16 data set, I will be observing whether or not a certain person 
# voted for Donald Trump in the 2016 presidential election based on a variety of 
# numerical variables representative of different demographic information, 
# including household income, a ideology, partisanship (on a scale of 1-7), 
# weight of importance of religion, frequency of prayer, fear of other races, 
# belief that racism is rare in the United States, weight of religiosity, and 
# empathetic racism. It is possible that these variables could be more carefully 
# selected, given the differences in how the aforementioned beliefs of people 
# are weighed (some are on a scale of 7, some 6, some 4, etc.)

# `votetrump` is the binary variable serving as the truth variable. A value of
# 1 indicates that an observation (person) voted for Donald Trump in 2016,
# and a value of 0 indicates that a person did not. In other words, the response
# variable is whether a given person voted for Donald Trump, based on the
# array of explanatory numerical variables representing demographics / opinion.
# i.e. If one's religious alignment is stronger, are they more likely to vote
# for Donald Trump than one whose alignment is weaker?

# Note: This is a rather large data set (there are many rows). Initial 
# processing of the data was computationally expensive (took 1 to 2 minutes to
# train each SVM and caused a observed significant rise in CPU temperature). As 
# such, with the potential bias acknowledged, this data frame has been truncated 
# to reduce computational time (this was done in the midst of pulling samples 
# from the data for the modeling process).

# Variables prefixed with my_ to ensure no interference with Worksheet data.

data("TV16", package = "stevedata")

my_samplingframe <- TV16 %>% select(-state, -age, -female, -collegeed, -racef,
                                     -bornagain)

my_samplingframe <- na.omit(my_samplingframe) %>%
  mutate(snum = sample.int(n(), n()) / n()) 

my_training <- my_samplingframe %>% filter(snum < 0.06*2) %>% select(-snum)

my_query <- my_samplingframe %>% filter(snum >= 0.06*2, snum < 0.08*2) %>% 
  select(-snum)

my_test <- my_samplingframe %>% filter(snum >= 0.08*2) %>% select(-snum)

my_training %>% write_csv('HW4B_training.csv')
my_query %>% write_csv('HW4B_query.csv')
my_test %>% write_csv('HW4B_test.csv')

# Separating Correct Answer / Identifying Variable
my_tr_input <- my_training %>% select(-votetrump)
my_tr_truth <- my_training$votetrump %>% as.factor()

my_q_input <- my_query %>% select(-votetrump)
my_q_truth <- my_query$votetrump %>% as.factor()

my_t_input <- my_test %>% select(-votetrump)
my_t_truth <- my_test$votetrump %>% as.factor()

# SVM Training
trump_pca <- my_tr_input %>% prcomp()

trump_pca$x %>% as_tibble() %>% mutate(votetrump = my_tr_truth) %>%
  ggplot(aes(PC1, PC2, color = votetrump)) + geom_point()

# Observation: there is an evident divide here, suggesting that in general, it
# is possible that one's identification with a particular identity or ideology
# may be correlated to whether they vote for Donald Trump in 2016.

# Trying SVM training against data
my_slinear <- my_tr_input %>% svm(y = my_tr_truth, kernel = 'linear')
my_spoly <- my_tr_input %>% svm(y = my_tr_truth, kernel = 'polynomial')
my_sradial <- my_tr_input %>% svm(y = my_tr_truth, kernel = 'radial')
my_ssigmoid <- my_tr_input %>% svm(y = my_tr_truth, kernel = 'sigmoid')

my_tr_pca <- trump_pca$x %>% as_tibble() %>% mutate(votetrump = my_tr_truth)

# Training SVM against data after PCA transformation
my_sl <- svm(votetrump~., data = my_tr_pca, kernel = 'linear')
my_sp <- svm(votetrump~., data = my_tr_pca, kernel = 'polynomial')
my_sr <- svm(votetrump~., data = my_tr_pca, kernel = 'radial')
my_ss <- svm(votetrump~., data = my_tr_pca, kernel = 'sigmoid')

plot(my_sl, my_tr_pca, PC1~PC2)
plot(my_sp, my_tr_pca, PC1~PC2)
plot(my_sr, my_tr_pca, PC1~PC2)
plot(my_ss, my_tr_pca, PC1~PC2)

# SVM Query

# Proceeding to the query stage, I am omitting the polynomial model due to
# an observed large deviation from the trends of the plots. This model did an
# exceptionally poor job at separating the positives from the negatives,
# presumably because it seeks to accommodate the negatives (those who do not
# vote for Trump in 2016) primarily, generally ignoring the positives, so the
# separating surface is established in a way that excludes as many negatives
# as possible from the red area. The data is rather biased; stratifying the 
# sampling frames may potentially lead to more optimal performance than what is 
# currently seen.

my_q_results <- tibble(my_q_truth, 
                        my_sl_pred = predict(my_slinear, my_q_input),
                        my_sr_pred = predict(my_sradial, my_q_input),
                        my_ss_pred = predict(my_ssigmoid, my_q_input))

my_q_results1 <- my_q_results %>% pivot_longer(cols=!my_q_truth)

my_q_results2 <- my_q_results1 %>%
  mutate(my_tp = (my_q_truth == '1' & value == '1'),
         my_tn = (my_q_truth == '0' & value == '0'),
         my_fp = (my_q_truth == '0' & value == '1'),
         my_fn = (my_q_truth == '1' & value == '0'))

my_q_results3 <- my_q_results2 %>% group_by(name) %>%
  summarize(my_tp = sum(my_tp),
            my_tn = sum(my_tn),
            my_fp = sum(my_fp),
            my_fn = sum(my_fn))

my_q_results3 %>% mutate(my_accuracy = 
                           (my_tp + my_tn) / (my_tp + my_tn + my_fp + my_fn),
                         my_sensitivity = my_tp / (my_tp + my_fn),
                         my_specificity = my_tn / (my_tn + my_fp),
                         my_ppv = my_tp / (my_tp + my_fp),
                         my_npv = my_fn / (my_tn + my_fn),
                         my_f1 = (2 * my_tp) / (2 * my_tp + my_fp + my_fn))

# Observations: Looking at results, it appears that the radial and linear models
# are the best candidates in terms of accuracy and f1 score. While in the plot,
# the sigmoid model appears to attempt to reason with the separation of the
# data, its score is lower than both of the aforementioned "better" models by
# a considerable margin (f1 = roughly 82% versus roughly 87% for both linear and 
# radial, accuracy = roughly 84% versus roughly 88% and 89% for both linear and 
# radial, respectively).

# SVM Testing:

# Generally, the radial model appears to outperform the linear model, albeit by
# smaller margins, so I will proceed with the radial model.

my_t_results <- tibble(my_t_truth, 
                       my_sr_pred = predict(my_sradial, my_t_input))

my_t_results1 <- my_t_results %>% pivot_longer(cols=!my_t_truth)

my_t_results2 <- my_t_results1 %>%
  mutate(my_tp = (my_t_truth == '1' & value == '1'),
         my_tn = (my_t_truth == '0' & value == '0'),
         my_fp = (my_t_truth == '0' & value == '1'),
         my_fn = (my_t_truth == '1' & value == '0'))

my_t_results3 <- my_t_results2 %>% group_by(name) %>%
  summarize(my_tp = sum(my_tp),
            my_tn = sum(my_tn),
            my_fp = sum(my_fp),
            my_fn = sum(my_fn))

my_t_results3 %>% mutate(my_accuracy = 
                           (my_tp + my_tn) / (my_tp + my_tn + my_fp + my_fn),
                         my_sensitivity = my_tp / (my_tp + my_fn),
                         my_specificity = my_tn / (my_tn + my_fp),
                         my_ppv = my_tp / (my_tp + my_fp),
                         my_npv = my_fn / (my_tn + my_fn),
                         my_f1 = (2 * my_tp) / (2 * my_tp + my_fp + my_fn))

# Observations: My test results for the radial model appear to be similar, with
# an f1 score of 86% and an accuracy score of 89%. Notably, sensitivity and
# specificity scores are lower by a considerable margin, compared to the query
# results. The f1 score for the radial model test result is, moreover, lower
# than the f1 score for the linear model query result by .002, piquing further
# curiosity, in that it begs further investigation as to how both models are
# different, and whether one truly performs better than the other. While the
# sigmoid model has a consistent gap in performance between the radial and
# linear model, the radial and linear model appear to be more or less neck and
# neck. Perhaps tuning with hyperparameters may be helpful.