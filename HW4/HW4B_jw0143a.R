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

data("spam", package = 'kernlab')

View(spam)

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
svm_linear <- svm(type~., data = training, kernel = 'linear')

svm_linear <- training_input %>% svm(y = training_truth, kernel = 'linear')

svm_poly <- svm(type~., data = training, kernel = 'polynomial')
svm_radial <- svm(type~., data = training, kernel = 'radial')
svm_sigmoid <- svm(type~., data = training, kernel = 'sigmoid')

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

# Using my dataset
data('storms')

