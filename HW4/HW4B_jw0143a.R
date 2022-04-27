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
data("MedGPA", package = "Stat2Data")

my_samplingframe <- MedGPA %>% select(-Accept, -Sex) %>%
  mutate(snum = sample.int(n(), n()) / n())

my_training <- my_samplingframe %>% filter(snum < 0.6) %>% select(-snum)

my_query <- my_samplingframe %>% filter(snum >= 0.6, snum < 0.8) %>% 
  select(-snum)

my_test <- my_samplingframe %>% filter(snum >= 0.8) %>% select(-snum)

my_training %>% write_csv('HW4B_training.csv')
my_query %>% write_csv('HW4B_query.csv')
my_test %>% write_csv('HW4B_test.csv')

# Separating Correct Answer / Identifying Variable
my_tr_input <- my_training %>% select(-Acceptance)
my_tr_truth <- my_training$Acceptance

my_q_input <- my_query %>% select(-Acceptance)
my_q_truth <- my_query$Acceptance

my_t_input <- my_test %>% select(-Acceptance)
my_t_truth <- my_test$Acceptance

# Training

my_tr_input[is.na(my_tr_input)] <- 0

med_pca <- my_tr_input %>% prcomp()

med_pca$x %>% as_tibble %>% mutate(type = my_tr_truth) %>%
  ggplot(aes(PC1, PC2, color = type)) + geom_point()

# Trying SVM training
my_linear <- svm(Acceptance~., data = my_training, kernel = 'linear')

my_linear <- my_tr_input %>% svm(y = my_tr_truth, kernel = 'linear')

my_poly <- svm(Acceptance~., data = my_training, kernel = 'polynomial')
my_radial <- svm(Acceptance~., data = my_training, kernel = 'radial')
my_sigmoid <- svm(Acceptance~., data = my_training, kernel = 'sigmoid')

my_tr_pca <- med_pca$x %>% as_tibble() %>% mutate(type = my_tr_truth)

my_sl <- svm(type~., data = my_tr_pca, kernel = 'linear')
my_sp <- svm(type~., data = my_tr_pca, kernel = 'polynomial')
my_sr <- svm(type~., data = my_tr_pca, kernel = 'radial')
my_ss <- svm(type~., data = my_tr_pca, kernel = 'sigmoid')

plot(my_sl, my_tr_pca, PC1~PC2)
plot(my_sp, my_tr_pca, PC1~PC2)
plot(my_sr, my_tr_pca, PC1~PC2)
plot(my_ss, my_tr_pca, PC1~PC2)
