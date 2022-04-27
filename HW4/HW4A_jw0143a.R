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

# Identifying Modeling Problem
training_B <- training %>% filter(Group == "B")

training_B %>% ggplot(aes(x, y)) + geom_point()

# Trying linear fit
lfit <- lm(y~x, training_B)

training_lfit <- training_B %>% add_predictions(lfit, var = 'lfit_y')

training_lfit %>% ggplot() + geom_point(aes(x, y)) + 
  geom_line(aes(x, lfit_y), color = 'red') 

training_lfit <- training_lfit %>% add_residuals(lfit, var = 'lfit_y')

training_lfit <- training_B %>% add_predictions(lfit, var = 'lfit_y') %>% 
  add_residuals(lfit, var = 'lfit_resid')

training_lfit %>% ggplot(aes(x, lfit_resid)) + geom_point()

# Trying Polyfit
polyfit <- lm(y~poly(x, 2), training_B)

training_polyfit <- training_B %>% add_predictions(polyfit, var = 'polyfit_y') %>%
  add_residuals(polyfit, var = 'polyfit_resid')

training_polyfit %>% ggplot(aes(x, polyfit_resid)) + geom_point()

# Query - Inspection / Investigation
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

my_training %>% count(Group, t) %>% 
  pivot_wider(names_from = t, values_from = n) %>%
  column_to_rownames('Group') %>% chisq.test()

my_training %>% count(Group) %>% select(n) %>% chisq.test()

my_training %>% ggplot(aes(x, y, color = Group)) + geom_point() +
  facet_grid(vars(s), vars(t))

my_training %>% filter(Group == 'H') %>% 
  ggplot(aes(x, y, color = Group)) + geom_point() +
  facet_grid(vars(s), vars(t))

my_training %>% filter(Group == 'H') %>% ggplot(aes(y, x)) + geom_point()

my_training %>% filter(t == 1) %>% ggplot(aes(x, y)) + geom_point()

# -- Modeling Problem 1 --

# In this modeling problem, I will be looking strictly at the 'Group' value, H.
# I will be considering x to be the response variable, with respect to y
# (quantitative explanatory variable) and Group H (categorical explanatory
# variable). There appears to be a correlation in this group, such that the
# data, or x, decreases as y increases. Interestingly, there also appears
# to be a cutoff for values at a certain point in y. In the graph's original
# orientation, there are no values beneath a certain point in y; all values are
# either equivalent to that cutoff point or higher, hence my decision to invert
# the axes, such that y values are portrayed on the x axis, and x values are
# portrayed on the y axis (x is the dependent variable and y is the explanatory
# variable alongside `Group` = B. Using the original orientation may not be
# ideal, as it would mean that I am predicting values past an apparent cutoff 
# line.

# By first impressions, I suspect that linear regression can be a workable 
# option given the pattern of this data, that may fit it somewhat well. The data
# seems to be steadily decreasing, so it might make some sense.

my_trH <- my_training %>% filter(Group == 'H')

H_lfit <- lm(y~x, data = my_trH)

my_trH %>% ggplot(aes(x, y)) + geom_point() # Note the cutoff at y = 0
my_trH %>% ggplot(aes(y, x)) + geom_point()

trH_lfit <- my_trH %>% 
  add_predictions(H_lfit, var = 'trH_lfit_y') %>%
  add_residuals(H_lfit, var = 'trH_lfit_resid')

trH_lfit %>% ggplot() + geom_point(aes(y, x)) + 
  geom_line(aes(trH_lfit_y, x), color = 'blue')

# We can see the cutoff for the values at y = 0. There are few (or no)
# values in the data for y < 0. That being said, a linear fit looks to "work"
# rather roughly, but these cutoff are not well accommodated by this model.
# The line would need to be truncated to start only at y = 0. With that aside,
# the line might not fare well in the long run, as it appears to peel away from
# the data at circa y = 300.

# For my second proposed model, I will be considering a polynomial fit. I
# suspect this may help with the apparent curve in the line that can now be
# seen with the linear model peeling away from the data. It may also help
# accommodate the context of the cutoff.

H_polyfit <- lm(y~poly(x, 2), my_trH)

trH_polyfit <- my_trH %>%
  add_predictions(H_polyfit, var = 'trH_polyfit_y') %>%
  add_residuals(H_polyfit, var = 'trH_polyfit_resid')

trH_polyfit %>% ggplot() + geom_point(aes(y, x)) + 
  geom_line(aes(trH_polyfit_y, x), color='green')

# Considering the plot, it seems a polynomial fit works slightly better, as it
# remains close to the data for somewhat a longer duration than the linear 
# model, which starts to quickly peel away at some point. The curve appears to
# better accommodate the cutoff at y = 0 as well.

# Looking at Residuals
trH_lfit %>% ggplot(aes(trH_lfit_resid)) + geom_histogram()
trH_polyfit %>% ggplot(aes(trH_polyfit_resid)) + geom_histogram()

trH_lfit %>% summarize(mean = mean(trH_lfit_resid), sd = sd(trH_lfit_resid))
trH_polyfit %>% 
  summarize(mean = mean(trH_polyfit_resid), sd = sd(trH_polyfit_resid))

trH_lfit %>% ggplot(aes(trH_lfit_resid, x)) + geom_point()
trH_polyfit %>% ggplot(aes(trH_polyfit_resid, x)) + geom_point()

# It appears that the best candidate model is a polynomial fit. Admittedly,
# neither models are the most ideal, with high standard deviations consistent
# with the data being widely spread out from the mean, but the polyfit has the 
# lowest of the two (sd = 82.1, versus lfit's sd = 84.3), so the data is closest 
# to the mean with this model. Histogram plots are rather inconclusive, as are 
# the dotplots; generally, there are far too many points spread out far from the
# mean. Neither option is close, but the polyfit is the closest.

my_qH <- my_query %>%
  filter(Group == 'H') %>%
  add_residuals(H_lfit, var='qH_lfit_resid') %>%
  add_predictions(H_lfit, var='qH_lfit_pred') %>%
  add_residuals(H_polyfit,var='qH_polyfit_resid') %>%
  add_predictions(H_polyfit, var='qH_polyfit_pred')

my_qH %>% ggplot() + geom_point(aes(y, x)) + 
  geom_line(aes(qH_polyfit_pred, x), color = 'green') +
  geom_line(aes(qH_lfit_pred, x), color = 'orange')

my_qH %>% ggplot(aes(qH_lfit_resid, x)) + geom_point()
my_qH %>% ggplot(aes(qH_polyfit_resid, x)) + geom_point()
my_qH %>% summarize(mean = mean(qH_lfit_resid), sd = sd(qH_lfit_resid))
my_qH %>% summarize(mean = mean(qH_polyfit_resid), sd = sd(qH_polyfit_resid))

# It looks like the polyfit model works best in this case, judging by
# similarities between results from the testing stage (visually, and in terms of
# standard residual analysis) so I will proceed to the testing stage with that 
# alone.

# Testing
my_tH <- my_test %>% filter(Group == 'H') %>%
  add_residuals(H_polyfit, var='tH_polyfit_resid') %>%
  add_predictions(H_polyfit, var='tH_polyfit_pred')

my_tH %>% ggplot(aes(tH_polyfit_resid)) + geom_histogram()
my_tH %>% ggplot(aes(tH_polyfit_resid, x)) + geom_point()
my_tH %>% summarize(mean = mean(tH_polyfit_resid), sd = sd(tH_polyfit_resid))

# Observations: The standard deviation seems to have lowered quite a bit from
# my results in the training and query sample for the polynomial fit (sd = 79.2
# compared to 82.1 for query and 87.3 for training). This is not a very good
# fit, though it may have been more preferable over a linear model. This model
# does not well accommodate the fact that the points are spread out rather
# liberally, with many points far from the mean given by the line/curve.


# -- Modeling Problem 2 --

# In this modeling problem, I will be focusing on the 'Group' variable, F. It
# appears quite evident that there is a pattern rather unusual in comparison
# with the rest of the groups, in that the points are very closely-knit and seem
# to resemble a linear pattern, at least from the perspective of the facet plot
# portraying all of the training data, split according to variables `s` and `t`.

my_trF <- my_training %>% filter(Group == 'F')

my_trF %>% ggplot(aes(x, y)) + geom_point()

# After plotting just Group F on the graph, it remains evident that there is a 
# rather linear-looking pattern with this group in terms of `x`, `y`
# relationship. Considering this, along with the fact of how closely-knit the 
# points generally are, I propose a linear model.

# Alternatively, I propose a polynomial fit model; I suspect an extremely subtle
# curve in the plot which may be better accommodated by such a model.

# Trying lfit
F_lfit <- lm(y~x, data = my_trF)

F_polyfit <- lm(y~poly(x, 2), my_trF)

trF_lfit <- my_trF %>%
  add_predictions(F_lfit, var = 'trF_lfit_y') %>%
  add_residuals(F_lfit, var = 'trF_lfit_resid')

trF_lfit %>% ggplot() + geom_point(aes(x,y)) + 
  geom_line(aes(x, trF_lfit_y), color = 'orange')

# Clearly, I was deceived and now see that a linear fit might not be the most
# ideal after all. It is closest to the data between x = 0 and x = 50 with 
# smaller discrepancies, but begins to depart rather quickly at roughly
# x = 60-65.

# Trying polyfit
trF_polyfit <- my_trF %>%
  add_predictions(F_polyfit, var = 'trF_polyfit_y') %>%
  add_residuals(F_polyfit, var = 'trF_polyfit_resid')

trF_polyfit %>% ggplot() + geom_point(aes(x, y)) +
  geom_line(aes(x, trF_polyfit_y), color = 'green')

# I see that the polyfit model works much better with the data, and well
# accommodates the curve I failed to see at first. It is worth noting the
# apparent beginning of a departure from the data somewhere between x = 85 and
# x = 90, suggestive that this model may have predictive limitations as well.

# Looking at Residuals
trF_lfit %>% ggplot(aes(trF_lfit_resid)) + geom_histogram()
trF_polyfit %>% ggplot(aes(trF_polyfit_resid)) + geom_histogram()

trF_lfit %>% summarize(mean = mean(trF_lfit_resid), sd = sd(trF_lfit_resid))
trF_polyfit %>% 
  summarize(mean = mean(trF_polyfit_resid), sd = sd(trF_polyfit_resid))

trF_lfit %>% ggplot(aes(x, trF_lfit_resid)) + geom_point()
trF_polyfit %>% ggplot(aes(x, trF_polyfit_resid)) + geom_point()

# Analysis: From plotting the residuals as histograms, it can be seen that there 
# is a much more normal distribution for the polyfit model, while the lfit model 
# shows a leftward skew on the histogram. Moreover, the standard deviation for 
# the polyfit is over 0.100 smaller than the standard deviation for the lfit 
# model. Plotting the residuals as points show a hump-like shape for the lfit
# model, while the plots are significantly more balanced around 0 for the
# polyfit model.

my_qF <- my_query %>%
  filter(Group == 'F') %>%
  add_residuals(F_lfit, var='qF_lfit_resid') %>%
  add_predictions(F_lfit, var='qF_lfit_pred') %>%
  add_residuals(F_polyfit,var='qF_polyfit_resid') %>%
  add_predictions(F_polyfit, var='qF_polyfit_pred')

my_qF %>% ggplot() + geom_point(aes(x, y)) + 
  geom_line(aes(x, qF_polyfit_pred), color = 'green') +
  geom_line(aes(x, qF_lfit_pred), color = 'orange')

my_qF %>% ggplot(aes(x, qF_lfit_resid)) + geom_point()
my_qF %>% ggplot(aes(x, qF_polyfit_resid)) + geom_point()
my_qF %>% summarize(mean = mean(qF_lfit_resid), sd = sd(qF_lfit_resid))
my_qF %>% summarize(mean = mean(qF_polyfit_resid), sd = sd(qF_polyfit_resid))

# Analysis: It looks like the polyfit model fits the data more smoothly than
# the linear model. The linear model peels away, whereas the polyfit model does
# not with the query sample. Running standard residual analysis reveals that
# there remains a significant difference between the standard deviation for the
# polyfit model and the linear fit model, such that polyfit's is smaller, not
# quite by 0.100 this time, but relatively close. By inspecting the graph
# visually, it can be seen that the polyfit model accommodates the data better.
# I will proceed with this model.

# Testing
my_tF <- my_test %>% filter(Group == 'F') %>%
  add_residuals(F_polyfit, var='tF_polyfit_resid') %>%
  add_predictions(F_polyfit, var='tF_polyfit_pred')

my_tF %>% ggplot(aes(tF_polyfit_resid)) + geom_histogram()
my_tF %>% ggplot(aes(x, tF_polyfit_resid)) + geom_point()
my_tF %>% summarize(mean = mean(tF_polyfit_resid), sd = sd(tF_polyfit_resid))

# Observations: The standard deviation remains lower for the polyfit model
# by similar margins as with the earlier samples. The polyfit model seems to
# be an okay fit, and seems to work better than the linear model. The
# variability is rather low in general, especially in stark comparison to the
# previous problem with Group `H`, but there is definitely deviation from the
# mean / line.
