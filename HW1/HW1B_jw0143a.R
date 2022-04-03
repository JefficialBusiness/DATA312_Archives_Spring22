# HW1B
# Jeffrey Williams
# Dr. Robinson
# January 28, 2022


# Clearing workspace
rm(list=ls())
gc()

# Invoking / installing libraries and fetching data
install.packages("Stat2Data")
library(tidyverse)
library(Stat2Data)


#+
data("FirstYearGPA")

# Numerical Variables: GPA, HSGPA, SATV, SATM, HU, SS
# Categorical Variables (All are Binary): Male, FirstGen, White, CollegeBound

# Identifying mean, variance, and standard deviation for numerical variable,
# GPA
mean(FirstYearGPA$GPA)
var(FirstYearGPA$GPA)
sd(FirstYearGPA$GPA)

# Finding correlation between two numerical variables, HSGPA and GPA
cor(FirstYearGPA$GPA, FirstYearGPA$HSGPA)
#+
# First scatter-plot and line of best fit for numerical variables
# Numerical Variables: College and High School GPA (GPA, HSGPA)
plot(FirstYearGPA$HSGPA, FirstYearGPA$GPA,
     main = "High School GPA vs. First Year College GPA",
     xlab = "GPA - First Year in College",
     ylab = "GPA - High School")
myline <- lm(FirstYearGPA$GPA ~ FirstYearGPA$HSGPA)
abline(myline)

# Retrieving analytical information about graph / line
summary(myline)
coef(myline)
#+

# ANALYSIS: There is statistical significance for the given correlation at the 
# 95% confidence interval

# Equation for line of best fit: y = 0.56x + 1.18

# Creating scatter-plots, experimenting with Tidyverse
ggplot(FirstYearGPA)+geom_point(aes(HSGPA, GPA))
ggplot(FirstYearGPA, aes(HSGPA, GPA))+geom_point()
FirstYearGPA %>% ggplot(aes(HSGPA, GPA)) + geom_point()
#+
# Scatter-plot colored by the categorical variable, CollegeBound
ggplot(data=FirstYearGPA) +
  geom_point(mapping=aes(x=HSGPA, y=GPA, color=CollegeBound))
#+
# Scatter-plot with the points colored by the numerical variable, SATM
ggplot(data=FirstYearGPA) +
  geom_point(mapping=aes(x=HSGPA, y=GPA, color=SATM))
#+
# Loess Curves
ggplot(data=FirstYearGPA) +
  geom_smooth(mapping=aes(x=HSGPA, y=GPA, color=SATM))
#+
# Adding layers upon layers
ggplot(data=FirstYearGPA) +
  geom_smooth(mapping=aes(x=HSGPA, y=GPA, color=SATM)) +
  geom_point(mapping=aes(x=HSGPA, y=GPA, color=SATM))
#+ 
ggplot(data=FirstYearGPA) + 
  geom_smooth(mapping=aes(x=HSGPA, y=GPA, linetype=as.factor(FirstGen)))
#+
ggplot(data=FirstYearGPA) +
  geom_point(mapping=aes(x=HSGPA, y=GPA, color=SATM)) +
  geom_smooth(mapping=aes(x=HSGPA, y=GPA, color=SATM)) 
#+
# Some experimentation with Facet Plots
ggplot(data=FirstYearGPA) + 
  geom_point(mapping=aes(x=HSGPA, y=GPA)) + 
  facet_wrap(~FirstGen)
#+
ggplot(data=FirstYearGPA) + 
  geom_point(mapping=aes(x=HSGPA, y=GPA)) + 
  facet_wrap(CollegeBound~FirstGen)
#+
ggplot(data=FirstYearGPA) + 
  geom_point(mapping=aes(x=HSGPA, y=GPA)) + 
  facet_grid(CollegeBound~FirstGen)
#+
# Creating subsets based on Boolean categorical variable, White
iswhite <- FirstYearGPA[FirstYearGPA$White==1,]
nonwhite <- FirstYearGPA[FirstYearGPA$White==0,]

# Conversion of binary variable, White, into Boolean statements for future use
FirstYearGPA$whiteBoolean <- FirstYearGPA$White > 0
#+
# T-test: Statistically Significant difference at 95% CI with p-value = 0.00025
t.test(nonwhite$GPA, iswhite$GPA)
#+
# Bar plot understanding proportion that are first-generation college students
ggplot(data=FirstYearGPA) + geom_bar(mapping=aes(x=FirstGen, y=stat(prop)))
#+
# Chi-square test on two categorical variables
# Statistical significance between two categorical variables at 95% CI
newtable <- table(FirstYearGPA$FirstGen, FirstYearGPA$White)
newtable
chsq=chisq.test(newtable)
chsq
chsq$observed
chsq$expected
round(chsq$expected, 2)
#+
# Creating a pie-chart
mytable <- table(FirstYearGPA$White)
myLabels <- names(mytable)
pie(mytable, myLabels, main="% of White Students")
#+
# Step 43b: examining ratios; how many non-white/white first-gens?
ggplot(data=FirstYearGPA) + geom_bar(mapping=aes(x=FirstGen, fill=whiteBoolean),
                                     position="fill")
#+ 
# Use of stat summary, examining GPA differences in those who identify with
# category "White" and those who do not
ggplot(data=FirstYearGPA) +
  stat_summary(mapping=aes(x=White, y=GPA))

# 45. stat_summary appears to be associated with geom_pointrange. I figured this
# out by submitting "?stat_summary" into the console and looking at usage
# information.
                                     