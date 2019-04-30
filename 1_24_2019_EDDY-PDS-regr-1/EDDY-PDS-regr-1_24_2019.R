# #1
# Create 2 dichotomous variables

DATA <- read.csv("IBM_HR_TRIMMED_3.csv", row.names=1)


# Pearson r for the 2 dichotomous variables

CHISQ_RESULT <- chisq.test (DATA$Attrition, DATA$Department)

CHISQ_RESULT

#       phi = sqrt(chi square / n)
# Did it backwards

PHI_RESULT <- sqrt ( CHISQ_RESULT$p.value / 1407)
PHI_RESULT

#
# Compare to chi-square calculated with contingency table (from old times)
# DO THIS

#____________________________________________________________________________________________

# #2
# Compute Pearson r between one dichotomous and one continuous variable
#     AKA Point bisearial correlation coefficient

ATTR_NUMERIC <- as.numeric (DATA$Attrition)

POINT_BISERIAL_RESULT <- cor.test (DATA$JobSatisfaction, ATTR_NUMERIC)

POINT_BISERIAL_RESULT

# Using the usual formula for computing the t to test the null hypothesis that rho is zero, 
# test the null hypothesis that the point 
# biserial correlation coefficient has a value of zero in the population. 
#
# #3
#
# Compare your computed value of t with that obtained when you 
# tested the relationship between the dichotomous variable and the 
# continuous variable in the traditional way, with a pooled variances 
# independent samples t-test, like we did when we were covering Chapter 7.
#
#
#
# #4
# Conduct a correlation/regression analysis predicting one of your continuous variables from another
# continuous variable. Start by evaluating the assumptions of the tests of significance (normality
# and homoscedasticity).  Obtain the correlation coefficient, regression line, test of significance, 
# and a 95% confidence interval for rho.  Obtain a scatter plot with the regression line superimposed
# on the scatter plot. Be prepared to write an APA-style report of the results should I call on you to do so.
#

library(psych)
library(ggplot2)

describe (DATA$DistanceFromHome)
describe (DATA$EnvironmentSatisfaction)

CORRELATION <- cor.test (DATA$DistanceFromHome, DATA$EnvironmentSatisfaction)

CORRELATION

REGRESSION <- lm (DATA$DistanceFromHome ~ DATA$EnvironmentSatisfaction)

summary(REGRESSION)


# https://stats.idre.ucla.edu/r/faq/how-can-i-do-a-scatterplot-with-regression-line-or-any-other-lines/

with(DATA,plot(DATA$JobInvolvement, DATA$DistanceFromHome))
abline(REGRESSION)


#
#
#
#



