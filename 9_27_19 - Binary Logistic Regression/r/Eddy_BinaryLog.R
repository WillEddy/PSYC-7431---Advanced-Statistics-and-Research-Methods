# Author: William Eddy, M.A. Candidate East Carolina University, william.sebastian.eddy@gmail.com
# Alternative name provided by Professor Poopypants algorithm: Booger Burger? Go Poopypants.

# For reproduction purposes, I have included installation commands for all required packages.

install.packages("ggplot2"); install.packages("cowplot"); install.packages("rstudioapi"); install.packages("compareGroups")
install.packages("tidyverse"); install.packages("psych"); install.packages("mdscore"); install.packages("car")
install.packages("lmtest"); install.packages("survey"); install.packages("ResourceSelection"); install.packages("pwr")


##########
##
## Use these packages
##
##########
library(ggplot2); library(cowplot); library(rstudioapi); library(compareGroups)
library(tidyverse); library(psych); library(mdscore); library(car)

## Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#Import raw data
data <- read.table("Log-06.dat")
# Name columns
colnames(data) <- c("SOCPROB","REPEAT","ADDSC","DROPOUT")

###########
##
## Test linearity of the logit using the Box-Tidwell test
## Done before labelling dichotomous variable values
##
###########

library(car)
boxTidwell(DROPOUT ~ ADDSC, ~ SOCPROB + REPEAT, data=data)

##########
##
## Implement data labels and categorize dichotomous variables as factors
##
##########

# Label variable values - this also sets them as dichotomous variables (factors in r)
data$SOCPROB <- factor(data$SOCPROB,
                       levels = c(0,1),
                       labels = c("0 - Yes 9th grade social problems", "1 - No 9th grade social problems"))

data$REPEAT <- factor(data$REPEAT,
                      levels = c(0,1),
                      labels = c("0 - Didn't repeat a grade", "1 - Repeated a grade"))

data$DROPOUT <- factor(data$DROPOUT,
                       levels = c(0,1),
                       labels = c("0 - Completed high school", "1 - Dropped out of high school"))



###########
##
## Create and view the intercept-only model
##
###########

LM1 <- glm(DROPOUT ~ 1, data=data, family="binomial")
summary(LM1)
# Return -2 Log Liklihood
LM1_minus2log <- -2*(logLik(LM1))



#####################################
##
## Create and view logistic regression using all variables to predict DROPOUT
##
#####################################

LM2 <- glm(DROPOUT ~ ., data=data, family="binomial")
summary(LM2)
anova(LM2, test="Chisq")
?anova
# Return -2 Log Liklihood
LM2_minus2log <--2*(logLik(LM2))

# Return odds ratios
exp(LM2$coefficients)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null.2 <- LM2$null.deviance/-2
ll.proposed.2 <- LM2$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
McFaddenR2 <- (ll.null.2 - ll.proposed.2) / ll.null.2
McFaddenR2
sqrt(McFaddenR2)

## The p-value for the R^2 above
1 - pchisq(2*(ll.proposed.2 - ll.null.2), df=(length(LM2$coefficients)-1))

# Create confusion matrix; Calculate sensitivity and specificity

threshold=0.1
predicted_values<-ifelse(predict(LM2,type="response")>threshold,1,0)
actual_values<-data$DROPOUT
conf_matrix<-table(predicted_values,actual_values)
conf_matrix

13/(24) # % dropouts correctly predicted - sensitivity
172/(226) # % non-dropouts correctly predicted - specificity



#####################################
##
## Create models without each variable and calculate delta(-2 log likelihood)
##
#####################################

LM3 <- glm(DROPOUT ~ SOCPROB + REPEAT, data=data, family="binomial")
# Return -2 Log Liklihood
LM3_minus2log <- -2*(logLik(LM3))
# Find delta(-2 log likelihood) for ADDSC
LM3_minus2log - LM2_minus2log

LM4 <- glm(DROPOUT ~ SOCPROB + ADDSC, data=data, family="binomial")
# Return -2 Log Liklihood
LM4_minus2log <- -2*(logLik(LM4))
# Find delta(-2 log likelihood) for REPEAT
LM4_minus2log - LM2_minus2log

LM5 <- glm(DROPOUT ~ ADDSC + REPEAT, data=data, family="binomial")
# Return -2 Log Liklihood
LM5_minus2log <- -2*(logLik(LM5))
# Find delta(-2 log likelihood) for SOCPROB
LM5_minus2log - LM2_minus2log



##################
##
## Plot the logistic regression
##
##################

predicted.data <- data.frame(
  probability.of.DROPOUT=LM2$fitted.values,
  DROPOUT=data$DROPOUT)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.DROPOUT, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.DROPOUT)) +
  geom_point(aes(color=DROPOUT), alpha=1, shape=4, stroke=2) +
  xlab("Probability rank") +
  ylab("Predicted probability of dropping out of high school")

 Save plot as png / pdf
 ggsave("Dropout_Probabilities.png")

##################
##
## Use CompareGroups package to show percentages across DROPOUT variable
## 
## CompareGroups package Output includes some tests where results
## differ from better understood individual tests in next section
## and so are ignored
##
## Conduct t test and chi square comparisons
##
##################

allgroups_compare <- compareGroups(DROPOUT ~ . , data = data)
 allgroups_compare
 summary(allgroups_compare)

allgroups_table <- createTable(allgroups_compare, show.ratio=TRUE)
allgroups_table

# Some descriptive statistics on DROPOUT by group
describe(Dropouts$ADDSC)
describe(Graduates$ADDSC)

# export2csv(allgroups_table, "../tables/allgroups_table.csv")

# Independent samples t-test - ADDSC by DROPOUT groups

# Standard deviations are similar, but N is different - reported separate variance t-test p value but
# confidence interval from shared variance test as suggested by professor
Dropouts <- subset(data, DROPOUT == "1 - Dropped out of high school")
Graduates <- subset(data, DROPOUT == "0 - Completed high school")
# Equal variances
t.test(Dropouts$ADDSC, Graduates$ADDSC, var.equal = TRUE)
# Separate variances
t.test(Dropouts$ADDSC, Graduates$ADDSC, var.equal = FALSE)

# Chi squared tests
chisq.test(data$REPEAT, data$DROPOUT, correct = FALSE)
chisq.test(data$SOCPROB, data$DROPOUT, correct = FALSE)
?chisq.test

##################
##
## Other tests - Not all reported
##
##################

# Likelihood ratio test
library(lmtest)
lrtest(LM1, LM2)

# Wald test (FAILED TO IMPLEMENT)
  library(survey)
  regTermTest(LM2, ~DROPOUT +., data=data)

# Hosmer-Lemeshow (likely implemented incorrectly rendering result unusable)
library(ResourceSelection)
Hosmer_Lem <- hoslem.test(data$DROPOUT,LM2$fitted.values)
Hosmer_Lem

# Use anova function to compare the two models
anova(LM1, LM2, test="Chisq")

# Power analysis
library(pwr)
pwr.f2.test(1, 249, .08, .05)
pwr.t.test(n=250, sig.level=.05, power=.08)
pwr.chisq.test(N = 250, df = 1, sig.level = 0.05, power = .08)


##################
##
## Evaluating the model
# Wald, Hosmer, McFadden model tests
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
## 
## Power analysis in R https://www.statmethods.net/stats/power.html
##
## StatQuest Example of logistic regression - I used some of this code.
## https://statquest.org/2018/07/23/statquest-logistic-regression-in-r/#code
## 
##################
