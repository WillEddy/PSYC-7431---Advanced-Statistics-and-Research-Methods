library(ggplot2); library(cowplot); library(rstudioapi); library(compareGroups); library(tidyverse); library(psych); library(lmtest); library(mdscore)

# Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
data <- read.csv("../data/Known.csv")

glimpse(data)

# Name columns
# colnames(data) <- c("SOCPROB","REPEAT","ADDSC","DROPOUT")

# Label variable values - this also sets them as dichotomous variables (factors in r)
colnames(data)[1] <- "decision"

data$decision <- factor(data$decision,
                       levels = c(0,1),
                       labels = c("0 - Stop the research", "1 - Continue the research"))

data$gender <- factor(data$gender,
                      levels = c(0,1),
                      labels = c("0 - Female", "1 - Male"))

# Intercept only model
LR1 <- glm(decision ~ 1, data, family = "binomial")
LR1
summary(LR1)
# To get Exp(B), view exponentiated Estimate below:
exp(LR1$coefficients)
# Return -2 Log Liklihood
-2*(logLik(LR1))

# Wald chisq test?
# wald.test(coef(LR1), vcov(LR1), Terms=1)
# wald.test(LR2,2)

ll.null <- LR1$null.deviance/-2
ll.proposed <- LR1$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null


# For bigger models?
#  exp(cbind(ODDS=coef(LR1), confint(LR1)))

xtabs(~DROPOUT + SOCPROB, data = data)
xtabs(~DROPOUT + REPEAT, data = data)

###########
##
## Now do the actual logistic regression
##
###########
LR2 <- glm(decision ~ gender, data=data, family="binomial")
summary(LR2)
# To get Exp(B), view exponentiated Estimate below:
exp(LR2$coefficients)
# Return -2 Log Liklihood
-2*(logLik(LR2))

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
ll.null.2 <- LR2$null.deviance/-2
ll.proposed.2 <- LR2$deviance/-2
(ll.null.2 - ll.proposed.2) / ll.null.2

# In output:
# First Estimate is log(odds) that someone who is"Yes Social Problems" is also "Yes drop out of high school"

# Second Estimate is log(odds ratio) that a person who is  "Yes Social Problems" is also "Yes drop out of high school"
# If sample has "Yes Social Problems," the odds are 1.27 times greater to have
# "Yes drop out of high school" than if sample has "No Social Problems"

## Now calculate the overall "Pseudo R-squared" and its p-value
## NOTE: Since we are doing logistic regression...
## Null devaince = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviance = 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

## Lastly, let's  see what this logistic regression predicts, given
## that a patient is either female or male (and no other data about them).
predicted.data <- data.frame(
  probability.of.DROPOUT=logistic$fitted.values,
  SOCPROB=data$SOCPROB)

## We can plot the data...
ggplot(data=predicted.data, aes(x=SOCPROB, y=probability.of.DROPOUT)) +
  geom_point(aes(color=SOCPROB), size=5) +
  xlab("Social Problems") +
  ylab("Predicted probability of Dropping out of high school")

## Since there are only two probabilities (one for females and one for males),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.DROPOUT + SOCPROB, data=predicted.data)

#####################################
##
## Now we will use all of the data available to predict DROPOUT
##
#####################################

LR3 <- glm(decision ~ gender + idealism + relatvsm, data=data, family="binomial")
summary(LR3)
# To get Exp(B), view exponentiated Estimate below:
exp(LR3$coefficients)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

## now we can plot the data
predicted.data <- data.frame(
  probability.of.DROPOUT=logistic$fitted.values,
  DROPOUT=data$DROPOUT)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.DROPOUT, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.DROPOUT)) +
  geom_point(aes(color=DROPOUT), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of dropping out of high school")

# Save plot as png / pdf
# ggsave("Dropout_Probabilities.png")


# Conduct pairwise comparisons
allgroups_compare <- compareGroups(DROPOUT ~ . , data = data)
allgroups_table <- createTable(allgroups_compare, show.ratio=TRUE)
summary(allgroups_compare)
plot(allgroups_compare)

socprobs_compare <- compareGroups(DROPOUT ~ . , data = data, subset = SOCPROB == "Yes 9th grade social problems")
createTable(socprobs_compare)

compareGroups(DROPOUT ~ . , data = data, subset = SOCPROB == "No 9th grade social problems")


t.test(data$DROPOUT, data$ADDSC)

chisq.test(data$REPEAT, data$DROPOUT)
chisq.test(data$SOCPROB, data$DROPOUT)

