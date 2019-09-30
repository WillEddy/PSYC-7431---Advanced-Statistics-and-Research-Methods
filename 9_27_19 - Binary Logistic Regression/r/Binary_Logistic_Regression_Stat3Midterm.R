library(rstudioapi); library(psych); library(ISLR); 
# library(stringr); library(lubridate); library(tidyverse); library(data.table); library(corrplot); 

# Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
DATA <- read.table("../data/Log-06.dat")

# Name columns
colnames(DATA) <- c("SOCPROB","REPEAT","ADDSC","DROPOUT")

# Label variable values - this also sets them as dichotomous variables
DATA$SOCPROB <- factor(DATA$SOCPROB,
                    levels = c(0,1),
                    labels = c("Yes 9th grade social problems", "No 9th grade social problems"))

DATA$REPEAT <- factor(DATA$REPEAT,
                       levels = c(0,1),
                       labels = c("Didn't repeat a grade", "Repeated a grade"))

DATA$DROPOUT <- factor(DATA$DROPOUT,
                       levels = c(0,1),
                       labels = c("Completed high school", "Dropped out of high school"))


##### Descriptive statistics
descriptives <- describe(DATA)
descriptives
# Write to csv for creating pretty table in report
write.csv(descriptives, "1_descriptives.csv")

###### Frequency table of dichotomous variables across DROPOUT categories
dropoutXsocprobs <-  xtabs(~DROPOUT + SOCPROB, data = DATA)
dropoutXrepeat <-  xtabs(~DROPOUT + REPEAT, data = DATA)
dropoutXsocprobs
dropoutXrepeat
# Write to csv for creating pretty table in report
write.csv(dropoutXsocprobs, "2_dropoutXsocprobs_Classification_table.csv")
write.csv(dropoutXrepeat, "3_dropoutXrepeat_Classification_table.csv")

##### Build model and display results
glm.fit <- glm(DROPOUT ~ .,family=binomial, data = DATA)
glm.fit
glm_summary <- summary(glm.fit)
glm_summary
# Classification tables
glm.steps <- step(glm.fit, DROPOUT~., data=mydata, 
                  family="binomial")
# Classification table
classDF <- data.frame(response = DATA$DROPOUT, predicted = round(fitted(glm.steps),0))


glm.fit.rep <- glm(DROPOUT ~ REPEAT,family=binomial, data = DATA)
glm.fit.rep
glm_summary_rep <- summary(glm.fit.rep)
glm_summary_rep

glm.probs <- fitted(glm.steps)
table(glm.probs>.5, DATA$DROPOUT)

xtabs(~ predicted + response, data = classDF)

# Calculate sensitivity and specificity
2/(1+2) # % dropouts correctly predicted - sensitivity
225/(22+245) # % non-dropouts correctly predicted - specificity

#Calculate true positive and true negative percentages



# Write coefficients to csv for creating pretty table in report
write.csv(glm_summary$coefficients, "4_logistic_regression_coefficients.csv")




# Conduct pairwise comparisons
t.test(DATA$DROPOUT, DATA$ADDSC)
chisq.test(DATA$REPEAT, DATA$DROPOUT)
chisq.test(DATA$SOCPROB, DATA$DROPOUT)

# UNUSED CODE  and SOURCES

# Wald test readout in glm function:
# https://stats.stackexchange.com/questions/60074/wald-test-for-logistic-regression


# Plot distributions
# par(mfrow = c(2,2))
# for( i in 1:8){
#   hist(DATA[,i], main = colnames(DATA)[i],xlab = colnames(DATA)[i], col = 'purple')
# }

# Statquest on logistic regression: 
# https://www.youtube.com/watch?v=C4N3_XJJ-jU
# https://www.youtube.com/watch?v=vN5cNN2-HWE

# Consider using "Deviance residuals clearly exkplained" youtube video
# Odds and log(odds) clearly explained
# Odds and log(odds) ratios clearly explained
# Details of logistic regression clearly explained - several videos
# Saturated models and eviance statistics
# 
