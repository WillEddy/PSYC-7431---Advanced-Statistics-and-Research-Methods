# Author: William Eddy, M.A. Candidate East Carolina University, william.sebastian.eddy@gmail.com
# Alternative name provided by Professor Poopypants algorithm: Booger Burger? Go Poopypants.

# For reproduction purposes, I have included installation commands for all required packages.

##########
##
## Use these packages
##
##########
library(ggplot2); library(cowplot); library(rstudioapi); library(compareGroups)
library(tidyverse); library(psych); library(mdscore); library(car)
library(stargazer)


## Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
data <- read.table("../data/Potthoff-06.dat")

#######
# Prepare dataset fot analysis
#######


# Name columns
colnames(data) <- c("GROUP","TIME","EXAM","TIME x GROUP")

# Label GROUP values - this also sets them as dichotomous variables (factors in r)
data$GROUP <- factor(data$GROUP,
                       levels = c(0,1),
                       labels = c("0 - Control", "1 - Experimental"))

# Create z-scores from the continuous variables
data$TIME_z <- scale(data$TIME, center = TRUE, scale = TRUE)
data$EXAM_z <- scale(data$EXAM, center = TRUE, scale = TRUE)
data$TIMExGROUP_z <- scale(data$`TIME x GROUP`, center = TRUE, scale = TRUE)


write.csv(data, "Potthoff-06.csv", row.names = FALSE)



#######
# Create interaction plot of regression lines
#######
ggplot(data, aes(x=TIME_z, y=EXAM_z, col=GROUP)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)


#######
# Conduct tests of coincidence
#######

FULL_MODEL_1 <- lm(TIME_z~EXAM_z + TIMExGROUP_z, data)
FULL_MODEL_2 <- lm(TIME_z~EXAM_z, data)


var.test(FULL_MODEL_1, FULL_MODEL_2, alternative = "two.sided")

var.test(CONTROL_MODEL_1, EXPERIMENTAL_MODEL_1, alternative = "two.sided")

?var.test





library(lsmeans)

FullModel_slopes <- lstrends(FULL_MODEL_1, Exam_z, var="")

pairs(FULL_MODEL_1)


anova(FULL_MODEL_1, FULL_MODEL_2)

anova(CONTROL_MODEL_1, EXPERIMENTAL_MODEL_1)

processR::fit2alpha(FULL_MODEL_1, 3)


##################################################################
# UNUSED CODE
# 
# # Separate 2 groups
CONTROL_GROUP <- subset(data, GROUP == "0 - Control")
EXPERIMENTAL_GROUP <- subset(data, GROUP == "1 - Experimental")

# Create regression models for each group
CONTROL_MODEL_1 <- lm(TIME_z~EXAM_z, CONTROL_GROUP)
EXPERIMENTAL_MODEL_1 <- lm(TIME_z~EXAM_z, EXPERIMENTAL_GROUP)


# 
# # Plot
# 
# ggplot(data, aes(x = TIME, y = EXAM)) + 
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")
# 


# stargazer(CONTROL_MODEL_1, EXPERIMENTAL_MODEL_1,type="text", 
#           column.labels = c("Main Effects", "Interaction"), 
#           intercept.bottom = FALSE, 
#           single.row=FALSE,     
#           notes.append = FALSE, 
#           header=FALSE) 
