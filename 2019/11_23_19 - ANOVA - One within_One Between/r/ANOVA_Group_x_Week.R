# Author: William Eddy, M.A. Candidate East Carolina University, william.sebastian.eddy@gmail.com

# For reproduction purposes, I have included installation commands for all required packages.

##########
##
## Use these packages
##
##########
#library(ggplot2); library(cowplot); library(rstudioapi); library(compareGroups)
#library(tidyverse); library(psych); library(mdscore); library(car)
#library(stargazer)


## Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
data <- read.table("../data/Insects-06.dat")

#######
# Prepare dataset for analysis
#######

# Name columns
colnames(data) <- c("GROUP","WEEK1","WEEK2","WEEK3", "WEEK4")


aov.week1= aov(GROUP~WEEK1,data=data)
summary(aov.week1)




# Label Groups
data$GROUP <- factor(data$GROUP,
                     levels = c(1,2,3),
                     labels = c("Chemical", "Microbial", "Predator"))



# Analysis of Wicklund Data with missing values - FROM HOWELL DOCUMENT
#data <- read.table(file.choose(), header = T)
attach(data)
Time = factor(Time)
Group = factor(Group)
Subj = factor(Subj)
library(nlme) 
model <- lme(dv ~ Time + Group + Time*Group,  random = ~1 | Subj)
#model2 <- update(model, correlation = corCompSymm(.388,form = ~1 | Subj))
# This line above leads to weird results and I don't know why.
summary(model)
anova(model)
# This model is very close to the one produced by SAS using compound #symmetry, # when it comes to F values, and the log likelihood is the same. But the AIC # and BIC are quite different. The StDev for the Random Effects are the same # when squared. The coefficients are different because R uses the first level # as the base, whereas SAS uses the last.
