# Author: William Eddy, M.A. Candidate East Carolina University, william.sebastian.eddy@gmail.com

##########
##
## Assignment
##
##########
# Import the data into R,
# compute and report the correlation between apgar score (variable: apgar)
# and the gestational age of the child in weeks (variable: gestat)*.
# Be sure to include your R syntax and output with your answer.

library(rstudioapi); library(psych)

## Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Import raw data
data <- read.csv("../data/apgar-1.csv")

## Calculate requested correlation
corr.test(data$apgar, data$gestat)