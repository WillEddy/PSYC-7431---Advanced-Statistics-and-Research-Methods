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