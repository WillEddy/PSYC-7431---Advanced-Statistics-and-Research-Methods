# Author: William Eddy, M.A. Candidate East Carolina University, william.sebastian.eddy@gmail.com
# Alternative name provided by Professor Poopypants algorithm: Booger Burger? Go Poopypants.

# For reproduction purposes, I have included installation commands for all required packages.

##########
##
## Use these packages
##
##########
library(rstudioapi); library(jtools); library(psych); library(interactions)


## Set working directiory where this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
data <- read.table("../data/P_cuulong-06.dat")

# Label columns
colnames(data) <- c("Catch","Priapam","Koleos") # ,"TIME x GROUP"


# ________________________________________________________________________________________________________________
# Multiple linear regression analysis
# ________________________________________________________________________________________________________________


# Descriptive statistics and correlations
describe(data)
print(corr.test(data), short=FALSE)

# Conduct multiple regression predicting catch from other 2
Reg1 <- lm(Catch ~ Priapam + Koleos, data = data)

# "part.r" is semipartial correlations
summ(Reg1, confint = TRUE, part.corr = TRUE)
# Include standardized slopes (beta weights)
summ(Reg1, confint = TRUE, part.corr = TRUE, scale = TRUE)


# ________________________________________________________________________________________________________________
# Moderation analysis
# ________________________________________________________________________________________________________________

# Add z-scores to dataset
data$Catch_z <- as.numeric( scale(data$Catch, center = TRUE, scale = TRUE))
data$Priapam_z <- as.numeric( scale(data$Priapam, center = TRUE, scale = TRUE))
data$Koleos_z <- as.numeric( scale(data$Koleos, center = TRUE, scale = TRUE))

# Create interaction term (unused in code below)
data$CatchXKoleos_Z <- data$Catch_z * data$Koleos_z

# Conduct multiple regression predicting catch from other 2 plus the interaction term
Reg2 <- lm(Catch_z ~ Priapam_z * Koleos_z, data = data)
summ(Reg2, confint = TRUE, part.corr = TRUE))

# Use interact_plot from "interactions" package to create plot
interact_plot(Reg2, pred = Priapam_z, modx = Koleos_z, x.label = "Standardized Priapam", y.label = "Standardized Catch", 
              legend.main = "Standardized Koleos")
# Simple slopes analysis
sim_slopes(Reg2, pred = Priapam_z, modx = Koleos_z, johnson_neyman = FALSE, confint = TRUE)



# https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
# library(psycho)

