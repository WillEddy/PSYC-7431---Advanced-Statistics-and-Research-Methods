library(psych)

DATA <- read.csv("IBM_HR_TRIMMED_3.csv", row.names=1)
DAT <- DATA [,c(12,13,19)]

# Convert character strings to numeric for correlation table function
DAT$OverTime <- as.numeric(DAT$OverTime)

# Produce correlation table with p values
library(ppcor)
COR_CONT <- spcor(DAT)
COR_CONT

# Model with interaction between categorical and predictor variables
AOV_W <- aov(PercentSalaryHike~TrainingTimesLastYear*OverTime,DAT)

print(summary(AOV_W))

plot(AOV_W)

# Model without interaction between categorical and predictor variables
AOV_WO <- aov(PercentSalaryHike~TrainingTimesLastYear+OverTime,DAT)
print(summary(AOV_WO))


print(anova(AOV_W,AOV_WO))


# Effect sizes - part of this assignment?
library(effects)
effect("OverTime", AOV_WO)

t.test(DAT$PercentSalaryHike,DAT$Overtime)
t.test(DAT$TrainingTimesLastYear,DAT$Overtime)

# Sources utilized:
# https://www.tutorialspoint.com/r/r_analysis_of_covariance.htm
# https://cran.r-project.org/web/packages/fastDummies/vignettes/making-dummy-variables.html

# Code I didn't end up using:
#
# Unnecessary dummy creation
#DATdummy <- dummy.code(DAT$OverTime)
#DAT2 <- cbind(DAT,DATdummy)
#View(DAT2)