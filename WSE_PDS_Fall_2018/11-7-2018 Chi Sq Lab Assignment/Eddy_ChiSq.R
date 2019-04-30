
library(psych)


IBM <- as.data.frame(read.csv ("EDDY_IBM_HR_subset2.csv", col.names = c("Attrition","NoveltySeeking")))

######################
#
# Splitting the variable NoveltySeeking at the mean to make a dichotomous variable
#

IBM$NoveltySeekingDichotomous <- cut(IBM$NoveltySeeking, 
                       breaks = c(-Inf, 19.99, Inf), 
                       labels = c("NoveltySeeking", "NotNoveltySeeking"), 
                       right = FALSE)

IBM$Attrition  <- cut(IBM$Attrition, 
                                     breaks = c(-Inf, 1, Inf), 
                                     labels = c("StayedAtIBM", "LeftIBM"), 
                                     right = FALSE)

################

NoveltyXAttrition<- table(IBM$Attrition, IBM$NoveltySeekingDichotomous)

addmargins(NoveltyXAttrition)

NoveltyXAttrition

ChiSQofData <- chisq.test(IBM$Attrition, IBM$NoveltySeekingDichotomous, correct = FALSE)
ChiSQofData

library(epitools)

OddsRatiosofData <- oddsratio(IBM$NoveltySeekingDichotomous, IBM$Attrition)
OddsRatiosofData

IBMsimpleData <- subset(IBM, select = c(Attrition, NoveltySeekingDichotomous))
