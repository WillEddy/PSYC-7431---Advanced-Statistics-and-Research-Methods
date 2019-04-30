library(psych)

IBM <- read.csv ("EDDY_IBM_HR_subset2.csv")
colnames(IBM) <- c("Attrition","NoveltySeeking")

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

