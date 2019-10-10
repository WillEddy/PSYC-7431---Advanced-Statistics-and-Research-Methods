Cyberloaf <- Cyberloaf_Consc_Age <- read.csv("C:/R_Files/PSYC-7431---Advanced-Statistics-and-Research-Methods/1_30_2019 - Correlation Regression WithR/Cyberloaf_Consc_Age.txt", sep="")

library( car )
scatterplot(Cyberloafing ~ Conscientiousness, data = Cyberloaf, smooth = FALSE)

fit <- lm(Cyberloafing~Conscientiousness+Age,data=Cyberloaf)
summary(fit)