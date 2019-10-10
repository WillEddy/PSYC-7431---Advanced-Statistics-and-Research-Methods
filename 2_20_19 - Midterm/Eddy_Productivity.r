DATA <- read.table("Productivity-09.dat", col.names = c("Productivity",
            "Morale","Fairness","Jobskill","Payannual","Timeout"))

library(psych)

describe(DATA)

CORR_TEST_ALL <- corr.test(DATA)
print(CORR_TEST_ALL, short=FALSE)

SET_COR_PROD <- setCor(Productivity ~ Morale + Fairness + 
                          Jobskill + Payannual + Timeout, data=DATA, alpha=.1)
SET_COR_PROD
summary (SET_COR_PROD)
plot(SET_COR_PROD)

library(ppcor)
spcor(DATA)

#LM_PROD <- lm(Productivity ~ Morale + Fairness + 
#                         Jobskill + Payannual + Timeout, data=DATA, alpha=.1)
#summary(LM_PROD)


#Sources:
# http://personality-project.org/r/r.regression.html
# https://stackoverflow.com/questions/34860970/pearson-correlation-and-significance-of-the-correlation
# http://faculty.cas.usf.edu/mbrannick/regression/Part3/Partials.html

