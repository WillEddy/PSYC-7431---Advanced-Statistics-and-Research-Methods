
library(readr)
DATA <- read_table2("C:/R_Files/PSYC-7431---Advanced-Statistics-and-Research-Methods/3_19_2019 - One Way ANOVA Homework/Buss-09.dat", 
                       col_names = c("group","gender/sex","type of imagined infidelity","pulse_increase"))

#source("http://bioconductor.org/biocLite.R")
#biocLite("multtest")

# Part 0 of Wuensch document

PULSE = factor(DATA$pulse_increase)

View(PULSE)
boxplot(DATA$`type of imagined infidelity` ~ DATA$, col="lightgray")

# Part 1 of Wuensch document

Anv = aov(DATA$`type of imagined infidelity` ~ PULSE)
model.tables(Anv, "means")
summary(Anv)


# Part 2 of Wuensch document

oneway.test(DATA$`type of imagined infidelity` ~ PULSE)

# Part 3 of Wuensch document
library(gmodels)

fit.contrast(Anv, PULSE, c(1, -2, 1), df=T)
fit.contrast(Anv, PULSE, c(1, 0, -1), df=T)



 # Trying to get regwq output
DAT$`type of imagined infidelity` <- as.numeric(DAT$`type of imagined infidelity`)
#DAT$pulse_increase <- as.factor(DAT$pulse_increase)
library(mutoss)
REGWQ_OUTPUT <- regwq(`type of imagined infidelity` ~ PULSE, data = DATA, alpha = 0.05)

# soMETHING DIFFERENT

