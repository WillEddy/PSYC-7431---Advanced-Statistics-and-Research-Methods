
setwd("C:/R_Files/PSYC-7431---Advanced-Statistics-and-Research-Methods/4_19_19-Kruiskal-Wallis-ANOVA")
DATA <- read.csv("C:/R_Files/PSYC-7431---Advanced-Statistics-and-Research-Methods/4_19_19-Kruskal-Wallis-ANOVA/DATA/Kruskal-09.dat", sep="")

library(psych)

DATA.Present = subset(DATA, DATA$Group=="1")
DATA.Caged = subset(DATA, DATA$Group=="2")
DATA.Absent = subset(DATA, DATA$Group=="3")

Present_DESCRIP <- describe(DATA.Present)
Caged_DESCRIP <- describe((DATA.Caged))
Absent_DESCRIP <- describe((DATA.Absent))


# Copy table to clipboard
write.table(Present_DESCRIP, "clipboard", sep="\t", row.names=FALSE)

write.table(Caged_DESCRIP, "clipboard", sep="\t", row.names=FALSE)

write.table(Absent_DESCRIP, "clipboard", sep="\t", row.names=FALSE)

# Kruskal-Wallis ANOVA

kruskal.test(DATA$Latency, DATA$Group)

# Pairwise comparisons

DATA.1_2 = DATA[1:43,]
DATA.1_3 = DATA[c(1:22,44:65),]
DATA.2_3 = DATA[23:65,]

kruskal.test(DATA.1_2$Latency, DATA.1_2$Group)

kruskal.test(DATA.1_3$Latency, DATA.1_3$Group)

kruskal.test(DATA.2_3$Latency, DATA.2_3$Group)

