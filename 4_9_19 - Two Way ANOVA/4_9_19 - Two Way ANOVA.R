
DATA <- read.table("Buss-09.dat", header=TRUE, col.names = c("Group", "Gender/Sex", "Type of Imagined Infidelity", "Pulse_Increase"))

DATA$Group[DATA$Group==1] <- "Male, Sexual Image"
DATA$Group[DATA$Group==2] <- "Male, Emotional Image"
DATA$Group[DATA$Group==3] <- "Female, Sexual Image"
DATA$Group[DATA$Group==4] <- "Female, Emotional Image"
DATA$Gender.Sex[DATA$Gender.Sex==1] <- "Male"
DATA$Gender.Sex[DATA$Gender.Sex==2] <- "Female"
DATA$Type.of.Imagined.Infidelity[DATA$Type.of.Imagined.Infidelity==1] <- "Sexual"
DATA$Type.of.Imagined.Infidelity[DATA$Type.of.Imagined.Infidelity==2] <- "Emotional"

# View(DATA)

# Descriptive statistics

table(DATA$Gender.Sex, DATA$Type.of.Imagined.Infidelity)

# View mean and SD of Pulse_Increase by group

aggregate(DATA$Pulse_Increase, by=list(DATA$Gender.Sex, DATA$Type.of.Imagined.Infidelity), FUN=mean)

aggregate(DATA$Pulse_Increase, by=list(DATA$Gender.Sex, DATA$Type.of.Imagined.Infidelity), FUN=sd)

# Calculating F_max. Result is <10, and therefore the data has approximately equal variances

(3.947743^2/3.872860^2)

aggregate(DATA$Pulse_Increase, by=list(DATA$Gender.Sex), FUN=mean)

aggregate(DATA$Pulse_Increase, by=list(DATA$Type.of.Imagined.Infidelity), FUN=mean)


#__________________________________________________________________________________________________________

# Omnibus factorial ANOVA

omnibus <-aov(DATA$Pulse_Increase ~ DATA$Gender.Sex*DATA$Type.of.Imagined.Infidelity)
summary(omnibus)

# Simple Main Effects

Gender.Male = subset(DATA, DATA$Gender.Sex=="Male")
Gender.Female = subset(DATA, DATA$Gender.Sex=="Female")

ImaginedInfidelity.Male = aov(Pulse_Increase~Type.of.Imagined.Infidelity, data=Gender.Male)
summary(ImaginedInfidelity.Male)
# eta-squared
31.4/(31.4+2927.7)


ImaginedInfidelity.Female = aov(Pulse_Increase~Type.of.Imagined.Infidelity, data=Gender.Female)
summary(ImaginedInfidelity.Female)
# eta-squared
14.6/(14.6+2866.4)

# Larger eta-squared indicates that Effect of imagined infidelity is greater in Males than Females


Infidelity.Emotional = subset(DATA, DATA$Type.of.Imagined.Infidelity=="Emotional")
Infidelity.Sexual = subset(DATA, DATA$Type.of.Imagined.Infidelity=="Sexual")

Gender.Emotional = aov(Pulse_Increase~Gender.Sex, data=Infidelity.Emotional)
summary(Gender.Emotional)
# eta-squared
5.4/(5.4+2887.6)


Gender.Sexual = aov(Pulse_Increase~Gender.Sex, Infidelity.Sexual)
summary(Gender.Sexual)
# eta-squared
137.6/(137.6+2906.5)

# Larger eta-squared indicates that Effect of gender is greater in sexual than emotional conditions of imagined infidelity

# Other perspective simple main effects

interaction.plot(DATA$Type.of.Imagined.Infidelity, DATA$Gender.Sex, DATA$Pulse_Increase, type="b", 
                 col=c("red","blue"), pch=c(16, 18), main = "Interaction between Gender/sex and Type of Imagined Infidelity",
                 ylab= "Pulse Increase Estimated Marginal Means", xlab = "Imagined Infidelity Type", trace.label =  "Gender/Sex"
                 )


