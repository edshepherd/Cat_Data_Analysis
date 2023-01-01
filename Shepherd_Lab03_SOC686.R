# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab03_SOC686.R", echo = TRUE, max.deparse.length = 1000)

#Open Log and Read in Data
sink("assign_03_shepherd.log")
rm(list=ls(all = TRUE))
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
library(foreign)
library(carData)
library(car)
mygss <- read.dta("gsscum7212teach.dta")

#Select, Tabulate and Summarize Variables for Analysis
usevar <- c("mntlhlth",'age','sex','race','educ','inc1k')
useddta <- mygss[usevar]

table(useddta$mntlhlth, useNA = c("ifany"))
summary(useddta$mntlhlth)
table(useddta$age, useNA = c("ifany"))
summary(useddta$age)
table(useddta$sex, useNA = c("ifany"))
summary(useddta$sex)
table(useddta$race, useNA = c("ifany"))
summary(useddta$race)
table(useddta$educ, useNA = c("ifany"))
summary(useddta$educ)
table(useddta$inc1k, useNA = c("ifany"))
summary(useddta$inc1k)

#TASK 1: Create Binary Response Variables
# 1 = poor mental health mntlhl > 0
useddta$mntlhc2 <- ifelse(useddta$mntlhlth > 0, 1, 0)

#Create dummy variables female (male = 0)
useddta$female <- as.numeric(useddta$sex == "female")



#Task 2: Create Binary Indicator Variables for Multi-Category Nomial Variables
#Create dummy variables for race
useddta$raceF <- factor(useddta$race,
                        levels = c(1,2,3,NA),
                        labels = c("1white", "2black", "3other"))
table(useddta$raceF,useddta$race, useNA = 'ifany')
useddta$nonwhite <- as.numeric(useddta$race != 'white')
# In document discuss how many can be used in the regression model
# In document discuss how to interpret the coefficient

#Task 3 Graph Bivariate Scatterplot
#Drop Missing Cases
nmdta <- useddta[complete.cases(useddta),]

#Create Scatter plot matrix
scatterplotMatrix(~ mntlhlth + age + sex + race +
                   educ + inc1k + mntlhc2,
                  smooth = list(span = 0.7), data = useddta)
scatterplot(useddta$educ,useddta$mntlhc2)
#Describe scatterplot

#Task 4 Run Logit
logit.model <- glm(mntlhc2 ~ age + female + nonwhite + educ + inc1k, family = binomial(link = 'logit'),
                   data = useddta)
coef(logit.model)
summary(logit.model)
#Task 5 Produce and Interpret Odds Ratio
exp(logit.model$coefficients)
#Create confidence intervals ... for fun
exp(confint(logit.model))

#Close Out
save(useddta, file = "Assignment_03.rdata")
sink()




