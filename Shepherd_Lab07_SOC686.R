# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab07_SOC686.r", echo=T, max.deparse.length=10000)
library(foreign)


#Open Log and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("Shepherd_asgn07.log", split=T)
rm(list=ls(all=TRUE))
mygss <- read.dta("gsscum7212teach.dta", convert.factor=F)

#MANAGE DATA AND RUN LOGIT
#SELECT DATA
useddta <- subset(mygss, 
                select=c(mntlhlth, age, sex, race, educ, inc1k))

#Create dummy variables female (male = 0)
useddta$female <- as.numeric(useddta$sex==2)
useddta$male <- as.numeric(useddta$sex == 1)

#Create Binary Indicator Variables for Multi-Category Nomial Variables

useddta$white <- ifelse(useddta$race == 1, 1, 0)
useddta$black <- ifelse(useddta$race == 2, 1, 0)
useddta$other <- ifelse(useddta$race == 3, 1, 0)

nmdta <- useddta[complete.cases(useddta),] #no missing data

#summarize data
summary(nmdta$mntlhlth)
summary(nmdta$inc1k)
summary(nmdta$age)
table(nmdta$female)
table(nmdta$white)

#Task 1 Create Ordinal Response Variable
# 1 number of days is 0
# 2 days 1 to 7
# 3 days 8-14
# 4 days 15+

nmdta$mntlhlthOrd <- cut(nmdta$mntlhlth, breaks = c(0,1,8,15,30), labels = c(1,2,3,4), right = FALSE)
#Used the table functions to verify the correctness of the ordinal variable
table(nmdta$mntlhlthOrd)
table(nmdta$mntlhlth)

#Task 2 Run Proportional Odds Model
library (MASS)
ordlogit.model1 <- polr(mntlhlthOrd ~ age + male + other + black + educ + inc1k, data = nmdta, method = c("logistic"))
summary(ordlogit.model1)
ordlogit.model2 <- polr(mntlhlthOrd ~ male + other + black  + inc1k, data = nmdta, method = c("logistic"))
summary(ordlogit.model2)

#Task 3 Test Hypothesis Using the Likelihood Ratio Test
#Wald Test
library(car)
waldtest = linearHypothesis(ordlogit.model1, c("age = 0","educ = 0" ))
waldtest

#Test the same hypothesis using LR test

lrTest = anova(ordlogit.model2, ordlogit.model1, test = "Chisq")
print(lrTest)

#TASK 4 Test using AIC and BIC
library(stats4)
AIC(ordlogit.model1,ordlogit.model2)

BIC(ordlogit.model1,ordlogit.model2)

#TASK 5 Compare Using Pseudo R-Squared
library(DescTools)

PseudoR2(ordlogit.model1, c("McFadden", "Nagel"))
PseudoR2(ordlogit.model2, c("McFadden", "Nagel"))

#Close log
save(nmdta, file = "Assignment_07.rdata")
sink()
