# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab06_SOC686.r", echo=T, max.deparse.length=10000)
library(foreign)


#Open Log and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("Shepherd_asgn06.log", split=T)
rm(list=ls(all=TRUE))
mygss <- read.dta("gsscum7212teach.dta", convert.factor=F)

#MANAGE DATA AND RUN LOGIT
#SELECT DATA
useddta <- subset(mygss, 
                select=c(mntlhlth, age, sex, race, educ, inc1k))

#Create Binary Response Variables
# 1 = poor mental health mntlhl > 0
useddta$mntlhc2 <- ifelse(useddta$mntlhlth > 0, 1, 0)

#Create dummy variables female (male = 0)
useddta$female <- as.numeric(useddta$sex==2)
useddta$male <- as.numeric(useddta$sex == 1)

#Create Binary Indicator Variables for Multi-Category Nomial Variables

useddta$white <- ifelse(useddta$race == 1, 1, 0)
useddta$black <- ifelse(useddta$race == 2, 1, 0)
useddta$other <- ifelse(useddta$race == 3, 1, 0)

nmdta <- useddta[complete.cases(useddta),] #no missing data

#summarize data
summary(useddta$mntlhlth)
summary(useddta$inc1k)
summary(useddta$age)
table(useddta$female)
table(useddta$white)
# Create 2 logit models
logit.model <- glm(mntlhc2 ~ age + male + other + black + educ + inc1k, family = binomial(link = 'logit'),
                   data = nmdta)
logit.model2 <- glm(mntlhc2 ~ male + other + black + educ + inc1k, family = binomial(link = 'logit'),
                    data = nmdta)
logit.model3 <- glm(mntlhc2 ~ male + other + black + inc1k, family = binomial(link = 'logit'),
                    data = nmdta)
#Task 1
#Compare using AIC
library(stats4)
AIC(logit.model, logit.model2, logit.model3)

#Interpret results in document

#Task 2
#Compare Using BIC
BIC(logit.model,logit.model2, logit.model3)

#Interpret results in document

#Task 3 and 4
#Compare using McFadden's and Tjur's

library(DescTools)

PseudoR2(logit.model, c("McFadden", "Tjur"))
PseudoR2(logit.model2, c("McFadden", "Tjur"))
PseudoR2(logit.model3, c("McFadden", "Tjur"))


#Task 5
#Compare using H-S GOF
library(generalhoslem)
HL1 = logitgof(nmdta$mntlhc2, fitted(logit.model), g = 10)
HL1

HL2 = (logitgof(nmdta$mntlhc2, fitted(logit.model2), g = 10))
HL2

HL3 = (logitgof(nmdta$mntlhc2, fitted(logit.model3), g = 10))
HL3
save(nmdta, file = "Assignment_06.rdata")
sink()
