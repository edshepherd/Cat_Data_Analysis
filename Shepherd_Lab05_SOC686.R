# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab05_SOC686.r", echo=T, max.deparse.length=10000)
library(foreign)
library(carData)
library(car)

#Open Log and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("Shepherd_asgn05F.log", split=T)
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
logit.model <- glm(mntlhc2 ~ age + male + other + black + educ + inc1k, family = binomial(link = 'logit'),
                   data = nmdta)
nmdta$logitpr <- predict(logit.model, type = "response")

summary(nmdta$logitpr)

#Task 1
#Wald Test: Effects of age on mental health
HT01 = linearHypothesis(logit.model,c("age = 0"))

HT01
#Interpret Results in Assignment


#Task 2
#Use Score test to see if the effects of race on mental health is 0
noracelogit.model <- update(logit.model, .~. -other - black)
anova(noracelogit.model, logit.model, test = "Rao")
#Interpret Results in Assignment

#Task 3
#Use Wald test education and income  equal to 0
HT02 = linearHypothesis(logit.model, c("educ = 0", ("inc1k = 0")))
HT02
#Interpret Results in Assignment

#Task 4
#Use LR Test for educ = 0 an inc1k = 0

NoEducNoIncLogit.model <- update(logit.model, .~. - educ - inc1k)
summary(NoEducNoIncLogit.model)

HT03 = anova(NoEducNoIncLogit.model, logit.model, test = "Chisq")
HT03
#Interpret Results in Assignment


#Task 5
#Test whether the effects of educ and inc are equal using Wald Test

HT04 = linearHypothesis(logit.model, c("educ = inc1k"))
HT04
#Interpret Results in Assignment

save(nmdta, file = "Assignment_05.rdata")
sink()