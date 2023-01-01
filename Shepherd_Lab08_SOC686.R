# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab08_SOC686.r", echo=T, max.deparse.length=10000)
library(foreign)


#Open Log and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("Shepherd_asgn08.log", split=T)
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

#Create Ordinal Response Variable
nmdta$mntlhlthOrd <- cut(nmdta$mntlhlth, breaks = c(0,1,8,15,30), labels = c(1,2,3,4), right = FALSE)

#Used the table functions to verify the correctness of the ordinal variable
table(nmdta$mntlhlthOrd)
table(nmdta$mntlhlth)

#Run Proportional Odds Model
library (MASS)
ordlogit.model1 <- polr(mntlhlthOrd ~ age + male + other + black + educ + inc1k, data = nmdta, method = c("logistic"))
summary(ordlogit.model1)
ordlogit.model2 <- polr(mntlhlthOrd ~ male + other + black  + inc1k, data = nmdta, method = c("logistic"))
summary(ordlogit.model2)

#TASK 1 Calculate Predicted Probabilities for Estimation Sample
ordlogit.pred = predict(ordlogit.model1, type = "probs")
summary(ordlogit.pred)

#TASK 2 Calculated Predicted Probabilities for Hypothetical Cases
#35-year old white female with average education and median income and an otherwise similar male
hyp.person <- data.frame(age = 35, male = 0, black = 0, other = 0, educ = mean(nmdta$educ), inc1k = median(nmdta$inc1k))
hyp.pred = predict(ordlogit.model1, newdata = hyp.person, type = "probs")
hyp.pred
#Interpret results

#TASK 3 Calculate Differences in Predicted Probabilities
library(glm.predict)
valuesw = c(35, 0, 0, 0, mean(nmdta$educ), median(nmdta$inc1k))
valuesw
valuesm = c(35,1,0,0,mean(nmdta$educ),median(nmdta$inc1k))
valuesm
dis.change = dc(ordlogit.model1,values1 = valuesw ,values2 = valuesm,set.seed = 47)
dis.change
#Interpret results
#TASK 4: Calculate AME
library(margins)
summary(margins(ordlogit.model1))

#Interpret results of average marginal effects of education

#TASK 5 Plot the Predicted Probabilities
require(effects)
plot(effect("educ", ordlogit.model1, xlevels = list(educ = 0:20),given.values = c(black = 1, other = 0, age = mean(nmdta$age), educ = mean(nmdta$educ), inc1k = median(nmdta$inc1k))))
#Close log
save(nmdta, file = "Assignment_08.rdata")
sink()
