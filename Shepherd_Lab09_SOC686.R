# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab09_SOC686.r", echo=T, max.deparse.length=10000)
library(foreign)


#Open Log and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("Shepherd_asgn09.log", split=T)
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

#Run OLS
mntlhlth.model <- lm(formula = mntlhlth ~ age + female + black + other + educ + inc1k, data = nmdta )
(summary(mntlhlth.model))

#TASK 1 Run Bayesian Linear Reg (Non-Informative Pairs)
library(MCMCpack)
mcmc.model = MCMCregress(mntlhlth ~ age + female + black + other + educ + inc1k,
                         data = nmdta, burnin = 1000, mcmc = 10000, thin = 1,seed = 47304,b0 = 0, B0 = 1,marginal.likelihood = "Chib95")
summary(mcmc.model)

#Task 2 Run Confidence Intervals for Coefficients of Educ and Race
confint(mntlhlth.model)

#Interpret results in document

#Task 3 Run Bayesian Linear Regression (Informative Pairs)
mcmc.model2 = MCMCregress(mntlhlth ~ age + female + black + other + educ + inc1k,
                          data = nmdta, burnin = 1000, mcmc = 10000, thin = 1,seed = 47304, 
                          b0 = 2,
                          B0 = .25,
                          marginal.likelihood = "Chib95")
summary(mcmc.model2)

BF = BayesFactor(mcmc.model,mcmc.model2)
BF
#Task 4 Make Predictions for 35-year-old white woman with 16 years education and median income
library(HDInterval)
require(rjags)
library(BEST)
post.mat = as.matrix(mcmc.model)
cut.mat = post.mat[,c("(Intercept)", "age", 'female','black','other','educ','inc1k')]
pred.hyp <- 1*post.mat[,"(Intercept)"]+ 35*post.mat[,'age'] + 1*post.mat[,'female'] +
  0*post.mat[,'black'] + 0*post.mat[,'other'] + 16*post.mat[,'educ'] + median(nmdta$inc1k)*post.mat[,'inc1k']
summary(pred.hyp)
quantile(pred.hyp, c(.025,.5,.975))
#TASK 5 Calculate the difference in Predictions
pred.hyp2 <- 1*post.mat[,"(Intercept)"]+ 35*post.mat[,'age'] + 0*post.mat[,'female'] +
  0*post.mat[,'black'] + 0*post.mat[,'other'] + 16*post.mat[,'educ'] + median(nmdta$inc1k)*post.mat[,'inc1k']
summary(pred.hyp2)
quantile(pred.hyp-pred.hyp2, c(.025,.5,.975))
#Close log
save(nmdta, file = "Assignment_09.rdata")
sink()
