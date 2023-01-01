# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab02_SOC686.R", echo = TRUE, max.deparse.length = 1000)

#Task 1
sink("assign_02_shepherd.log")
rm(list=ls(all = TRUE))
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
library(foreign)
library(carData)
library(car)
mygss <- read.dta("gsscum7212teach.dta")

#Task 2
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

#TASK 3

#Make Dummy Variables
useddta$female <- as.numeric(useddta$sex == "female")
table(useddta$sex, useddta$female, useNA = c("ifany"))

useddta$nonblack <- as.numeric(useddta$race != 'black')
table(useddta$race, useddta$nonblack, useNA = c("ifany"))

#Drop Missing Data
nmdta <- useddta[complete.cases(useddta),]

#Make Pairwise Scatterplots

scatterplotMatrix(~ mntlhlth + age + sex + race +
                    educ + inc1k,
                  smooth = list(span = 0.7), data = useddta)
                      
#TASK 4
#Run OLS
#usevar <- c("mntlhlth",'age','sex','race','educ','inc1k')
ols.model <- lm(formula = mntlhlth ~ age + female + nonblack + educ + inc1k, data = nmdta )
(summary(ols.model))

#Interpret coefficients of female and nonblack in document

#Task 5
#Predicted Outcomes for Full Estimation Sample
nmdta$mntlhlthpr <- predict(ols.model, type = "response")
summary(nmdta$mntlhlthpr)

#Hypothetical Prediction for 35-year old white female with 20 years of educ
hyp.data <- data.frame( age = 35, nonblack = 1, female = 1,educ = 20, inc1k = 60)
pr = predict(ols.model, hyp.data, interval = "confidence")


#Close Out
save(useddta, file = "Assignment_02.rdata")
sink()




