# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab04A_SOC686.R", echo = TRUE, max.deparse.length = 1000)
#*****REDO OF ASSIGNMENT 3 BASED ON SAMPLE CODE
#Open Log and Read in Data
sink("assign_04A_shepherd.log")
rm(list=ls(all = TRUE))
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
library(foreign)
library(carData)
library(car)
mygss <- read.dta("gsscum7212teach.dta")
#SELECT A SUBSET OF DATA
mydta <- subset(mygss, 
                select=c(mntlhlth, age, sex, race, educ, inc1k))

# Descriptive Statistics Method 02: lapply Loop
# -6 means getting rid of the sixth column (income)
lapply(mydta[,-6], table, useNA="ifany")
lapply(mydta, summary, na.rm=T)
lapply(mydta, mean, na.rm=T) #GETS A WARINING ABOUT MEAN OF NON-NUMERIC DATA

#Create Binary Response Variable
# create binary response variable
mydta$mntlhlthc2 <- ifelse(mydta$mntlhlth > 0, 1, 0)

# create dummy for sex with female as the reference category
mydta$male   <- as.numeric(mydta$sex==1)
# create dummy for sex with male as the reference category
mydta$female <- as.numeric(mydta$sex==2)

# Create Binary Indicator Variables 
# for Multi-Category Variables
# drop missing cases using listwise deletion
mydta$white <- ifelse(mydta$race == 1, 1, 0)
mydta$black <- ifelse(mydta$race == 2, 1, 0)
mydta$other <- ifelse(mydta$race == 3, 1, 0)

View(table(mydta$white, mydta$race, useNA="ifany"))
table(mydta$black, mydta$race, useNA="ifany")
table(mydta$other, mydta$race, useNA="ifany")
# factorize race
# most R functions for estimating regression models
# will create dummy variables for factor variables 
# automatically
mydta$raceNew <- factor(mydta$race, 
                        levels=c(1, 2, 3),
                        labels=c("1white", "2black", "3other"))

usedta <- na.omit(mydta)

# Run Logit 
mylogit <- glm(mntlhlthc2 ~ age + female + black + other + educ + inc1k,
               data=usedta, family=binomial(link="logit"))
(summary(mylogit))



#Close Out
save(useddta, file = "Assignment_04A.rdata")
sink()