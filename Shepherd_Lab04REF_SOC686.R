# source("D:/teach/bsuteachnow/soc686/asgn/Shepherd_Lab04REF.r", echo=T, max.deparse.length=10000)
#START STARTER CODE FROM CANVAS
# remove everything (objects) in the working environment
rm(list=ls(all=TRUE))

# set up working directory, open log file, and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("asgn05-binReg02.log", split=T)

library(foreign)
mygss <- read.dta("gsscum7212teach.dta", convert.factor=F)

# select, tabulate, and summarize variables for analysis
mydta <- subset(mygss, 
                select=c(mntlhlth, age, sex, race, educ, inc1k))

# Descriptive Statistics Method 02: lapply Loop
# -6 means getting rid of the sixth column (income)
lapply(mydta[,-6], table, useNA="ifany")
lapply(mydta, summary, na.rm=T)
lapply(mydta, mean, na.rm=T)

# Create Binary Response Variable
# create binary response variable
mydta$mntlhlthc2 <- ifelse(mydta$mntlhlth > 0, 1, 0)

# how about the following coding? is it that different?
# mydta$mntlhlthc2a <- ifelse(mydta$mntlhlth != 0, 1, 0)

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

table(mydta$white, mydta$race, useNA="ifany")
table(mydta$black, mydta$race, useNA="ifany")
table(mydta$other, mydta$race, useNA="ifany")

# factorize race
# most R functions for estimating regression models
# will create dummy variables for factor variables 
# automatically
mydta$raceNew <- factor(mydta$race, 
                        levels=c(1, 2, 3),
                        labels=c("1white", "2black", "3other"))

usedta <- mydta[complete.cases(mydta),]
# or we can da
# usedta <- na.omit(mydta)

# Run Logit 
mylogit <- glm(mntlhlthc2 ~ age + female + black + other + educ + inc1k,
               data=usedta, family=binomial(link="logit"))
summary(mylogit)

# Produce and Interpret Odds Ratio (Coefficients)
exp(mylogit$coefficients)
exp(confint(mylogit))

# Calculate Predicted Probabilities for Within-Sample Cases
usedta$logitpr <- predict(mylogit, type="response")
summary(usedta$logitpr)
#END STARTER CODE FROM CANVAS

#BEGIN ASSIGNMENT 4


sink()