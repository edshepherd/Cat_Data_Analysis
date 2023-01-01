# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab01_SOC686.R", echo = TRUE, max.deparse.length = 1000)

#Task 1
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("assign_01_shepherd.log")
library(foreign)
mygss <- read.dta("gsscum7212teach.dta")

#Task 2
mydata <- subset(mygss, year> 2000)
mydata <- subset(mydata, year<2010)
usevar <- c("mntlhlth",'age','sex','race','educ')
useddta <- mydata[usevar]

#Task 3
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

#Task 4
useddta$college <- as.numeric(useddta$educ>=16)
table(useddta$educ, useddta$college, useNA = c("ifany"))

#Task 5
save(useddta, file = "Assignment_01.rdata")
sink()
