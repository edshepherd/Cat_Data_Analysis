# source("/Users/burrisfaculty/Desktop/DSCode/SOC686/Shepherd_Lab04_SOC686.r", echo=T, max.deparse.length=10000)
library(foreign)
library(carData)
library(Zelig)
#Open Long and read in data
setwd("/Users/burrisfaculty/Desktop/DSCode/SOC686")
sink("Shepherd_asgn04F.log", split=T)
rm(list=ls(all=TRUE))
mygss <- read.dta("gsscum7212teach.dta", convert.factor=F)
library(foreign)
library(carData)
library(Zelig)

#SELECT DATA
useddta <- subset(mygss, 
                select=c(mntlhlth, age, sex, race, educ, inc1k))
#SUMMARIZE USING  lapply instead of tables this time
lapply(useddta[,-6], table, useNA="ifany")
lapply(useddta, summary, na.rm=T)
lapply(useddta, mean, na.rm=T)

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

#Graph Bivariate Scatter Plot
nmdta <- useddta[complete.cases(useddta),] #no missing data
#Create Scatter plot matrix

#scatterplotMatrix(~ mntlhlth + age + sex + race +
                  #  educ + inc1k + mntlhc2,
                 # smooth = list(span = 0.7), data = nmdta)
#scatterplot(nmdta$educ,nmdta$mntlhc2)

#Task 1 Run Logit and Calculate Predicted Probabilities for Within Sample Cases

logit.model <- glm(mntlhc2 ~ age + male + white + black + educ + inc1k, family = binomial(link = 'logit'),
                   data = nmdta)
nmdta$logitpr <- predict(logit.model, type = "response")

summary(nmdta$logitpr)

#Task 2 Calculate the Predicted Probability for a Hypothetical Individual
# 35 year-old white woman with average education and income


z.out <-zelig(mntlhc2 ~ age + female + white + black + educ + inc1k,
              data = nmdta, model = "logit")
x.out <-setx(z.out, age = 35, female = 1, black = 0, white = 1, educ = mean(nmdta$educ), inc1k = mean(nmdta$inc1k))
set.seed(47306)
s.out <- sim(z.out, x = x.out)
summary(s.out)

#Interpret results in document

#Task 3 Calculate the Difference in Predicted Probabilities
x.start <- setx(z.out, age = 35, female = 1, white = 1, black = 0, educ = mean(nmdta$educ), inc1k = mean(nmdta$inc1k) )
x.end <- setx(z.out, age = 35, female = 0, white = 1, black = 0, educ = mean(nmdta$educ), inc1k = mean(nmdta$inc1k))
s.out <- sim(z.out, x = x.start, x1 = x.end)
summary(s.out)
#Interpret results in document

#Task 4 Computer Partial Change/Marginal Effect
#Calculate average marginal effects of education

require(margins)
summary(margins(logit.model))

#Interpret results in document

#Task 5 Plot Predicted Probabilities


plot(effect("age", logit.model,xlevels = list(18:65), 
            given.values = c(black = 1, white = 0, inc1k = median(nmdta$inc1k), educ = mean(nmdta$educ)) ))

save(nmdta, file = "Assignment_04.rdata")
sink()