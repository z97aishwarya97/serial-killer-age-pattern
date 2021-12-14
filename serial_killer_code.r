#clean workspace
rm(list = ls())

load("D:/R Workspaces/Statistical Theory and Methods/Coursework
Data/killersandmotives.Rdata")
getwd()
#killersandmotives

##DATA CLEANING
#x = 99, motives: Enjoyment or power, Revenge or vigilante justice, Unknown
createsample(99) #865 rows

#view sample data
View(mysample)
write.csv(mysample)
write.table(mysample)

install.packages("xlsx")
library("xlsx")
write.xlsx2(mysample, file = "mysamplebase.xlsx", sheetName = "alldata")

#to display the internal structure of the object
str(mysample)
View(mysample)

#calculate killers whose first kill was before 1900
#year first kill = year born + age of first kill. so remove where year first kill is less than 1900
mysample$YearFirstKill <- (mysample$YearBorn+mysample$AgeFirstKill)
sum(mysample$YearFirstKill<1900) #9
View(mysample)

#remove rows with age=99999 or motive=NA and killers who first killed before 1900
mysample <- mysample[!mysample$ AgeFirstKill == 99999, ]
mysample <- mysample[!is.na(mysample$Motive), ]
mysample <- mysample[!mysample$YearFirstKill < 1900, ]
#removing killerID = 1189 because agefirstkill 44 > agelastkill 43 so career
duration is negative
mysample <- mysample[!mysample$KillerID == 1189, ]

#adding a new column 'Career Duration'
mysample$CareerDuration <- (mysample$AgeLastKill-mysample$AgeFirstKill)

#Reasearch question: does the average age at first murder differ between killers with differnt motives?
str(mysample)

##DATA EXPLORATION
#numerically and graphically summarise the distribution of three variables: age at first kill, age at last kill and career duration
#AgeAtFirstKill
agefirst <- mysample$AgeFirstKill
mean(agefirst)
sd(agefirst)
max(agefirst)
min(agefirst)
#producs sample quantiles corresponding to their given probabilities, range
is 0-1
quantile(agefirst, type = 1)
#boxplot
boxplot(agefirst, xlab = "", ylab = "AgeFirstKill", main = "AgeFirstKill
Boxplot", notch = T, col = "red" ) #(if the plot window is small then throws
an error because of RStudio UI)
#histogram
hist(agefirst, freq = T, main = "Histogram of AgeFirstKill" , xlab = "Age
First Kill", col = "red")

#AgeLastKill
agelast <- mysample$AgeLastKill
mean(agelast)
sd(agelast)
max(agelast)
min(agelast)
#producs sample quantiles correspondnig to their given probabilities, range is 0-1
quantile(agelast, type = 1)
#boxplot
boxplot(agelast, xlab = "", ylab = "AgeLastKill", main = "AgeLastKill
Boxplot", notch = T, col = "blue" ) #(if the plot window is small then
throws an error because of RStudio UI)
#histogram
hist(agelast, freq = T, main = "Histogram of AgeLastKill", xlab =
"AgeLastKill")

#CareerDuration
career <- mysample$CareerDuration
mean(career)
sd(career)
max(career)
min(career)
#producs sample quantiles correspondnig to their given probabilities, range is 0-1
quantile(career, type = 1)
#boxplot
boxplot(career, xlab = "", ylab = "CareerDuration", main = "CareerDuration
Boxplot", notch = T, col = "green" ) #(if the plot window is small then
throws an error because of RStudio UI)
#histogram
hist(career, freq = T, main = "Histogram of CareerDuration", xlab =
"CareerDuration")

#Relationships between the variables
plot(mysample)
plot(mysample$AgeLastKill, mysample$AgeFirstKill, main = "Scatterplot of
AgeFirstKill vs AgeLastKill" ,xlab = "AgeLastKill", ylab = "AgeFirstKill")
abline(lm(mysample$AgeFirstKill ~ mysample$AgeLastKill, data = mysample),
col = "blue", lwd = 2)
plot(mysample$AgeLastKill, mysample$CareerDuration, main = "Scatterplot of
CareerDuration vs AgeLastKill" ,xlab = "AgeLastKill", ylab =
"CareerDuration")
abline(lm(mysample$CareerDuration ~ mysample$AgeLastKill, data = mysample),
col = "blue", lwd = 2)
plot(mysample$AgeFirstKill, mysample$CareerDuration, main = "Scatterplot of
CareerDuration vs AgeFirstKill" ,xlab = "AgeFirstKill", ylab =
"CareerDuration")
abline(lm(mysample$CareerDuration ~ mysample$AgeFirstKill, data = mysample),
col = "blue", lwd = 2)

#finding other relations
plot(mysample$AgeFirstKill, mysample$Sex) #male killers started earlier than female killers
plot(mysample$CareerDuration, mysample$Sex) #male killers more than female

plot(mysample$AgeLastKill, mysample$Race)
plot(mysample$AgeFirstKill, mysample$Race)
plot(mysample$CareerDuration, mysample$Race)

plot(mysample$AgeLastKill, mysample$Motive)
plot(mysample$AgeFirstKill, mysample$Motive)
plot(mysample$CareerDuration, mysample$Motive)

plot(mysample$AgeLastKill, mysample$InsanityPlea)
plot(mysample$AgeFirstKill, mysample$InsanityPlea)
plot(mysample$CareerDuration, mysample$InsanityPlea)

levels(mysample$Race)
levels(mysample$Sex)
levels(mysample$Sentence)
levels(mysample$Motive)
levels(mysample$InsanityPlea)

#correlation co-efficients
cor(mysample$AgeLastKill, mysample$AgeFirstKill) #0.7637381
cor(mysample$AgeLastKill, mysample$CareerDuration) #0.4937577
cor(mysample$AgeFirstKill, mysample$CareerDuration) #-0.1842478
#positive abd close to 1 - strong positive linear relation
#weak positive linear relation
#weak negative linear relation

##MODELLING
#Based on graphical summaries propose distributions that might be used to model the three variables
#estimating the mean and standard deviation then plotting density curve on top of histogram
mean(mysample$AgeFirstKill) #30.42
mean(mysample$AgeLastKill) #35.16
mean(mysample$CareerDuration) #4.74

sd(mysample$AgeFirstKill) #8.67
sd(mysample$AgeLastKill) #9.8
sd(mysample$CareerDuration) #6.44

#Type of variable
#AgeFirstKill - Continuous variable
#AgeLastKill - Continuous variable
#CareerDuration - Continuous variable

hist(mysample$AgeFirstKill, freq = FALSE, main = "Histogram of AgeFirstKill"
, xlab = "Age First Kill") #freq is set to false so that density curve is comparable
#similar bell-shaped curve to a normal distribution
#thinking of age as a random variable X we can propose a density function, in this case a normal distibution would be appropriate
#not possible to write down the density function so using tables we get
help(pnorm)
pnorm(10, mean = 30.42, sd = 8.67)
pnorm(20, mean = 30.42, sd = 8.67)
pnorm(30, mean = 30.42, sd = 8.67)#0.4807 - approximately 48.07% chance that a serial killer starts before the age of 30
pnorm(40, mean = 30.42, sd = 8.67)
pnorm(70, mean = 30.42, sd = 8.67)
#density curve
x <- seq(from = min(mysample$AgeFirstKill), to = max(mysample$AgeFirstKill),
by = 0.1)
lines(x, dnorm(x, mean = 30.42, sd = 8.67), lwd = 2, col = 'blue')
install.packages("e1071")
library("e1071")
skewness(mysample$AgeFirstKill)

hist(mysample$AgeLastKill, freq = F, main = "Histogram of AgeLastKill", xlab
= "Age Last Kill")
#bell curve shaped so normal distribution, skewness is 0
#density curve
x <- seq(from = min(mysample$AgeLastKill), to = max(mysample$AgeLastKill),
by = 0.1)
lines(x, dnorm(x, mean = 35.16, sd = 9.8), lwd = 2, col = 'blue')
skewness(mysample$AgeLastKill)

hist(mysample$CareerDuration, freq = F, main = "Histogram of Career
Duration", xlab = "Career Duration")
#skewness is the measure of asymmetry of a histogram, direction of skewness is to the tail, positive skewness - tail is on the right side and negative skewness - tail is on the left side
#positively skewed with a long tail to right, non-negative sample therefore consider an exponential distribution
#exponential distribution is very often used to model time-to-event variables, speciically when an event occurs randomly and at a constant rate of lambda events per unit time
#density curve
x <- seq(from = min(mysample$CareerDuration), to =
max(mysample$CareerDuration), by = 0.1)
lines(x, dexp(x, rate = 1/9), lwd = 2, col = 'blue')
skewness(mysample$CareerDuration)

##ESTIMATION
#estimation of parameters of models (normal and exponential) to confirm our assumptions from the shape of histogram
#MOM is the simplest but produces a biased result while max likelihood produces an unbiasaed result
AFK <- mysample$AgeFirstKill
#log likelihood function
afk_log_lik_gaussian <- function(mu,sigma) {
 -sum(dnorm(AFK, mean=mu, sd=sigma, log=TRUE))
}
library(stats4)
gaussian_fit <- mle(afk_log_lik_gaussian,
 start=list(mu=1, sigma=0.1),
 method="L-BFGS-B")
summary(gaussian_fit)
ALK <- mysample$AgeLastKill
alk_log_lik_gaussian <- function(mu,sigma) {
 -sum(dnorm(ALK, mean=mu, sd=sigma, log=TRUE))
}
library(stats4)
gaussian_fit <- mle(alk_log_lik_gaussian,
 start=list(mu=2, sigma=0.2),
 method="L-BFGS-B")
summary(gaussian_fit)

CD <- mysample$CareerDuration
lklh.exp<- function(CD, theta) theta*exp(-theta*CD)
log.lklh.exp <- function(CD, theta) {
 -sum(log(theta)-theta*CD)
}
result <- optim(par = 2, log.lklh.exp, CD = CD, method = "Brent", lower = 0,
upper = 3)
theta <- result$par
theta
1/theta
#theta is lambda = 0.210737582
#1/lambda = mean = 4.745238

library(MASS)
fitdistr(CD, "Exponential")

##TESTING HYPOTHESIS
#three motives are enjoyment or power, vigilante, unknown
#numerically summarise age at first kill for the motives and check it is normally distributed
#Perform hypothesis testing for each motive for wether mean age at first kill is 27 years
table(mysample$Motive)

enj_pow_kill <- mysample[mysample$Motive =="Enjoyment or power",
"AgeFirstKill"]
mean(enj_pow_kill)
sd(enj_pow_kill)

vig_kill <- mysample[mysample$Motive == "Revenge or vigilante justice",
"AgeFirstKill"]
mean(vig_kill)
sd(vig_kill)

unknown_kill = mysample[mysample$Motive == "Unknown", "AgeFirstKill"]
mean(unknown_kill)
sd(unknown_kill)

hist(enj_pow_kill, freq = F, main = "Histogram of AgeFirstKill for Enjoyment
or Power as motive",xlab = "AgeFirstKill, Motive: Enjoyment or Power")
a <- seq(from = min(enj_pow_kill), to = max(enj_pow_kill), by = 0.1)
lines(a, dnorm(a, mean = 30.50427, sd = 8.439555), lwd = 2, col = 'blue')
#not skewed

hist(vig_kill, freq = F, main = "Histogram of AgeFirstKill for Revenge or
Vigilante Justice as motive", xlab = "AgeFirstKill, Motive: Revenge or
Vigilante Justice")
b <- seq(from = min(vig_kill), to = max(vig_kill), by = 0.1)
lines(b, dnorm(b, mean = 29.85246, sd = 9.608739), lwd = 2, col = 'blue')
#right skewed

hist(unknown_kill, freq = F, main = "Histogram of AgeFirstKill for Unknown
motive", xlab = "AgeFirstKill, Motive: Unknown")
c <- seq(from = min(unknown_kill), to = max(unknown_kill), by = 0.1)
lines(c, dnorm(c, mean = 29.97403, sd = 9.892812), lwd = 2, col = 'blue')
#right skewed

#the mean and sd we have above are only for the sample and might not be true for the entire population. To make an inference for a large population we are doing the following
#you don't have all the data at times because it might be too expensive of difficult to collect that
#when p value is low then we reject the hypothesis
#previous reasearch has suggested that mean = 27 and sigma sqr. = 74
#size of sample is greater than 30 so large sample then clt tells us we don't need to worry about if the data normally distributed
t.test(enj_pow_kill, mu = 27)
t.test(vig_kill, mu = 27)
t.test(unknown_kill, mu = 27)

install.packages("BSDA")
library("BSDA")
74^0.5
z.test(enj_pow_kill, mu = 27, sigma.x = 8.602325)
z.test(vig_kill, mu = 27, sigma.x = 8.602325)
z.test(unknown_kill, mu = 27, sigma.x = 8.602325)
#for a large sample we have a higher degree of freedom and t-test approaches the normal disribution the difference between t test and z test is negligible

help(t.test)

##COMPARISON OF POPULATIONS
#hypothesis to check if mean age at first kill differs between motives
#enj_pow_kill vs vig_kill
t.test(enj_pow_kill, vig_kill)
z.test(enj_pow_kill, vig_kill, sigma.x = 8.602325, sigma.y = 8.602325)

#vig_kill vs unknown_kill
t.test(vig_kill, unknown_kill)
z.test(vig_kill, unknown_kill, sigma.x = 8.602325, sigma.y = 8.602325)

#enj_pow_kill vs unknown_kill
t.test(enj_pow_kill, unknown_kill)
z.test(enj_pow_kill, unknown_kill, sigma.x = 8.602325, sigma.y = 8.602325)