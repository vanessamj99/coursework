library(tidyverse)
pop2 <- read_csv("pop2.csv")
#chapter 7
#1. Compute the population average of the variable "bmi".
mean(pop2$bmi)
#24.984446
#2. Compute the population standard deviation of the variable "bmi".
sd(pop2$bmi)
#4.188511
#3. Compute the expectation of the sampling distribution for the sample average of the variable.
pop2Bar <- rep(0,100000)
for(i in 1:100000){
  samp <- sample(pop2$bmi,150)
  pop2Bar[i]<- mean(samp)
}
mean(pop2Bar)
#24.98547
#4. Compute the standard deviation of the sampling distribution for the sample average of the variable.
sd(pop2Bar)
#0.3420797
#5. Identify, using simulations, the central region that contains 80% of the
#sampling distribution of the sample average.
quantile(pop2Bar,c(0.20,0.80))
#   20%       80%
# 24.69860  25.27274
#6. Identify, using the Central Limit Theorem, an approximation of the central region that contains 80% of the sampling distribution of the sample
#average
qnorm(c(0.20,0.80),mean(pop2Bar),sd(pop2Bar))
#24.69757     25.27337


#chapter 9
#1. What is the sample average of the change in score between the patient's
#rating before the application of the device and the rating after the application?
magnets <- read_csv("magnets.csv")
summary(magnets)
#3.5
#2. Is the variable "active" a factor or a numeric variable?
summary(magnets)
#is it an active variable
#3. Compute the average value of the variable "change" for the patients that
#received and active magnet and average value for those that received an
#inactive placebo. (Hint: Notice that the first 29 patients received an active
#magnet and the last 21 patients received an inactive placebo. The subsequence of the first 29 values of the given variables can be obtained via
#the expression "change[1:29]" and the last 21 vales are obtained via the
#expression "change[30:50]".)
mean(magnets$change[1:29])
#5.241379
mean(magnets$change[30:50])
#1.095238
#4. Compute the sample standard deviation of the variable "change" for the
#patients that received and active magnet and the sample standard deviation for those that received an inactive placebo.
sd(magnets$change[1:29])
#3.236568
sd(magnets$change[30:50])
#1.578124
#5. Produce a boxplot of the variable "change" for the patients that received
#and active magnet and for patients that received an inactive placebo.
#What is the number of outliers in each subsequence?
boxplot(magnets$change[1:29])
boxplot(magnets$change[30:50])
#outliers are 3,4, and 5

#Chapter 10

#10.1
#1. Simulate the sampling distribution of average and the median of a sample
#of size n = 100 from the Normal(3, 2) distribution. Compute the expectation and the variance of the sample average and of the sample median.
#Which of the two estimators has a smaller mean square error?
sig <- sqrt(2)
for(i in 1:10^5){
  x <- rnorm(100,3,sig)
  xmean <-mean(x)
  xmedian <- median(x)
}
mean(xmean)
mean(xmedian)
var(xmean)
var(xmedian)
#2. Simulate the sampling distribution of average and the median of a sample
#of size n = 100 from the Uniform(0.5, 5.5) distribution. Compute the
#expectation and the variance of the sample average and of the sample
#median. Which of the two estimators has a smaller mean square error?
xmean <- rep(0,10^5)
xmedian <-rep(0,10^5)

for(i in 1:10^5){
  x <- runif(100,0.5,5.5)
  xmean[i] <-mean(x)
  xmedian[i] <-median(x)
}
mean(xmean)
mean(xmedian)
var(xmean)
var(xmedian)
#10.2
#1. Compute the proportion in the sample of those with a high level of blood
#pressure16
ex2 <-read.csv("ex2.csv")
summary(ex2)

#2. Compute the proportion in the population of those with a high level of
#blood pressure.
pop2 <-read.csv("pop2.csv")
mean(pop2$group == "HIGH")
#3. Simulate the sampling distribution of the sample proportion and compute
#its expectation.
p <- rep(0,10^5)
for(i in 1:10^5){
  x <- sample(pop2$group,150)
  p <-mean(x == "HIGH")
}
mean(p)
#4. Compute the variance of the sample proportion.
var(p)
#5. It is proposed in Section 10.5 that the variance of the sample proportion
#is Var(P^) = p(1 ??? p)/n, where p is the probability of the event (having a
#high blood pressure in our case) and n is the sample size (n = 150 in our
#case). Examine this proposal in the current setting

p <-mean(pop2$group=="HIGH")
p*(1-p)/150