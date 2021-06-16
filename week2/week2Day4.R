library(tidyverse)
#Intro to Stat with Randomization and Simulation 
#2.2
#Guided Practice 2.2 If we don't think the side of the room a person sits on in
#class is related to whether the person owns an Apple product, what assumption are
#we making about the relationship between these two variables?

#2.5
#Guided Practice 2.5 What is the difference in promotion rates between the two
#simulated groups in Table 2.2? How does this compare to the observed difference
#29.2% from the actual study?

#Chapter 3
#3.2
#Guided Practice 3.2 Estimate the standard error of ^p = 0.44 using Equation (3.1). Because p is unknown and the standard error is for a confidence interval,
#use ^p in place of p.

#Chapter 9 
#9.2
#Question 9.2. In Chapter 13 we will present a statistical test for testing if
#there is a difference between the patients that received the active magnets and
#the patients that received the inactive placebo in terms of the expected value
#of the variable that measures the change. The test statist for this problem is
#taken to be
magnets <- read_csv("magnets.csv")
#Our goal is to investigate the sampling distribution of this statistic in a case where
#both expectations are equal to each other and to compare this distribution to
#the observed value of the statistic.
#1. Assume that the expectation of the measurement is equal to 3.5, regardless of what the type of treatment that the patient received. We take the
#standard deviation of the measurement for patients the receives an active
#magnet to be equal to 3 and for those that received the inactive placebo
#we take it to be equal to 1.5. Assume that the distribution of the measurements is Normal and there are 29 patients in the first group and 21 in the
#second. Find the interval that contains 95% of the sampling distribution
#of the statistic.


#2. Does the observed value of the statistic, computed for the data frame
#"magnets", falls inside or outside of the interval that is computed in 1?
x1avg <- mean(magnets$change[1:29])
x2avg <- mean(magnets$change[30:50])
x1var <- var(magnets$change[1:29])
x2var <- var(magnets$change[30:50])
(x1avg-x2avg)/sqrt(x1var/29 + x2var/21)
#5.985601