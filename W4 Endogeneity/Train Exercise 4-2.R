library(ggplot2)
library(magrittr)
library(dplyr)
install.packages("lmtest")

source("http://bit.ly/theme_pub")
theme_set(theme_pub())

exer42 <- read.csv('C:/Users/peter/OneDrive/Econometrics/TrainExer42.csv', header = TRUE, sep = ",")

# First consider the case where the event only directly affects price (?? = 0). 
# Estimate and report the price coefficients under all 4 scenarios for ?? and calculate the R2 
# for all these regressions. 
# Do the estimated price coefficients signal any endogeneity problem for these values of ?? and ??? 
# Can you also explain the pattern you find for the R2?**
  

model42_0 <- lm(data = exer42, SALES0_0 ~ PRICE0)
model42_1 <- lm(data = exer42, SALES0_1 ~ PRICE1)
model42_5 <- lm(data = exer42, SALES0_5 ~ PRICE5)
model42_10 <- lm(data = exer42, SALES0_10 ~ PRICE10)
exer42a_sum <- tibble(A = '0',
                      B = c('0', '1', '5', '10'),
                      beta = c(coefficients(model42_0)[[2]], coefficients(model42_1)[[2]], coefficients(model42_5)[[2]], coefficients(model42_10)[[2]]),
                      r_squared = c(summary(model42_0)$r.squared, summary(model42_1)$r.squared, summary(model42_5)$r.squared, summary(model42_10)$r.squared))
exer42a_sum


# In the scenario of alpha = 0 (i.e. event did not take place), the coefficient of beta is stable at almost -1 in all scenario. It means that price is not endogenous, since event does not affect sales.

# In terms of R-squared, with the increase of beta, the value of R-squared is also increase. Meaning that as variation in sales increases, beta can help in explaining it.


### Question b

# Repeat the exercise above, but now consider the case where the event only directly affects sales, 
# that is, set ?? = 0 and check the results for the four different values of ??.**
  
model42_B0_0 <- lm(data = exer42, SALES0_0 ~ PRICE0)
model42_B0_1 <- lm(data = exer42, SALES1_0 ~ PRICE0)
model42_B0_5 <- lm(data = exer42, SALES5_0 ~ PRICE0)
model42_B0_10 <- lm(data = exer42, SALES10_0 ~ PRICE0)
exer42b_sum <- tibble(A = c('0', '1', '5', '10'),
                      B = '0',
                      beta = c(coefficients(model42_B0_0)[[2]], coefficients(model42_B0_1)[[2]], coefficients(model42_B0_5)[[2]], coefficients(model42_B0_10)[[2]]),
                      r_squared = c(summary(model42_B0_0)$r.squared, summary(model42_B0_1)$r.squared, summary(model42_B0_5)$r.squared, summary(model42_B0_10)$r.squared))
exer42b_sum


# As A increases, the beta for Price slightly decreases. 
# It is still around -1. Price is not endogenous since event directly affects sales. 
# R2 drops significantly as the A increases, meaning that events bring higher variance of sales.

### Question c

# Finally consider the parameter estimates for the cases where the event affects price and sales, that is, look at ?? = ?? = 0, 1, 5, 10. Can you see the impact of endogeneity in this case?**
  
model42_AB_0 <- lm(data = exer42, SALES0_0 ~ PRICE0)
model42_AB_1 <- lm(data = exer42, SALES1_1 ~ PRICE1)
model42_AB_5 <- lm(data = exer42, SALES5_5 ~ PRICE5)
model42_AB_10 <- lm(data = exer42, SALES10_10 ~ PRICE10)
exer42c_sum <- tibble(A = c('0', '1', '5', '10'),
                      B = c('0', '1', '5', '10'),
                      beta = c(coefficients(model42_AB_0)[[2]], coefficients(model42_AB_1)[[2]], coefficients(model42_AB_5)[[2]], coefficients(model42_AB_10)[[2]]),
                      r_squared = c(summary(model42_AB_0)$r.squared, summary(model42_AB_1)$r.squared, summary(model42_AB_5)$r.squared, summary(model42_AB_10)$r.squared))
exer42c_sum

# Omission of Event leads to correlation between price and error. Price is endogenous in this case.

## Train Exercise 4-4

# Questions
# In this exercise we study the gasoline market and look at the relation between consumption and price in the USA.
# We will use yearly data on these variables from 1977 to 1999. Additionally we have data on disposable income, and
# some price indices. More precisely we have

# GC: log real gasoline consumption;
# PG: log real gasoline price index;
# RI: log real disposable income;
# RPT: log real price index of public transport;
# RPN: log real price index of new cars;
# RPU: log real price index of used cars.

# We consider the following model
# GC = ??1 + ??2PG + ??3RI + ??.

### Question a

# Give an argument why the gasoline price may be endogenous in this equation.**
  
#  There is a strategic decision for gasoline price, where in times of higher demand, gasoline price can be set higher, vice versa. Endogeneity might come from this decisions.

### Question b

# Use 2SLS to estimate the price elasticity (??2). Use a constant, RI, RPT, RPN, and RPU as instruments.**
  

exer44 <- read.csv('C:/Users/peter/OneDrive/Econometrics/Training-Exercise-4.4.csv')
exer44


# First step: Regress PG on instruments


stage_1_2sls <- lm(data = exer44, PG ~ RI + RPN + RPT + RPU)
summary(stage_1_2sls)


# Second step: Regress GC on PG_hat (from previous step) and RI


exer44$PG_hat <- predict(stage_1_2sls)
stage_2_2sls <- lm(data = exer44, GC ~ PG_hat + RI)
summary(stage_2_2sls)

# The slope of PG_hat is -0.54. Therefore, for every 1% increase in gasoline price, 
# gasoline consumption declines by -0.54%
  
#  OLS estimation yields weaker effect of price towards consumption.

exer44_ols <- lm(data = exer44, GC ~ PG + RI)
summary(exer44_ols)



### Question c

# Perform a Sargan test to test whether the five instruments are correlated with ??. What do you conclude?**
  
exer44$e_2sls <- residuals(stage_2_2sls)
sargan_regression <- lm(data = exer44, e_2sls ~ RI + RPN + RPT + RPU)
summary(sargan_regression)


sargan_stat <- nrow(exer44) * summary(sargan_regression)$r.squared
pchisq(sargan_stat, 
       df = 2, # df = k unrestricted model - k restricted model 
       lower.tail = FALSE)

# The Sargan test result is not statistically significant at p=0.05. 
# Therefore the null hypothesis of no correlation between Z and e cannot be rejected, 
# and the set of instruments are valid.


## Train Exercise 4.5

# In this exercise we reconsider the example of lecture 4.5. In this lecture we related the Grade Point Average [GPA]
# of learners in an engineering MOOC to the participation in a preparatory course. The dataset contains the following
# variables

# GPA: Grade Point Average in Engineering MOOC
# Gender: 0/1 dummy for gender (1: male, 0: female)
# Participation: 0/1 dummy for participation in a preparatory mathematics course (1: did participate, 0: did not participate)
# Email: 0/1 dummy for receiving an email invitation to take the preparatory course (1: received invitation, 0: did not receive invitation)

### Question a

# Redo the OLS estimation of the coefficients in a model that explains GPA using a constant, 
# gender and preparatory course participation. Also calculate standard errors and t-values. 
# Confirm that you obtain the same results as mentioned in the lecture**
  
exer45 <- read.csv('C:/Users/peter/OneDrive/Econometrics/TrainExer45.csv', header = TRUE, sep = ",")
ols_45 <- lm(data = exer45, GPA ~ PARTICIPATION + GENDER)
summary(ols_45)


### Question b

# Use the email dummy as an instrument to perform 2SLS estimation. First do the first-stage regression**
  
# Participation = ??1 + ??2Gender + ??3Email + ??.**
  
# Next calculate the predicted values according to this regression and perform OLS on the model**
  
# GPA = ??1 + ??2Gender + ??3Participation + d ??.**
  
# Confirm that the parameter estimates that you obtain are the same as reported in the lecture.**
  

first_stage_2sls_45 <- lm(data = exer45, PARTICIPATION ~ GENDER + EMAIL)
summary(first_stage_2sls_45)



exer45$PARTICIPATION_hat <- predict(first_stage_2sls_45)
second_stage_2sls_45 <- lm(data = exer45, GPA ~ PARTICIPATION_hat + GENDER)
summary(second_stage_2sls_45)

### Question c

# Obtain the standard errors that correspond to the final regression in the previous part. 
# These do not match with the standard errors reported in the lecture! 
# Why are the standard errors from part (b) wrong?**
  
# The standard error from the final stage of 2SLS is 0.798. 



## Train Exercise 5-5

### Question a


exer55 <- read.csv('C:/Users/peter/OneDrive/Econometrics/TrainExer5-5.csv', header = TRUE, sep = ",")
mod55 <- glm(data = exer55, 
             response ~ male + activity + age + I((age/10)**2), 
             family = 'binomial')
summary(mod55)

### Question b

# The researcher assumes that a value of 1 corresponds with positive response. 
# What happens if we impose that that positive response is zero and negative response equals 1. 
# The new response variable can be obtained from the old response variable 
# by the following transformation respnewi =???respi+1. (All zero observations become one 
# and all one observations become zero).**
  
exer55 <- exer55 %>%
  mutate(response_new = ifelse(response == 1, 0, 1))
mod55_b <- glm(data = exer55, 
               response_new ~ male + activity + age + I((age/10)**2), 
               family = 'binomial')
summary(mod55_b)

# The estimates magnitude are all the same, but the signs changed. 
# Estimates are still valid even if the responses are inverted.


### Question c

# Test the null hypothesis H0: ??1 = ??2 = 0 versus H1: no restrictions on ??1 and ??2, using a likelihood ratio test**
  
# restricted model
mod55_res <- glm(data = exer55, 
                 response ~ age + I((age/10)**2), 
                 family = 'binomial')
lmtest::lrtest(mod55_res, mod55)


# The likelihood ratio test statistics is significant at p=0.05. 
# Therefore we reject H0 that b1=b2=0. The set of variables b1 and b2 are significant in the regression.
