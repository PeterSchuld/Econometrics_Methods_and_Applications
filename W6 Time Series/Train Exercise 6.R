library(ggplot2)
library(magrittr)
library(dplyr)
install.packages("lmtest")

source("http://bit.ly/theme_pub")
theme_set(theme_pub())

exer61 <- read.csv('C:/Users/peter/OneDrive/Econometrics/TrainExer61.csv', header = TRUE, sep = ",")

## Train Exercise 6-1

### Question a

# Use dataset TrainExer61 to make the following graphs: the time series plot of xt against time t, 
# the time series plot of yt against time t, and the scatter plot of yt against xt. What conclusion 
# could you draw from these three graphs?**
  


# From the time series plot below, we can see that the relation between X and Y do not track 
# each other and looks like a random walk. However, when we plot it into a crossplot, we can see 
# a negative correlation pattern between the two variables. In this case, the correlation might be spurious.

exer61 %>%
  ggplot(aes(x = as.numeric(row.names(.)))) +
  geom_line(aes(y = X), color = 'red') +
  geom_line(aes(y = Y), color = 'blue') +
  labs(title = 'Time series X and Y')


exer61 %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point() +
  labs(title = 'Cross plot between X and Y')


### Question b

# To check that the series ??x t and ??y t are uncorrelated, regress ??y t on a constant and ??x t. 
# Report the t-value and p-value of the slope coefficient.**
  
exer61_modb <- lm(data = exer61, EPSY ~ EPSX)
summary(exer61_modb)
```
# The t-value of the slope is < 2, which is not significant at p=0.05. EPSX is not correlated with EPSY

### Question c

# Extend the analysis of part (b) by regressing ??y t on a constant, ??x t, 
# and three lagged values of ??y t and of ??x t. Perform the F-test for the joint insignificance 
# of the seven parameters of ??x t and the three lags of ??x t and ??y t. 
# Report the degrees of freedom of the F-test and the numerical outcome of this test, 
# and draw your conclusion. Note: The relevant 5% critical value is 2.0.**
  
exer61 <- exer61 %>%
  mutate(EPSX_lag1 = lag(EPSX, 1),
         EPSX_lag2 = lag(EPSX, 2),
         EPSX_lag3 = lag(EPSX, 3),
         EPSY_lag1 = lag(EPSY, 1),
         EPSY_lag2 = lag(EPSY, 2),
         EPSY_lag3 = lag(EPSY, 3))
exer61_modc <- lm(data = exer61, EPSY ~ EPSX + EPSX_lag1 + EPSX_lag2 + EPSX_lag3 + EPSY_lag1 + EPSY_lag2 + EPSY_lag3)
summary(exer61_modc)


# Summary shows the F statistics of 0.55 and p-value of 0.799. It means that we cannot reject H0 of the slope 
# of sets of variables = 0. The sets of variables are not important in predicting EPSY.

### Question d

# Regress y on a constant and x. Report the t-value and p-value of the slope coefficient. What conclusion would you be tempted to draw if you did not know how the data were generated?**
  
# The regression shows that X is important in predicting Y, however this is a spurious correlation 
# representing the crossplot in Question a.

### Question e

# Let et be the residuals of the regression of part (d). Regress et on a constant and the one-period 
# lagged residual et???1. What standard assumption of regression is clearly violated for the regression in part (d)?**
  
# Here we can see that the residuals are strongly correlated with its lag. 
# Standard assumption of regression includes that residuals are random and uncorrelated.


exer61$residual_modd <- residuals(exer61_modd)
exer61$residual_modd_lag1 <- lag(exer61$residual_modd)
exer61_mode <- lm(data = exer61, residual_modd ~ residual_modd_lag1)
summary(exer61_mode)


# The plot below confirmed this finding.


exer61 %>%
  ggplot(aes(x = Y, y = residual_modd)) +
  geom_point()



exer61_modd <- lm(data = exer61, Y ~ X)
summary(exer61_modd)





