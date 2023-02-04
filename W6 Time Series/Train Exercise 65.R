library(ggplot2)
library(magrittr)

if(!require('tidyr')) {
  install.packages('tidyr')
  library('tidyr')
}

library(dplyr)

install.packages("lmtest")

source("http://bit.ly/theme_pub")
theme_set(theme_pub())

exer65 <- read.csv('C:/Users/peter/OneDrive/Econometrics/W6 Time Series/TrainExer64.csv', header = TRUE, sep = ",")


### Question a

# Make time series plots of log(IP) and log(CLI), and also of the yearly growth rates GIP and GCLI. What conclusions do you draw from these plots?**
  
exer65_train <- filter(exer65, YEAR <= 2002)
exer65_test <- filter(exer65, YEAR > 2002)


# The time series LOGIP and LOGCLI is not stationary and has an upward trend. However it might be cointegrated.


exer65_train %>%
  gather(key = 'metric', value = 'value', -YEAR) %>%
  filter(metric == 'LOGIP' | metric == 'LOGCLI') %>%
  ggplot(aes(x = YEAR, y = value, color = metric)) +
  geom_line()


# Meanwhile the growth rates GIP and GCLI is probably stationary, however we need to check it statistically.


exer65_train %>%
  gather(key = 'metric', value = 'value', -YEAR) %>%
  filter(metric == 'GIP' | metric == 'GCLI') %>%
  ggplot(aes(x = YEAR, y = value, color = metric)) +
  geom_line()


### Question b

# (i) Perform the Augmented Dickey-Fuller (ADF) test for log(IP). In the ADF test equation, 
# include (among others) a constant (??), a deterministic trend term (??t), and two lags of GIP = ???log(IP). 
# Report the coefficient of log(IPt???1) and its standard error and t-value, and draw your conclusion.**
  
# (ii) Perform a similar ADF test for log(CLI).**

# For question b(i), the coefficient of LOGIP_lag1 is -0.275, standard error is 0.101, and t-value is -2.706. Since t-value -2.706 > -3.5, we cannot reject H0 of non-stationarity. Therefore LOGIP is non-stationary.


exer65_train <- exer65_train %>%
  mutate(LOGIP_lag1 = lag(LOGIP, 1),
         GIP_lag1 = lag(GIP, 1),
         GIP_lag2 = lag(GIP, 2),
         LOGCLI_lag1 = lag(LOGCLI, 1),
         GCLI_lag1 = lag(GCLI, 1),
         GCLI_lag2 = lag(GCLI, 2))

adf_exer65b_1 <- lm(data = exer65_train, GIP ~ YEAR + LOGIP_lag1 + GIP_lag1 + GIP_lag2)
summary(adf_exer65b_1)


# For question b(ii), LOGCLI_lag1 coefficient is -0.24, standard error is 0.127, and t-value is -1.885. LOGCLI is also non-stationary.


adf_exer65b_2 <- lm(data = exer65_train, GCLI ~ YEAR + LOGCLI_lag1 + GCLI_lag1 + GCLI_lag2)
summary(adf_exer65b_2)
