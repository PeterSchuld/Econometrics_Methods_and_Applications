library(ggplot2)
library(magrittr)
library(dplyr)
source("http://bit.ly/theme_pub")
theme_set(theme_pub())

df <- read.csv('C:/Users/peter/OneDrive/Econometrics/TrainExer 3-1.csv', header = TRUE, sep = ",")
df$Index[72]<-"1229.23"
df$Index[73]<-"1469.25"
df$Index[74]<-"1320.28"
df$Index[75]<-"1148.08"

df$Index[77]<-"1111.92"
df$Index[78]<-"1211.92"
df$Index[79]<-"1248.29"
df$Index[80]<-"1418.30"
df$Index[81]<-"1468.36"

df$Index[83]<-"1115.10"
df$Index[84]<-"1257.64"
df$Index[85]<-"1257.60"
df$Index[86]<-"1426.19"
df$Index[87]<-"1848.36"

df$Index<-as.numeric(df$Index)
sapply(df, class)
plot(df$Year , df$Index)


exer31 <- df # read_table2('C:/Users/peter/OneDrive/Econometrics/TrainExer 3-1-corrected.txt')
head(exer31)

exer31_chg <- exer31 %>%
  select(Year, Index, BookMarket) %>%
  mutate(log_index = log(Index),
         log_index_change = log_index - lag(log_index))
model31 <- lm(data = exer31_chg, log_index_change ~ BookMarket)
summary(model31)

model31b <- lm(data = exer31_chg, Index ~ BookMarket)
summary(model31b)


# Based on the model above, BookMarket ratio is still significant in this specification



### Question b

# Use dataset TrainExer33 to regress the change in the log of the S&P500 index on a constant, 
# the book-to-market ratio, and the square of the book-to-market ratio. 
# Is the relationship between the index and book-to-market quadratic?**
  
exer31_chg$BookMarket_sq <- (exer31_chg$BookMarket)**2


model33 <- lm(data = exer31_chg, log_index_change ~ BookMarket + BookMarket_sq)
summary(model33)


# Regression result shows insignificant effect of the square of book market ratio. 
# Therefore relationship might not be quadratic. 
# Note that we cannot solve the question by only plotting the variables.

### Question c

# Define a dummy that is 1 for 1980 and all following years. 
# Regress the change in the log of the S&P500 index on a constant, the book-to-market ratio, 
# and an interaction between the book-to-market ratio and the just-defined dummy. 
# Is the relationship between the index and book-to-market stable over the pre- and post-1980 period?
  
exer31_chg <- exer31_chg %>%
  mutate(is_1980_younger = ifelse(Year >= 1980, 1, 0))
model33_dummy <- lm(data = exer31_chg, log_index_change ~ BookMarket + BookMarket:is_1980_younger)
summary(model33_dummy)


# The interaction term of BookMarket and the dummy variable is insignificant. 
# Therefore, we can say that the relationship between index and book to market 
# is stable over the pre and post 1980 period.