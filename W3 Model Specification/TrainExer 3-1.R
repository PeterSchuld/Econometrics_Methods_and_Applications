library(ggplot2)
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

#df$LogIndex <- log(df$Index)
#library(dplyr)
#df1 <- df$LogIndex %>% ungroup %>%  summarise(across(-1, ~abs(diff(.))))


#df2 <- df[2:87,]
#df2$LogIndex_change <- df1$LogIndex

#mymodel <- lm(df2$LogIndex_change ~ df2$BookMarket, data = df1)
#summary(mymodel)

#plot(df1$Year, df1$LogIndex_change)

df_chg <- df %>%
  select(Year, Index, BookMarket) %>%
  mutate(log_index = log(Index),
         log_index_change = log_index - lag(log_index))
my_model <- lm(data = df_chg, log_index_change ~ BookMarket)
summary(my_model)


### Question b

# Now regress the S&P500 index (without any kind of transformation) on a constant and the book-to-market ratio. 
# Consider whether the effect of book-to-market on the index is significant in this specification.**
  
model31b <- lm(data = df_chg, Index ~ BookMarket)
summary(model31b)

### Question c

## Make a plot of the residuals e from both question (a) and (b) and comment on the difference.**
  

exer31_chg$base_residual <- model31b$residuals


# Log change model residual


exer31_chg %>%
  drop_na() %>%
  mutate(log_chg_residual = model31$residuals) %>%
  ggplot(aes(x = Year, y = log_chg_residual)) +
  geom_point()


# Base model residual


exer31_chg$base_residual <- model31b$residuals
qplot(data = exer31_chg, x = Year, y = base_residual)


# Plots above shows that the residuals for log change model is randomly distributed and has mean 0. There is no pattern if we plot it across the year. Meanwhile the residual plot of base model with no transformation has a non-random distribution, with a strong pattern following the Index trend. This indicates the violation of linear regression assumption.

