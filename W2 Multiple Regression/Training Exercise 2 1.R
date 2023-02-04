df1 <- read.csv('C:/Users/peter/OneDrive/Econometrics/TrainExer21.csv', header = TRUE, sep = ",")
#print(df1$Wage)
mymodel <- lm(df1$LogWage ~ df1$Female, data = df1)
summary(mymodel)
df1['epsilon']<-residuals.lm(mymodel)
print(df1)

mymodel2 <- lm(df1$epsilon ~ df1$Educ, data = df1)
summary(mymodel2)

mymodel3 <- lm(df1$epsilon ~ df1$Parttime, data = df1)
summary(mymodel3)
