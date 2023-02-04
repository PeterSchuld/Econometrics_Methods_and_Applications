df <- read.csv('C:/Users/peter/OneDrive/Econometrics/TestExer2-GPA-round2.csv', header = TRUE, sep = ",")
#print(df1$Wage)

# (a) (i) Regress FGPA on a constant and SATV. Report the coefficient of SATV and its standard error and p-value
# (give your answers with 3 decimals).
# (ii) Determine a 95% confidence interval (with 3 decimals) for the effect on FGPA 
# of an increase by 1 point in SATV

mymodel <- lm(df$FGPA ~ df$SATV, data = df1)
summary(mymodel)
# df1['epsilon']<-residuals.lm(mymodel)
summary(df$epsilon)
#print(df1)

# df1['y_hat']<-2.44173 + 0.0609 * df1$SATV 

# df1['x_deviation_squared'] <- (df1$SATV - mean(df1$SATV))^2
# print(sum(df1$x_deviation_squared))


mymodel2 <- lm(df$FGPA ~ df$SATM, data = df1)
summary(mymodel2)
a<-predict.lm(mymodel,newdata=df1, interval='prediction') # 95% interval by default

my_data <- as.data.frame.table(a) 
plot(my_data)

mymodel2 <- lm(df$FGPA ~ df$FEM, data = df1)
summary(mymodel2)

res <- cor(df[2:5])
round(res, 2)