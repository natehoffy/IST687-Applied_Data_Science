#################### Linear Modeling - Week 8 ####################
# Practice using data to determine if more frequent car oil changes are a good thing
# following R code defines our dataset
oilChanges <- c(3,5,2,3,1,4,6,4,3,2,0,10,7,8) # number of oil changes each car had in past 3 years
repairs <- c(300, 300, 500, 400, 700, 420, 100, 290, 475, 620, 600, 0, 200, 50) # $ amount of repairs
miles <- c(20100, 23200, 19200, 22100, 18400, 23400, 17900, 19900, 20100, 24100, 18200, 19600, 20800, 19700) # miles driven by each car
oil <- data.frame(oilChanges, repairs, miles)
oil

# Given this data, can we determine if changing the oil frequently is a good thing?
# Our independent variables are ‘oilChanges’ and ‘miles’
# The dependent variable (the one we are trying to predict) is ‘repairs’
# First let's explore
plot(oil$oilChanges, oil$repairs)
plot(oil$miles, oil$repairs)
# see correlation between # of oil changes and repairs $, but not really any patter between miles and repair $

# Let's build our first model using oilChanges as our independent variable
model1 <- lm(formula=repairs ~ oilChanges, data=oil)
# The lm() command creates a big data structure (the model that we justcreated) -> store it as model1
# the ‘lm’ command takes two parameters,1) tells lm to use ‘oilChanges to predict ‘repairs’ (the squiggly line ‘~’ tells lm what is
## the independent and dependent variables to be modeled, with the column on the left
## being the independent variables). The other parameter tells lm the name of the
## dataframe we are using.

summary(model1)

# Next, we show the line of best fit (based on the model) to the x-y plot of repair costs against oil changes with this command:
plot(oil$oilChanges, oil$repairs)
abline(model1)


# Next, let’s try using both oilChanges and miles, to create the linear model
m <- lm(formula=repairs ~ oilChanges + miles, data=oil)
summary(m)

# We need to model the cost of oil changes into our dataset too
# Assume cost of $350 per oil change
oil$oilChangeCost <- oil$oilChanges * 350
oil$totalCost <- oil$oilChangeCost + oil$repairs
m <- lm(formula=totalCost ~ oilChanges, data=oil)
plot(oil$oilChanges, oil$totalCost)
abline(m)

# To test this model, we can have R predict the total repair costs for a given number of oil changes with the following R code:
test = data.frame(oilChanges=0)
predict(m,test, type="response")

test = data.frame(oilChanges=5)
predict(m,test, type="response")

test = data.frame(oilChanges=10)
predict(m,test, type="response")

ggplot(oil, aes(x = oilChanges, y = totalCost)) + geom_point() + stat_smooth(method = "lm", col = "red")

##### More practice with modeling #####

air <- airquality
air$Ozone[is.na(air$Ozone)] <- mean(air$Ozone,na.rm=TRUE)
any(is.na(air$Ozone))
air$Solar.R[is.na(air$Solar.R)] <- mean(air$Solar.R,na.rm = T)
any(is.na(air$Solar.R))

plot(air$Solar.R,air$Ozone) # no relation
plot(air$Wind,air$Ozone) # some relation
plot(air$Temp,air$Ozone) # some relation
plot(air$Month,air$Ozone)

m <- lm(formula = Ozone ~ Wind, data = air)
summary(m)

m <- lm(formula = Ozone ~ Wind + Temp, data = air)
summary(m)

m <- lm(formula = Ozone ~ Wind + Temp + Solar.R, data = air)
summary(m)

m <- lm(formula = Ozone ~ Wind + Temp + Solar.R, data = air)
summary(m)


##### In class alternative models #####
x <- c(1:10)
y <- c(1:10)
df <- data.frame(x,y)
plot(df$x,df$y)

m <- lm(formula=y ~ x, data=df)
summary(m)
abline(m)


x <- c(1,2,3,4,5)
y <- c(0.2,2.2,3.8,4.9,5.1)

df <- data.frame(x,y)
plot(df$x,df$y)
m1 <- lm(formula = y ~ x, data = df)
summary(m1)
abline(m1)

g <- ggplot(df,aes(x=x,y=y)) + geom_point()
g
g + stat_smooth(method = 'lm', col = 'red')

w <- c(1,3,4,4.5,5,6.1,7.3,7.9,9.2,10.05)
df <- data.frame(x,y,w)
plot(df$x,df$w)
m1 <- lm(formula = w ~ x, data = df)
summary(m1)
abline(m1)

ggplot(df, aes(x = x, y = w)) + geom_point() + stat_smooth(method = 'lm', col = 'red')


x <- sample(1:100, 10, replace = F)  
y <- sample(1:100, 10, replace = F)  

df <- data.frame(x,y)  
plot(x, y)  
m1 <- lm(formula = x ~ y, data = df)  
summary(m1)  
abline(m1)

ggplot(data = df, aes(x = x, y = y)) + geom_point() + stat_smooth(method = 'lm', col = 'red')
  
  
  
###########################################################
mpg.lm = lm(formula = mpg ~ wt, data = mtcars)
summary(mpg.lm)  

mpg.lm = lm(formula = mpg ~ hp, data = mtcars)
summary(mpg.lm)  


mpg.lm <- lm(formula = mpg ~ hp + wt, data = mtcars)  
summary(mpg.lm)

mpg.lm <- lm(formula = mpg ~ hp + wt + cyl, data = mtcars)  
summary(mpg.lm)

mpg.lm <- lm(formula = mpg ~ ., data = mtcars)  
summary(mpg.lm)

mpg.lm <- lm(formula = mpg ~ wt + cyl, data = mtcars)  
summary(mpg.lm)



# the output of predict
range(mtcars$wt)
newdata = data.frame(cyl=4,wt=2.8)
predict(mpg.lm, newdata, type='response')


mtcars[mtcars$hp > 110 && mtcars$hp < 130, ]

# see how good the model is
# pvalue < 0.05?
# how much does model explain (the r-squared value)
sum.model <- summary(mpg.lm)
paste("p-values:")
sum.model$coef[,4] #p-values
paste("adjusted r squared:", sum.model$adj.r.squared) #R^2


# show the full model - using a scatter plot of the actual values and a set of predicted values
mpg.lm = lm(formula=mpg ~ hp + wt, data=mtcars)


range(mtcars$wt)
xweight <- seq(0,6,6/32)
xweight <- xweight[1:32]

range(mtcars$hp)
xhp <- seq(50, 355, 300/length(xweight))  
xhp <- xhp[1:32]

ympg <- predict(mpg.lm, list(wt = xweight, hp=xhp),type="response")
ympg <- round(ympg)

# scatter plots
g <- ggplot(mtcars,aes(x=hp,y=wt)) + geom_point(aes(size=mpg,color=mpg))
g
g + geom_smooth(method="lm")


