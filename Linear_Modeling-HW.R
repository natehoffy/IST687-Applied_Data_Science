########## IST 687 - Making Predictions HW ##########
########## Nate Hoffelmeyer ##########
########## March 2, 2018 ##########

# 1) & 2) Read in the data, obtained from the url in the hw file. Save file to computer and load via gdata pkg read.xls
EnsurePackage('gdata')
antelope <- read.xls("~/Desktop/IST 687/mlr01.xls")

# 3) Inspect the data --> rename columns too
str(antelope)
antelope
colnames(antelope) = c("fawn_pop","adult_pop","precip_amt","winter_strng")
antelope

# 4) # Bivariate plots of fawns to adults to precip to winter severity
g <- ggplot(antelope,aes(x=adult_pop,y=fawn_pop)) + geom_point() + xlab("Adults") + ylab("Fawns") + ggtitle("Fanws vs. Adult Antelopes")
g # plot of fawns vs adults 

g <- ggplot(antelope,aes(x=precip_amt,y=fawn_pop)) + geom_point() + xlab("Precipitation") + ylab("Fawns") + ggtitle("Fawns vs. Precipitation Amount")
g # plot of fawns versus precipitation amount

g <- ggplot(antelope,aes(x=winter_strng,y=fawn_pop)) + geom_point() + xlab("Winter Strength") + ylab("Fawns") + ggtitle("Fawns vs. Winter Strength")
g # plot of fawns versus strength of winter

# 5) Build regression models using lm function
## model predicting number of fawns based on strength of winter
m <- lm(formula = fawn_pop ~ winter_strng, data = antelope)  
summary(m)
predict(m,winter_strng=4,type='response')

## model predicting number of fawns based on strength of winter and precip amount
m <- lm(formula = fawn_pop ~ winter_strng + precip_amt, data = antelope)
summary(m)
newdata = data.frame(winter_strng=3,precip_amt=mean(antelope$precip_amt))
predict(m,newdata,type='response')

## model predicting number of fawns based on strength of winter + precip + adult pop
m <- lm(formula = fawn_pop ~ winter_strng + precip_amt + adult_pop, data = antelope)
summary(m)
newdata = data.frame(winter_strng=3,precip_amt=mean(antelope$precip_amt),adult_pop=mean(antelope$adult_pop))
predict(m,newdata,type='response')

# The best model is the last model, with all the variables used to predict fawn_pop
# In model 3, all vars are stat sig at 95% level and intercept is sig at 99% level --> most data & most sig so  best predict
# In model 1, winter_strng was stat sig at 95% level
# In model 2, intercept sig at 95% level and precip_amt at 99%, but winter_strng not significant

# The model that would do the best job with the fewest predictors would be the first model, where we
## just use winter_strng to predict fawn_pop. This model gives us the most significant intercept, and
## independent variable in winter strength, even when looking at just precip and adult individually (shown below for ref)
m <- lm(formula = fawn_pop ~ precip_amt, data = antelope) #intercept negative, so not as helpful as above model
summary(m)

m <- lm(formula = fawn_pop ~ precip_amt + adult_pop, data = antelope) #no significant x's
summary(m)

m <- lm(formula = fawn_pop ~ adult_pop, data = antelope) # very significant m (y=mx+b), but b is negative and not as sig
summary(m)

m <- lm(formula = fawn_pop ~ adult_pop + winter_strng, data = antelope) # intercept & one IV not significant
summary(m)

