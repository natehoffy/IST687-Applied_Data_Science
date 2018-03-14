########## IST 687 - Support Vector Machines ##########
########## Nate Hoffelmeyer ##########
########## March 6, 2018 ##########

# -------
# Step 1: Load the Data
# -------

# copy the dataset
aq <- airquality
summary(aq)
str(aq)
aq
# set nas equal to mean of col where there are nas
aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone,na.rm=T)
summary(aq)
str(aq)
aq
aq$Solar.R[is.na(aq$Solar.R)] <- mean(aq$Solar.R,na.rm=T)
summary(aq)
str(aq)
aq

# -------
# Step 2: Create train and test data sets
# -------

# create a random index (reduce bias) and cut point in dataset
randIndex <- sample(1:dim(aq)[1])
cutPoint2_3 <-floor(2*dim(aq)[1]/3)

#Creating training and testing data set.
aq.train <- aq[randIndex[1:cutPoint2_3],]
aq.test <- aq[randIndex[(cutPoint2_3+1):dim(aq)[1]],]
head(aq.train)

# -------
# Step 3: Build a Model using KSVM & visualize the results
# -------

# Building model using KSVM & predict test data
model.ksvm <- ksvm(Ozone ~ Solar.R + Wind + Temp, data=aq.train)
summary(model.ksvm)
pred.ksvm <- predict(model.ksvm,aq.test)
table(pred,aq.test$Ozone)

# Root mean squared error
rmse <- aq.test$Ozone - pred.ksvm
sqrt(mean(rmse^2))

# Scatter plot of results
g.ksvm <- ggplot(aq.test, aes(x=Temp, y=Wind)) + geom_point(aes(size = rmse, color=rmse))
g.ksvm

# Building model using SVM & predict test data
EnsurePackage("e1071")
model.svm <- svm(Ozone ~ Solar.R + Wind + Temp, data=aq.train)
summary(model.svm)
pred.svm <- predict(model.svm, aq.test)
table(pred,aq.test$Ozone)

# Root mean squared error for SVM model
rmse.svm <- aq.test$Ozone - pred.svm
sqrt(mean(rmse.svm^2))

# SVM model scatterplot
g.svm <- ggplot(aq.test, aes(x=Temp, y=Wind)) + geom_point(aes(size = rmse.svm, color=rmse.svm)) 
g.svm

# Building a model using LM & predict test data
model.lm <- lm(formula = Ozone~ Wind + Temp, data = aq.train)
pred.lm <- predict(model.lm, aq.test, type="response")
summary(model.lm)

# Root mean squared error for LM model
rmse.lm <- aq.test$Ozone - pred.lm
sqrt(mean(rmse.lm^2))

# LM model scatterplot
g.lm <- ggplot(data = aq.test, aes(x=Temp,y=Wind)) + geom_point(aes(size=rmse.lm, color = rmse.lm)) 
g.lm

# All charts shown using grid.arrange
EnsurePackage('gridExtra')
grid.arrange(g.ksvm,g.svm,g.lm)

# -------
# Step 4: Create a 'goodOzone' variable
# -------

# First calc average Ozone
meanOzone <- mean(aq$Ozone)
meanOzone 

# Add a column for mean Ozone to our aq data.frame, 0 if below average and 1 if above average
for ( i in 1:length(aq$Ozone))
{
  if( aq[i,1] < meanOzone )
  {
    aq[i,7] = '0'
  }
  else
  {
    aq[i,7] = '1'
  }
}
# variable is named V7 - change its name
colnames(aq) <- c("Ozone","Solar.R","Wind","Temp","Month","Day","goodOzone")
summary(aq)
str(aq)
head(aq)

# -------
# Step 5: See if we can do better at predicting good & bad days
# -------

# save as a new df
newAir <- aq
summary(newAir)
# randomize and set cut point of new df
randIndex1 <- sample(1:dim(newAir)[1])
CutPoint2_3 <-floor(2*dim(newAir)[1]/3)
# create train and test data.sets
newAir.train <- newAir[randIndex1[1:cutPoint2_3],]
newAir.test <- newAir[randIndex[(CutPoint2_3+1):dim(newAir)[1]],]
head(newAir.train)
# model using KSVM
m.ksvm <- ksvm(goodOzone ~ Wind + Temp, newAir.train)
predGO.ksvm <- predict(m.ksvm, newAir.test)
summary(predGO.ksvm)
str(predGO.ksvm)
table(predGO.ksvm,newAir.test$goodOzone)
# percent of goodOzone correctly predicted
results.go <- table(predGO.ksvm,newAir.test$goodOzone)
results.go
totCorrect.go <-  results.go[1,1] + results.go[2,2]
totCorrect.go
totintest.go <- nrow(newAir.test)
totintest.go
pctCorrect.go <- round((totCorrect.go/totintest.go)*100,1)
# format output of percent correct to show percent sign
paste(pctCorrect.go,"%",sep="")
# calc the error
rmse.goksvm <- as.numeric(newAir.test$goodOzone)-as.numeric(predGO.ksvm)
sqrt(mean(rmse.goksvm^2))
# plot the model ksvm good ozone (go)
g.goksvm <- ggplot(data = newAir.test, aes(x=Temp,y=Wind)) + geom_point(aes(size = rmse.goksvm, shape = predGO.ksvm, color = newAir.test$goodOzone))
g.goksvm
# model using SVM
m.svm <- svm(goodOzone ~ Wind + Temp, newAir.train, type = "C-classification")
predGO.svm <- predict(m.svm, newAir.test)
summary(predGO.svm)
results.gosvm <- table(predGO.svm,newAir.test$goodOzone)
results.gosvm
# accuracy of SVM model
totCorrect.gosvm <- results.gosvm[1,1] + results.gosvm[2,2]
totCorrect.gosvm
pctCorrect.gosvm <- round((totCorrect.gosvm/totintest.go)*100,1)
paste(pctCorrect.gosvm,"%",sep="")
# calculate the error of the svm model
err.gosvm <- as.numeric(newAir.test$goodOzone)-as.numeric(predGO.svm)
sqrt(mean(err.gosvm^2))
# plot the svm model of goodOzone
g.gosvm <- ggplot(data = newAir.test, aes(x=Temp,y=Wind)) + geom_point(aes(size = err.gosvm, shape = predGO.svm, color = newAir.test$goodOzone))
g.gosvm
# model using NB
m.nb <- naiveBayes(as.factor(goodOzone) ~ Wind + Temp, newAir.train)
predGO.nb <- predict(m.nb, newAir.test)
str(predGO.nb)
summary(predGO.nb)
results.gonb <- table(predGO.nb,newAir.test$goodOzone)
results.gonb
# accuracy of NB model
totCorrect.gonb <- results.gonb[1,1] + results.gonb[2,2]
totCorrect.gonb
pctCorrect.gonb <- round((totCorrect.gonb/totintest.go)*100,1)
paste(pctCorrect.gonb,"%",sep="")
# error of the NB model
err.gonb <- as.numeric(newAir.test$goodOzone) - as.numeric(predGO.nb)
sqrt(mean(err.gonb^2))
# plot the nb model
g.gonb <- ggplot(data = newAir.test, aes(x=Temp,y=Wind)) + geom_point(aes(size=err.gonb,shape = predGO.nb, col = newAir.test$goodOzone))
g.gonb
# combine all three charts with 2 charts in one row]
grid.arrange(g.goksvm,g.gosvm,g.gonb, nrow = 3) # i didn't do two charts in one row because you can't see them....

# -------
# Step 6: Which are the best Models
# -------

# the KSVM model was most accurate at 80.4%
# the SVM and NB models had lower rmse, both at 1.154701
# given that error on KSVM is not much higher and that it predicts 2% better - I would recommend using KSVM model
