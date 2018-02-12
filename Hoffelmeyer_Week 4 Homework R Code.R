##############
# Week 4 - HW
# Raw R Code
##############

# STEP 1: Write a summarizing function to understand the distribution of a vector
# Call the function printVecInfo
# Return mean, median, min & max, standard deviation, quantile at 0.05 and 0.95, and skewness
printVecInfo <- function(myVector){
  print(mean(myVector))
  print(median(myVector))
  print(min(myVector))
  print(max(myVector))
  print(sd(myVector))
  print(quantile(myVector, probs=c(0.05,0.95)))
  print(skewness(myVector))
}

printVecInfo2 <- function(vec) {     #Fucntion created to return the following from any vector and print the answers  (#1 & #2)
  Vmean <- mean(vec)                # mean passed through the function
  cat ("mean:",Vmean,"\n")    #comments mean with value
  Vmedian <- median(vec)           #return the median passed through the function
  cat("median:",Vmedian,"\n")  #comments median value
  Vmin <- min(vec)   #return the min value of the vector passed through the function
  cat ("min:", Vmin, "\n")     #comments min value
  Vmax <- max(vec)        #return the max value of the vector passed through the function
  cat ("max:", Vmax, "\n")      #comments max value
  Vsd <- sd(vec)                #the standard deviation of the vector passed through the function
  cat ("sd:", Vsd, "\n")         #comments standard deviation
  Vquant <- quantile(vec,probs = c(0.05,0.95))    #return the values of the quantiles 5% and 95% of the vector that is passed
  cat ("quantile:", Vquant, "\n")  #comments quantile
  Vskew <- skewness(vec)         #returns the skewness of the vector that is passed through the function
  cat ("skweness:", Vskew,"\n")
  return()                       #returns all of the above in printed form
}

# Test the function with a vector	(1,2,3,4,5,6,7,8,9,10,50)
v <- c(1,2,3,4,5,6,7,8,9,10,50)
printVecInfo2(v)

# STEP 2: Creating Samples in a Jar
# Create a variable 'jar' that has 50 red and 50 blue marbles
jar <- c('red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue','blue')
# Confirm	there	are	50	reds	by	summing	the	samples	that	are	red, first installing and then using stringr
install.packages("stringr")
library(stringr)
sum(str_count(jar, "red"))

# Sample 10 marbles from the jar and take the average of the red marbles
mean(str_count(sample(jar,10,replace=TRUE),"red"))

# Do the sampling 20 times using 'replicate' and take the average of the red marbles
replicate(20,mean(str_count(sample(jar,10,replace=TRUE),"red")))

# Repeat above 20 replications	but	this	time,	sample	the	jar	100	times instead of just 10. Also save it as a vector for use
hundredReds <- replicate(20,mean(str_count(sample(jar,100,replace=TRUE),"red")))

# Use	your	printVecInfo	to	see	information	of	the	samples
printVecInfo(hundredReds)

# Also generate a histogram of the hundredReds samples
hist(hundredReds)

# Repeat the 100 samplings, but replicate it 100 times instead of just 20
hundredHundredReds <- replicate(100,mean(str_count(sample(jar,100,replace=TRUE),"red")))
hundredHundredReds

# Use	your	printVecInfo	to	see	information	of	the	samples
printVecInfo(hundredHundredReds)

# Also generate a histogram of the hundredHundredReds samples
hist(hundredHundredReds)

# STEP 3: Explore the airquality dataset
# Store the 'airquality' dataset into a temporary variable
ac <- airquality

# Clean the ac dataset by removing NAs -> store as a clean set
cleanAC <- na.omit(ac)

#Explore Ozone w/ a printVecInfo and hist
printVecInfo(cleanAC$Ozone)
hist(cleanAC$Ozone)

#Explore Wind w/ a printVecInfo and hist
printVecInfo(cleanAC$Wind)
hist(cleanAC$Wind)

#Explore Temp w/ a printVecInfo and hist
printVecInfo(cleanAC$Temp)
hist(cleanAC$Temp)

