#******************** Week 6 Asynch Work ********************
# create a dataframe to work with
myCars <- mtcars
str(mtcars) # understand what we're working with
summary(myCars)
View(myCars)

#******************* Practicing with visualizations ********************
hist(myCars$cyl) # look at distribution of cars by cylinder count

barplot(mtcars$cyl, names.arg = myCars$hp, las = 2) #barplot of number of cylinders by horsepower

EnsurePackage("ggplot2") # check for ggplot2 package / install with EnsurePackage function you created in EnsurePackage file

# build a histogram with layers in ggplot2
g <- ggplot(myCars, aes(x=cyl)) # this creates a ggplot where we use the myCars dataframe, with x-axis as the cylinders
g <- g + geom_histogram(binwidth=1, color="black", fill="white") #states we're using a histogram with white bars outlined in black
g <- g + ggtitle("cars cylinders histogram") # finally, we add a title to our visual
g

# we can also "see" our cylinder distribution using a boxplot
ggplot(myCars,aes(x=factor(0),cyl)) + geom_boxplot()

# put cars into two groups - high hp and low hp and create boxplot
myCars$hpCategory <- ifelse(myCars$hp > 123, "high", "low") # create a new column in the df based on high hp > the median, else low
g <- ggplot(myCars,aes(x=factor(hpCategory),hp)) 
g <- g + geom_boxplot() + coord_flip() # coord_flip will rotate the plot by 90 degrees
g <- g + ggtitle('HP Grouped by Low or High Value')
g

# practice with ggplot2 line charts & bar charts - let's look at hp of cars by cylinder
g <- ggplot(myCars,aes(x=reorder(myCars$hp,cyl),y=cyl,group=1)) # rotate the hp levels so we can easily read
g <- g + geom_line() # create the line chart
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

g <- ggplot(myCars,aes(x=reorder(myCars$hp,cyl),y=cyl,group=1)) # rotate the hp levels so we can easily read
g <- g + geom_col() # create the bar chart
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

# let's make a bar chart where the color of the bar represents the hp category
g <- ggplot(myCars,aes(x=reorder(hp,cyl), y=cyl,fill=hpCategory))
g <- g + geom_col()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

# let's practice with scatterplots
g <- ggplot(myCars, aes(x=cyl,y=wt)) # plot cylinders on x, weight on y
g <- g + geom_point(aes(size=hp,color=hp)) # create the plot with the points colored & sized by horsepower
g

g + geom_text(aes(label=mpg), size=4) # add text to show mpg of the cars
g + geom_text(aes(label=mpg),size=4, hjust=1,vjust=-1) # move the text to clean the image up a bit
# chart is still ugly -> clean it up some more
# first define "key" (high hp cars, with high mpg) to look at
minhp <- 250
minmpg <- median(myCars$mpg)
# we define a new column ‘keycar’ to be true if that car fits our defined criteria of hp and mpg
myCars$keycar <- myCars$hp > minhp & myCars$mpg > minmpg
# create three buckets for our visualizations, and add commas for ease of visual
minLabel <- format(min(myCars$hp),big.mark=",", trim=TRUE)
maxLabel <- format(max(myCars$hp),big.mark=",", trim=TRUE)
medianLabel <- format(median(myCars$hp),big.mark=",", trim=TRUE)
# create the plot calling the format bins from above
g <- ggplot(myCars, aes(x=mpg,y=hp))
g <- g + geom_point(aes(size=cyl,color=cyl, shape=keycar))
g <- g + geom_text(data=myCars[myCars$mpg > minmpg & myCars$hp > minhp,],aes(label=cyl, hjust=1, vjust=-1))
g + scale_color_continuous(name="Car",breaks = with(myCars,c(min(hp),median(hp),max(hp))),labels = c(minLabel, medianLabel, maxLabel),low = "white",high = "black")

#******************* More practice with visualizations ********************
# Let's create a mock set of values to play with
timeToSF <- c(4,4.5,3.5,5,4,4.2)
timeToSFWeek2 <- c(4.5,5,3.8,5.2,4.6,4.3)
day <- c("mon","tues","wed","thurs","fri","sat")
week1 <- c(1,1,1,1,1,1)
week2 <- c(2,2,2,2,2,2)
time <- c(timeToSF,timeToSFWeek2)
week <- as.factor(c(week1,week2))
dayOfWeek <- c(day,day)
# then let's bring it all together as a data frame
dfTravelTime <- data.frame(day,timeToSF,timeToSFWeek2)
dfTravelTime
# now we'll plot it in a line chart
ggplot(dfTravelTime, aes(x=day,y=timeToSF,group=1)) + geom_line() # create simple line chart
g <- ggplot(dfTravelTime, aes(x=day,y=timeToSF,group=1)) + geom_line(color = "red", linetype="dashed", size=1.5) #create same chart but format it a bit
g
g + geom_point() #add a layer of data points to the chart
g + geom_point(color="blue",size=4) #change the color and size of the points on the chart
g + geom_point(color="blue",size=4) + ylab("time to SF (in hours)") #add a label to the y axis

# let's make a new dataframe to play with
df <- data.frame(dayOfWeek, time, week)
df
g <- ggplot(df,aes(x=dayOfWeek,group=week,color=week)) + geom_line(aes(y=time)) # line chart showing various travel time on day of week compared to each week
g
g <- g + ylab("Time to SF (in hours)") + ggtitle("Comparing Weekly Travel Time To SF") # label y-axis, and give chart a title
g

#******************* Even more practice with Bar & Scatter Plots ********************
ggplot(mtcars,aes(x=factor(0),mpg)) + geom_boxplot() #view of the distribution w/ factor(0) to see all MPGs together
ggplot(mtcars,aes(group=cyl,x=cyl,mpg)) + geom_boxplot() #view of the distribution of mpg groupd by cylinder

ggplot(mtcars,aes(x=cyl)) + geom_bar() #this is basically just a histogram

car.names <- rownames(myCars) #get the names of the cars in our myCars dataframe
car.names

g <- ggplot(myCars,aes(x=car.names,y=wt)) + geom_bar(stat="identity") #bar chart of the cars w/ name on x-axis and weight on y-axis
g

g <- g + theme(axis.text.x=element_text(angle=90,hjust=1)) #format the x-axis so we can actually read the car names
g

g <- g + ggtitle("My Car Weight Chart") #give our chart a nice little title
g

g <- ggplot(myCars,aes(x=cyl,fill=factor(gear))) + geom_bar() #histogram (again) of the cylinders, but showing underlying data of how many gears
g

g <- ggplot(myCars,aes(x=cyl,fill=factor(gear))) + geom_bar(position="dodge") #same histograme as last code, but not stacked so easier to compare the gears
g

g <- ggplot(myCars,aes(x=mpg,y=wt)) + geom_point() #scatter plot of wt and mpg in myCars dataframe
g

g <- ggplot(myCars,aes(x=mpg,y=wt)) + geom_point(aes(size=qsec)) #same plot, but this time points are sized based on the qsec var (quarter mile speed of each car)
g

g <- ggplot(myCars,aes(x=mpg,y=wt)) + geom_point(aes(size=qsec, color=qsec)) #same thing again, with size and color based on the speed
g
#now let's add our car names to the dots
g <- g + geom_text(aes(label=car.names),size=3)
g  

g <- ggplot(myCars,aes(x=mpg, y=car.names)) + geom_point(size=3) #chart of car names with MPG plotted on x-axis
g  

g <- ggplot(myCars,aes(x=mpg,y=reorder(car.names,mpg))) + geom_point(size=3) #same chart, but this time y-axis ordered by MPG
g
  
  
  
  
  