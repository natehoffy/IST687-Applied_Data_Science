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



