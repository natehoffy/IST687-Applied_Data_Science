# ---------- Nate Hoffelmeyer-IST 687-37225 -----------
# ------------------ Week 6 Homework ------------------
# ---- Viz HW: air quality Analysis ----

# Step 1: Load the data
airquality #since we're using the airquality df, it's already loaded - this is just a check we have access to it.

# Step 2: Clean the data
str(airquality) #str to see what the variables are stored as
df <- airquality #save airquality to a manipulatable dataframe called df
View(df) #view the df to see where the na's are
df$Ozone[is.na(df$Ozone)] <- mean(df$Ozone,na.rm=TRUE)  # replce NA's in col with mean of the col
df$Solar.R[is.na(df$Solar.R)] <- mean(df$Solar.R,na.rm=TRUE) #replace NA in col with mean of col
df #check that the setting of NA values to 0 worked
str(df) #check to see what the variables are stored as, since the df right above added lots of decimals to Ozone and Solar.R
df <- transform(df, Ozone = as.integer(Ozone), Solar.R = as.integer(Solar.R)) #transform Ozone and Solar.R from num to int
df # view the df to make sure ozone and solar.r removed decimals
str(df) #check to make sure the cols you modified are back to same as airquality

# Step 3: Understand the data distribution
# Create the following visualizations using ggplot:
# Histograms for each of the variables
gbar_ozone <- ggplot(df,aes(x=Ozone)) + geom_histogram(binwidth=40, color="black", fill="white") #create a histogram for ozone, formatted so you can understand it
gbar_ozone #view the histogram
gbar_solar.r <- ggplot(df,aes(x=Solar.R)) + geom_histogram(binwidth=40, color="black", fill="white") #create a histogram for Solar.R, formatted so you can understand it
gbar_solar.r
gbar_wind <- ggplot(df,aes(x=Wind)) + geom_histogram(binwidth=3, color="black", fill="white") #histogram for wind, formatted
gbar_wind
gbar_temp <- ggplot(df,aes(x=Temp)) + geom_histogram(binwidth=3, color="black", fill="white") #histogram for Temp, formatted
gbar_temp
# Boxplot for Ozone
gbox_ozone <- ggplot(df,aes(x=factor(0),Ozone)) + geom_boxplot() #ozone boxplot all together to see dispersion of the variable
gbox_ozone
# Boxplot for Wind
gbox_wind <- ggplot(df,aes(x=factor(0),y=Wind)) + geom_boxplot(color="Red",size=1)
gbox_wind

# Step 3: Explore how the data changes over time
df$Date <- as.Date(paste("1973", airquality$Month, airquality$Day, sep = "-")) #create a date column for use as the x-axis
df
# line chart for ozone
gline_ozone <- ggplot(df, aes(x = Date, y = Ozone)) + geom_line(color = "blue", size = 1)
gline_ozone
# line chart for temp
gline_temp <- ggplot(df, aes(x = Date, y = Temp)) + geom_line(color = "red", size = 1)
gline_temp
# line chart for Wind
gline_wind <- ggplot(df, aes(x = Date, y = Wind)) + geom_line(color = "green", size = 1)
gline_wind
# line chart for Solar.R
gline_solar <- ggplot(df, aes(x = Date, y = Solar.R)) + geom_line(color = "purple", size = 1)
gline_solar
# one chart with ozone, temp, wind, and solar.r combined
## create a scale function for drawing 4 lines
scale <- function(v){
  min <- min(v)
  max <- max(v)
  v1 <- (v-min)/(max-min)
  return (v1)
}
## scale the first four columns of our df into a vector
vec <- sapply(df[,1:4], scale)
## unlist the date column
date <- unlist(df$Date)
## combine vec and date into a new dataframe
vecdf <- data.frame(vec, date)
## reshape the data so each row has unique and can be shaped how we like in plot
datamelt <- melt(vecdf, id=c("date"))
datamelt
## Create one linechart for all 4 variables
melt_plot <- ggplot(datamelt, aes(x=date, y=value, color=variable, group=variable)) + geom_line(size = 0.2) + ggtitle('Line Chart for AirQuality')
melt_plot

# Step4: Look at all the data via a Heatmap
# heatmap w/ day on x-axis and variables on y-axis
gheat <- ggplot(datamelt, aes(x=date, y=variable)) + geom_tile(aes(fill=datamelt$value)) + scale_fill_gradient(low='green',high='red') + ggtitle('Heatmap for AirQuality')
gheat

# Step 5: Look at all the data via a scatter chart
g <- ggplot(df,aes(x=Wind,y=Temp)) + geom_point() # scatter with wind on x and temp on y
g <- g+ geom_point(aes(size=Ozone, color=Solar.R)) # add size of Ozone and color of Solar.R to our plot
g

# Step 6: Final Analysis




