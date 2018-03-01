########## IST 687 - Viz Map HW: Median Income ##########
########## Nate Hoffelmeyer ##########
########## February 27, 2018 ##########

##### Step 1: Load in the Data #####
# 1) Read the data - using the gdata package we have previously used.
EnsurePackage('gdata')
medianZip <- read.xls("~/Desktop/IST 687/MedianZIP.xls")
str(medianZip)
head(medianZip)

# 2) Clean up the dataframe
# a) Remove any info at front of file that's not needed
colnames(medianZip) <- c("zip","median","mean","population")
medianZip <- medianZip[-1, ]
head(medianZip)
# b) Update the column names to (zip,median,mean,population)
colnames(medianZip) <- c("zip", "median", "mean", "population")
head(medianZip)
# remove commas
medianZip$median <- gsub(",", "", medianZip$median)
medianZip$mean <- gsub(",", "", medianZip$mean)
medianZip$population <- gsub(",","",medianZip$population)
head(medianZip)

# 3) Load the 'zipcode' package
EnsurePackage('zipcode')
data(zipcode)
str(zipcode)
head(zipcode)

# get some clean zipcodes
medianZip$zip <- clean.zipcodes(medianZip$zip)
medianZip
zipcode[1, ]

# 4) Merge the zip code information from the two data frames (merge into one dataframe)
mergedZip <- merge(medianZip, zipcode, by="zip")
head(mergedZip)
str(mergedZip)
# clean the new dataframe
mergedZip$median<-as.numeric(mergedZip$median)
mergedZip$mean<-as.numeric(mergedZip$mean)
mergedZip$population<-as.numeric(mergedZip$population)
str(mergedZip)


# 5) Remove Hawaii and Alaska (just focus on the 'lower 48' states)
mergedZip <- subset(mergedZip,mergedZip$state != "AK")
EnsurePackage('sqldf')
sqldf("select * from mergedZip where state = 'AK'") #check you get 0 results :)
mergedZip <- subset(mergedZip,mergedZip$state != "HI")
sqldf("select * from mergedZip where state = 'HI'") #check you get 0 results :)


##### Step 2: Show the income & population per state #####
# 1) Create a simpler dataframe, with just the average median income and the population for each state.
avgmedianDf <- tapply(as.numeric(mergedZip$median),mergedZip$state,mean)
sumPop <- tapply(as.numeric(mergedZip$population),mergedZip$state,sum)
custDf <- data.frame(avgmedianDf,sumPop)
head(custDf)

# 2) Add the state abbreviations and the state names as new columns (make sure the state names are all lower case)
stateABB <- sort(unique(mergedZip$state))
custDf$state <- stateABB
custDf$stateNames <- state.name[match(custDf$state,state.abb)]
custDf$stateNames <- tolower(custDf$stateNames)
head(custDf)

# 3) Show the U.S. map representing the color with the average median income of that state
EnsurePackage('ggplot2')
EnsurePackage('ggmap')
dummyDF <- data.frame(state.name,stringsAsFactors = FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map.simple <- ggplot(dummyDF, aes(map_id = state)) # create a ggplot on the dummyDF and the map we want to use
map.simple <- map.simple + geom_map(map = us,fill="white", color="black") # use geom_map type (i.e. line, bar, etc) and style
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat) # expand map limits based on long and lat size
map.simple <- map.simple + coord_map() + ggtitle("basic map of USA") # coord_map makes sure it doesn't stretch
map.simple
map.income <- map.simple + geom_map(data=custDf,map=us,aes(fill=avgmedianDf,map_id=stateNames),color="black",na.rm=TRUE)
map.income + ggtitle("Average Median Income by State")

# 4) Create a second map with color representing the population of the state
map.pop <- map.simple + geom_map(data=custDf,map=map_data("state"),aes(fill=sumPop,map_id=stateNames),color="black",na.rm=TRUE)
map.pop + ggtitle("States by Population")

##### Step 3: Show the income per zip code #####
# 1) Draw each zip code on the map, where the color of the 'dot' is based on median income. Have the background black.
map.simple2 <- ggplot(dummyDF, aes(map_id = state)) # create a ggplot on the dummyDF and the map we want to use
map.simple2 <- map.simple2 + geom_map(map = us,fill="black", color="white") # use geom_map type (i.e. line, bar, etc) and style
map.simple2 <- map.simple2 + expand_limits(x = us$long, y = us$lat) # expand map limits based on long and lat size
map.simple2 <- map.simple2 + coord_map() + ggtitle("basic map of USA") # coord_map makes sure it doesn't stretch
map.simple2
mergedZip2 <- mergedZip
map.zip <- map.simple2 + geom_point(data=mergedZip2,aes(x = mergedZip2$longitude, y = mergedZip2$latitude, color=median),show.legend = F)
map.zip + ggtitle("Income per Zip Code")

##### Step 4: Show Zip Code Density #####
# 1) Now generate a different map, where we can easily see where there are lots of zip codes, and where there are few, using stat_density2d
map.density <- map.simple2 + stat_density2d(aes(x=mergedZip2$longitude, y=mergedZip2$latitude), data=mergedZip2, geom="density2d") + scale_alpha(range=c(0.00,0.25)) + ggtitle("Density for all Zip codes in USA") + theme(plot.title=element_text(lineheight=3.5,face="bold"))
map.density

##### Step 5: Zoom in to the region around NYC #####
# 1) Repeat Steps 3 & 4 but have the image / map be of the norhteast U.S. (centered around New York)
zoom.geo <- geocode("New York, ny")
zoomAmount <- 6
middlex <- zoom.geo$lon
middley <- zoom.geo$lat
ylimit <- c(middley-zoomAmount, middley+zoomAmount)
xlimit <- c(middlex-zoomAmount, middlex+zoomAmount)

map.zip + coord_cartesian(x = xlimit, y = ylimit)

map.density + coord_cartesian(x = xlimit, y = ylimit)

















