#######################
# Week 5 - Storage Wars
#######################

# work with gdata package to read excel information from a website
# Use install.packages() and library() functions to prepare gdata packages for use

install.packages("gdata")

library("gdata")

# now we can use read.xls() function from the gdata package to read our excel data
# check out  documentation here - http://cran.r-project.org/web/packages/gdata/gdata.pdf

#read in census data
testFrame<-read.xls("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.xls")

# let's take a look at what we got by using str() to create a summary of the structure of testFrame:
str(testFrame)

#####################
# clean the dataframe

# eliminate header rows we do not need
testFrame <- testFrame[-1:-8,]
# reminder that the minus sign inside brackets refers to rows to be eliminated
# leave column designator empty to keep all columns for now

# keep the first 5 columns and eliminate the rest (since others are blank / empty)
testFrame <- testFrame[,1:5]

# tail() function shows us last 10 rows contain notes we don't need --> eliminate them
testFrame <- testFrame[-52:-62,]

# rename the first column to stateName
testFrame$stateName <- testFrame[,1]
# We’ve used a little hack here to avoid typing out the ridiculously long name of that first variable/column 
# We’ve  simply copied the data from col 1 into a new column entitled "stateName."

# now let's eliminate the original column 1 that has the long name
testFrame <- testFrame[,-1]

# install the stringr package, and load it
install.packages("stringr")
library("stringr")

# next let's remove the dots from in from of state names
testFrame$stateName <- str_replace(testFrame$stateName,"\\.","")
# if error, install stringr package and load the library and try again
# can also use gsub, but gsub replaces each occurrence where str_replace does only first occurrence


# use Numberize to convert strings to numbers where needed, remove commas & other junk
# create the Numberize function
Numberize <- function(inputVector) {
  # Get rid of commas
  inputVector<-gsub(",","", inputVector)
  # Get rid of spaces
  inputVector<-gsub(" ","", inputVector)
  
  return(as.numeric(inputVector))
}

# run it a few times to create new vectors / remove old columns with bad formatting
testFrame$april10census <- Numberize(testFrame$X)
testFrame$april10base <- Numberize(testFrame$X.1)
testFrame$july10pop <- Numberize(testFrame$X.2)
testFrame$july11pop <- Numberize(testFrame$X.3)
testFrame <- testFrame[,-1:-4]

######################
# Accessing a Database

# install RMySQL package
install.packages("RMySQL")
library("RMySQL")

# connect db to R
con <- dbConnect(dbDriver("MySQL"), dbname="test")

# see what tables we have accessible to us
dbListTables(con)

# create a table with the census data
dbWriteTable(con,"census",testFrame,overwrite=TRUE)

# check the list to see if table was added
dbListTables(con)

# now we can run an SQL query on our table
dbGetQuery(con,"SELECT stateName, july11pop FROM census WHERE july11pop<1000000")

#############################################
# Comparing SQL and R for accessing a dataset

# install and library the sqldf package
install.packages("sqldf")
library("sqldf")

#sqldf allows us to us a dataframe as a database
sqldf("select avg(april10base) From testFrame")

mean(testFrame$april10base)

sqldf("select stateName From testFrame Where july11pop<1000000")

# grouping practice using 'tapply'
# first let's add regions using state.region
# remove D.C. then run the thing to add the region column
testFrame <- testFrame[testFrame$stateName != "District of Columbia",]
testFrame$region <- state.region

# determine average population for each region based on the states in a specific region
sqldf("select AVG(july11pop) From testFrame Group by region")

# in R we can do the above with tapply
tapply(testFrame$april10base, testFrame$region, mean)

# let's store the region mean for each state
# we'll do this by storing results of tapply and using them
# first store region means in regionMean variable, and region names in regionName variable
regionMean <- tapply(testFrame$april10base, testFrame$region, mean)
regionMean
regionNames <- names(regionMean)
regionNames

# identify rows that have region using which
which(regionNames[1] == testFrame$region)
which(regionNames[regionNames=="Northeast"] == testFrame$region)

# Either way, we get appropriate rows & use that info to define those rows to have the region mean of northeast
# We can do similar commands for the other regions.
testFrame$regionMean <- 0
testFrame$regionMean[which(regionNames[1] == testFrame$region)] <- regionMean[1]
testFrame$regionMean[which(regionNames[2] == testFrame$region)] <- regionMean[2]
testFrame$regionMean[which(regionNames[3] == testFrame$region)] <- regionMean[3]
testFrame$regionMean[which(regionNames[4] == testFrame$region)] <- regionMean[4]

# must be a way to do above without copy paste --> use for loop
for (x in 1:4) {
  indexes <- which(regionNames[x] == testFrame$region)
  testFrame$regionMean[indexes] <- regionMean[x]
}

# for loops can sometimes add more code and be less efficient in R. example:
a <- c(10:19)
b <- c(20:29)
c <- a+b
c

for (x in 1:10) {
  c[x] <- a[x] + b[x]
}
c


#####################
# Accessing JSON Data

# install and load RCurl and RJSONIO
install.packages("RCurl")
library("RCurl")
install.packages("RJSONIO")
library("RJSONIO")

# next create helper function to take address field and make it url we need:
MakeGeoURL <- function(address) {
  root <- "http://maps.google.com/maps/api/geocode/"
  url <- paste(root,"json?address=", address, "&sensor=false", sep = "")
  return(URLencode(url))
}

MakeGeoURL("1600 Pennsylvania Avenue, Washington, DC")

# now we use this in another function to actually get data from Google's API
Addr2latlng <- function(address) {
  url <- MakeGeoURL(address)
  apiResult <- getURL(url)
  geoStruct <- fromJSON(apiResult, simplify = FALSE)
  lat <- NA
  lng <- NA
  try(lat <- geoStruct$results[[1]]$geometry$location$lat)
  try(lng <- geoStruct$results[[1]]$geometry$location$lng)
  return(c(lat, lng))
}

# let's try it out
testData <- Addr2latlng("1600 Pennsylvania Avenue, Washington, DC")
str(testData)

# let's try with an even larger JSON dataset
# load the JSON dataset with the following code:
bikeURL <- "https://feeds.citibikenyc.com/stations/stations.json"
apiResult <- getURL(bikeURL)
results <- fromJSON(apiResult)
length(results)
# note that the results are lists, not vecotrs --> must use double brackets [[]] when working with

# the first item in the list describes when the web page was generated
when <- results[[1]]
when

# the next results is actually a list of stations
stations <- results[[2]]
length(stations)

# let's look at one of the stations
str(stations[[1]])

# we will now convert the station list to datafram
# first get number of rows in the list, and the names of all the attributes at each station (which will be our col names)
numRows <- length(stations)
nameList <- names(stations[[1]])

# next create a df, unlisting the list and putting elements back in using structure of the matrix
dfStations <- data.frame(matrix(unlist(stations),nrow=numRows,byrow=T),stringsAsFactors=FALSE)

# name columns appropriately
names(dfStations) <- nameList

# look at newly created df
str(dfStations)

# R thinks all cols are characters, but some are actually numbers. let's clean this up
dfStations$availableDocks <- as.numeric(dfStations$availableDocks)
dfStations$availableBikes <- as.numeric(dfStations$availableBikes)
dfStations$totalDocks <- as.numeric(dfStations$totalDocks)

# play with / look at some of the data
mean(dfStations$availableDocks)
mean(dfStations$availableBikes)

bikesAvailDF <- dfStations[dfStations$availableBikes>0,]
nrow(bikesAvailDF)







