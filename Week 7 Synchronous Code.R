#
#
#
#      pre-cursor functions,,,readCensus, Numberize 
#
#      read in the census data set
#
readCensus <- function() {
  urlToRead <-"http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  
  #read the data from the web
  testFrame <- read.csv(url(urlToRead))
  
  #remove the first 8 rows (âheader informationâ)
  testFrame<-testFrame[-1:-8,]
  
  #only keep the first 5 columns
  testFrame<-testFrame[,1:5]
  
  #rename the first column
  testFrame$stateName <- testFrame[,1]
  testFrame<-testFrame[,-1]
  
  #remove the last rows (tail info)
  testFrame<-testFrame[-52:-58,]
  
  #remove the âdotâ from the state name
  testFrame$stateName <- gsub("\\.","", testFrame$stateName)
  
  #convert the columns to actual numbers and rename columns
  testFrame$april10census <-Numberize(testFrame$X)
  testFrame$april10base <-Numberize(testFrame$X.1)
  testFrame$july10pop <-Numberize(testFrame$X.2)
  testFrame$july11pop <-Numberize(testFrame$X.3)
  testFrame <- testFrame[,-1:-4]
  
  #remove the old rownames, which are now confusing
  rownames(testFrame) <- NULL
  
  return(testFrame)
}
#
Numberize <- function(inputVector)
{
  # Get rid of commas
  inputVector<-gsub(",","", inputVector)
  # Get rid of spaces
  inputVector<-gsub(" ","", inputVector)
  
  return(as.numeric(inputVector))
}

#  Packages: maps, zipcode, mapproj, ggmap, ggplot2, gdata

#specify the packages of interest
packages=c("maps","zipcode","mapproj","ggmap","ggplot2","gdata")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()


#
#      state.name  - R data set
#
str(state.name)
head(state.name)
state.name[3]
#
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
dummyDF[3,]
dummyDF$state[3]
#
us <- map_data("state")       ## map_data is a function in ggplot2 package
str(us)
us[1,]
us[1000,]
#
map.simple <- ggplot(dummyDF, aes(map_id = state))
map.simple <- map.simple +
  geom_map(map = us, fill="light blue", color="black")
map.simple
map.simple <- map.simple +
  expand_limits(x = us$long, y = us$lat)
map.simple
#
#
#
map.simple <- map.simple +
  coord_map() + ggtitle("basic map of USA")
map.simple
#

# ???  map.simple + geom_point(aes(x = -100, y = 30))

dfStates <- readCensus()
str(dfStates)
dfStates$state <- tolower(dfStates$stateName)
#
#
map.popColor <- ggplot(dfStates, aes(map_id = state))
map.popColor <- map.popColor +
  geom_map(map = us, aes(fill=july11pop))+scale_color_gradient(low="light blue",high="dark blue")
map.popColor <- map.popColor +
  expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor +
  coord_map() + ggtitle("state population")
map.popColor
#
latlon <- geocode("syracuse university, syracuse, ny")
latlon
#
#   still using map.popColor, plotting a specific point
#
map.popColor +
  geom_point(aes(x = latlon$lon,
                 y = latlon$lat), color="darkred", size = 3)
#
latlon1 <- geocode("stanford university,stanford, ca")
latlon1
map.popColor +
  geom_point(aes(x = latlon1$lon,
                 y = latlon$lat), color="yellow", size = 3)
latlon1

#
#       data set used in HW7
#
mydata <- read.xls("MedianZIP.xlsx")
str(mydata)
head(mydata)


# clean up the data
# change column names to "zip", "Median","Mean", and "Population"
#
colnames(mydata) <- c("zip", "Median", "Mean", "Population")
#
# delete the first row of the dataframe
#
mydata <- mydata[-1,]
head(mydata)

# gsub function is used to perform replacement of matches determined by regular expression matching
# in this case gsub replace all "," in column "Median" with nothing ("")
#
mydata$Median <- gsub(",", "", mydata$Median)
# delete the "," in column "Mean" (replace all "," in column "Mean" with nothing)
mydata$Mean <- gsub(",", "", mydata$Mean)
# delete the "," in column "Population" (replace all "," in column "Population" with nothing)
mydata$Population <- gsub(",","",mydata$Population)
head(mydata)

data(zipcode)
str(zipcode)
head(zipcode)
#
#  clean.zipcodes   https://www.rdocumentation.org/packages/zipcode/versions/1.0/topics/clean.zipcodes
#
mydata$zip <- clean.zipcodes(mydata$zip)
mydata
zipcode[1,]
#
#    merge(mydata, zipcode, by="zip") into a new df  dfNew
#
head(mydata)
head(zipcode)
#
dfNew <- merge(mydata, zipcode, by="zip")
str(dfNew)
head(dfNew)
#
dfNew$Median<-as.numeric(dfNew$Median)
dfNew$Population<-as.numeric(dfNew$Population)
str(dfNew)
#
income <- tapply(dfNew$Median, dfNew$state, mean) # calc mean of median by state
str(income)
head(income)
#
state <- rownames(income)  # place rownames from income into state variable
head(state)
#
#   mean Median Income by State
#
medianIncome <- data.frame(state, income) # create a df with state variable & income variable
str(medianIncome)
head(medianIncome)
#
pop <- tapply(dfNew$Population, dfNew$state, sum ) # sum up population for each state
str(pop)
head(pop)
state <- rownames(pop)                        # same content as earlier
#
statePop <- data.frame(state, pop)            # create new df statePop
#
dfSimple <- merge(medianIncome, statePop, by="state")  # create new df by merging df's medianIncome, staeIncome
str(dfSimple)
head(dfSimple)
#
#    R data set - state.abb
#
str(state.abb)
head(state.abb)
#
match(dfSimple$state,state.abb)     # the relative position of state.abb in dfSimple$state
dfSimple$state
state.abb[45]
state.name[9]
state.name[45]
state.name[c(9,45)]
#
#   bring in full state name from abbreviated state name (openintro in HW7)
#
dfSimple$stateName <- state.name[match(dfSimple$state,state.abb)]
str(dfSimple)
head(dfSimple)

dfSimple$stateName <- tolower(dfSimple$stateName)
head(dfSimple)
#
# us <- map_data("state")   # performed above, not adding anything new
#
mapIncome <- ggplot(dfSimple, aes(map_id = stateName))
mapIncome <- mapIncome + geom_map(map = us, aes(fill = dfSimple$income))
mapIncome <- mapIncome + expand_limits(x = us$long, y = us$lat)
mapIncome <- mapIncome + coord_map()
mapIncome <- mapIncome + ggtitle("average median Income of the U.S")
mapIncome
#
#
#   bring in full state name from abbreviated state name (openintro in HW7)
#
head(dfNew)
dfNew$stateName <- state.name[match(dfNew$state,state.abb)]
dfNew$stateName <- tolower(dfNew$stateName)
head(dfNew)
#
#mapZip <- ggplot(dfNew, aes(map_id = stateName))
#mapZip <- mapZip + geom_map(map=us, fill="black", color="white")
#mapZip <- mapZip + expand_limits(x =us$long, y = us$lat)
#mapZip <- mapZip + geom_point(data = dfNew,aes(x = dfNew$longitude, y = dfNew$latitude, color=dfNew$Median))
#mapZip <- mapZip + coord_map() + ggtitle("Income per zip code")
#mapZip
#
#
# use dfNew to create map and set "stateName" as map ID
#
#      remove al & HI from dfNew$state
#
# dfNew$state<-dfNew[dfNew$state!="AK" & dfNew$state!="HI",]
#
mapZip <- ggplot(dfNew, aes(map_id = stateName))
# set the backgroud color to be black and line color to be white
mapZip <- mapZip + geom_map(map=us, fill="black", color="white")
# change the limits of x and y axes to print the whole map
mapZip <- mapZip + expand_limits(x =us$long, y = us$lat)
# plot points on map each "dot" represent a zip code and the color of "dots" is based on median income
mapZip <- mapZip + geom_point(data = dfNew,aes(x = dfNew$longitude, y = dfNew$latitude, color=dfNew$Median))
# make sure the map is not stretched and add a title for the map
mapZip <- mapZip + coord_map() + ggtitle("Income per zip code")
# plot the map
mapZip
#
#     subsetting map
#
latlon <- geocode("NYC, ny")
mapZipZoomed <-  mapZip + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 3)
mapZipZoomed <-  mapZipZoomed + xlim(latlon$lon-10, latlon$lon+10) + ylim(latlon$lat-10,latlon$lat+10) + coord_map()
mapZipZoomed
