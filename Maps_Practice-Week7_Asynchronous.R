# ******************** Maps ********************
EnsurePackage("ggmap") # ensure we have ggmap package installed using our cool function
us <- map_data("state") # get the map of the usa

# First, get state names from ggmap to make a dummy dataframe
dummyDF <- data.frame(state.name,stringsAsFactors = FALSE)
dummyDF$state <- tolower(dummyDF$state.name) # ggplot needs lowercase state names, so we use tolower

# We use ggplot and map geometry to make a map of our dummyDF
map.simple <- ggplot(dummyDF, aes(map_id = state)) # create a ggplot on the dummyDF and the map we want to use
map.simple <- map.simple + geom_map(map = us,fill="white", color="black") # use geom_map type (i.e. line, bar, etc) and style
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat) # expand map limits based on long and lat size
map.simple
map.simple <- map.simple + coord_map() + ggtitle("basic map of USA") # coord_map makes sure it doesn't stretch
map.simple

# Let's try to overlay our state map with census data
dfStates <- readStates()
View(dfStates)
dfStates$state <- tolower(dfStates$stateName) # make sure everything is lowercase for use w/ ggplot
map.popColor <- ggplot(dfStates,aes(map_id=state)) # create the map
map.popColor <- map.popColor + geom_map(map = us, aes(fill=Jul2011)) # fill it with the population data
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat) # expand to fit the lat and long of the us
map.popColor <- map.popColor+ coord_map() + ggtitle("state population") # give it a title
map.popColor

# Showing points on a map
map.simple + geom_point(aes(x = -100, y = 30)) #start by hardcoding the latitude & longitude of specific spot
# now we'll use geocode to find a logical location (like an address)
latlon <- geocode("syracuse, ny")
latlon
# we can use the x (longitude) and y (latitude) coordinates from laton to draw a point on our map at logical location
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size=3)
# adding a second point takes a bit more work:
l <- data.frame(latlon)
latlon <- geocode("colorado")
l[2,] <- latlon
l[3,] <- geocode("denver, colorado")
map.simple + geom_point(data=l,aes(x = lon, y = lat)) # gives error because we need a dummy state column even though we aren't using
l$state <- "?"
map.popColor + geom_point(data=l,aes(x = lon, y = lat), alpha = .5, color="darkred", size = 3)

# more advanced mashup using opendata 500 list of companies leveraging open data
urlFile <- "http://www.opendata500.com/us/download/us_companies.csv" # variable to read a url
od.companies <- read.csv(url(urlFile)) # read into a df the url above
str(od.companies) # take a look at our new df
# we see a lot of columns -> let's just focus on city & state
od.companies <- od.companies[od.companies$city != " ",] # make sure city wasn't left blank
# next we'll clean up the state abbreviations, removing D.C. and fixing KA to KS
od.companies$state <- as.character(od.companies$state) 
od.companies <- od.companies[od.companies$state != "DC",]
od.companies$state <- ifelse(od.companies$state == "KA", "KS", od.companies$state)
od.companies$cityState <- paste(od.companies$city, od.companies$state) # create a new column that combines the city and state
od.companies$geoCode <- geocode(od.companies$cityState) # pass new column into geocode function
#remove na
od.companies <- na.omit(od.companies)
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1) # show the locations on our maps
bad <- od.companies[od.companies$geoCode$lon > 0, ] # variable showing the bad lon location city from above
bad$cityState # run the var
# take only companies with < 0 lon
od.companies <- od.companies[od.companies$geoCode$lon < 0, ]
od.companies <- na.omit(od.companies) # remove NAs if you get any from filtering to < 0 lon companies
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1)
# Let's make the map more useful by setting dot size to company size
# Company size is a factor - so we must tell R order of the factors
od.companies$sizes <- factor(od.companies$full_time_employees, levels = c("1-10","11-50","51-200","201-500","501-1,000", "1,001-5,000", "5,001-10,000", "10,001+"))
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat, size=sizes), shape=1)

# "We can double encode the size of the company – have both the size of the
# circle and the color of the circle both represent the size of the company. In order to
# do this, we will need to use the ‘RColorBrewer’ package (to get a good range of colors)"
library(RColorBrewer)
myColors <- brewer.pal(length(levels(od.companies$sizes)),"Reds")
names(myColors) <- levels(od.companies $sizes)
myColors[1:3]
odAnalysisMap <- map.popColor + geom_point(data=od.companies,aes(x = geoCode$lon, y = geoCode$lat, color=sizes, size=sizes)) + scale_colour_manual(name ="sizeOfCompany", values = myColors) + ggtitle("Open Data Company Analysis")

# Practice zooming
odAnalysisMap + xlim(-85,-70) + ylim(35,45) + coord_map()

# Put a mark in Miami, Florida
latlon <- geocode("miami, fl")
latlon
# we can use the x (longitude) and y (latitude) coordinates from laton to draw a point on our map at logical location
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size=3)

########## An Example Map Mash-Up ##########
# Loading Data into R
cities <- c("Manhattan, NY","Boston, MA","Philadelphia, PA","Tampa, FL","Chicago, IL","Boise, ID","San Francisco, CA","Seattle, WA","Houston, TX")
bus <- c(10,7,6,5,7,3,10,7,5) #business ratings
weather <- c(5,3,6,7,3,6,10,7,2) #weather ratings
living <- c(7,6,6,7,5,4,6,8,2) #living condition ratings
city.df <- data.frame(cities,bus,weather,living)
city.df$state <- statesFake
city.df$geoCode <- geocode(cities)

map.simple + geom_point(data = city.df,aes(x=geoCode$lon,y=geoCode$lat, size=bus, color=weather))













