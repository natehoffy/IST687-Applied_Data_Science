########## Blazing Fast Exploratory Data Analysis ##########
# Installation and Loading
# Install the DataExplorer package if it doesn't exist
EnsurePackage('DataExplorer')

# Dataset
# Read in the Chocolate Bar Ratings data set from Kaggle (https://www.kaggle.com/rtatman/chocolate-bar-ratings)
choco = read.csv('~/Desktop/IST 687/flavors_of_cacao.csv', header = T, stringsAsFactors = F)

# Data Cleaning
# Cocoa.Percent is supposed to be a numeric, but is read as a char due to %, so needs fixing
choco$Cocoa.Percent = as.numeric(gsub('%','',choco$Cocoa.Percent))
choco$Review.Date = as.character(choco$Review.Date)

# Variables
# The first thing we will do in EDA is checking the dimension of the input dataset and the time of variables.
plot_str(choco)

# Missing Values
# Are there any missing values? Fortunately NO :)
plot_missing(choco)

# Continuous Variables
# Analyze represent continuous vars with histogram
plot_histogram(choco)
# Maybe you're a fan of density plots --> we can do that too!
plot_density(choco)

# Multivariate Analysis
# Correlation
plot_correlation(choco, type = 'continuous','Review.Date')

# Categorical variables
plot_bar(choco)

# Create a nice sharable report
create_report(npsdataCL_Top3_Test)




