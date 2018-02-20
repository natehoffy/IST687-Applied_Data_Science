# Load a sample dataset into R
load("~/Desktop/IST 687/nps_subset.RData")

# View the dataset
View(DT_exp_final_set)

# Dataframe it for ease of use w/ the naming convention
npsdata <- data.frame(DT_exp_final_set)
npsdata[1:5,]
summary(npsdata)
str(npsdata)
ncol(npsdata) # verify number of columns in the df
nrow(npsdata) # verify number of rows in the df

#********************************************* Null to NA *********************************************
nullToNA <- function(npsdata){
  #split df into numeric & non-numeric functions
  a <- npsdata[,sapply(df, is.numeric), drop = FALSE]
  b <- npsdata[,sapply(df, Negate(is.numeric)), drop = FALSE]
  # Change empty strings to NA
  b <- b[lapply(b,function(x) levels(x) <- c(levels(x),NA)),] #add NA level
  b <- b[lapply(b,function(x) x[x=="",]<-NA),] #change Null to NA
  #Put the columns back together
  d<-cbind(a,b)
  d[,names(npsdata)]
}
nullToNA(npsdata) #run the function above on the df
is.null(npsdata) # check if there are any null values
View(npsdata) # look and see that nulls were converted to NA
nrow(npsdata) # count rows to make sure nothing was deleted
#*********************************************
npsdataCL<-npsdata[complete.cases(npsdata$Likelihood_Recommend_H),] #filter out incomplete data for likelihood to recommend
View(npsdataCL$Likelihood_Recommend_H) #look at the likelihood to recommend data for sanity check
nrow(npsdataCL) # see how many rows (observations) exist after filtering to only complete likelihood cases
any(is.na(npsdataCL$Likelihood_Recommend_H)) #are there any NA remaining in likelihood to recommend?

# Not sure if lines 37-43 are needed
install.packages("NPS")
library("NPS")
npsdataNPS<-nps(npsdataCL$Likelihood_Recommend_H, breaks=list(0:6,7:8,9:10))
npsdataNPS
str(npsdataCL)
# Not sure if lines 37-43 are needed

# count up the likelihood to recommend scores, grouped by state, and display list in descending order of the counts (states with most count on top)
sqldf("select State_PL,count(Likelihood_Recommend_H) from npsdataCL group by State_PL order by count(Likelihood_Recommend_H) desc")

# save the data set to a text file so others can load it into their local instance
write.table(npsdataCL, "~/Desktop/IST 687/npsdataCL.txt", sep="\t")

# take all data from the 3 states with the highest counts of complete likelihood to recommend survey scores.
npsdataCL_Top3 <- sqldf("select * from npsdataCL where State_PL = 'California' or State_PL = 'Texas' or State_PL = 'Florida'")
View(npsdataCL_Top3) # view the df for sanity check

# save the new data set to a text file so others can load it into their local instance for group manipulation
write.table(npsdataCL_Top3, "~/Desktop/IST 687/npsdataCL_Top3.txt", sep="\t")


" Multi-line commented out code follows -- ignore everything below this line for now *******************

remove(npsdata3) 
remove(Numberize)
# omit the NAs, store in a new df, review the impact
npsdata3 <- na.omit(npsdata)
npsdata3


# Check for nulls and NAs
any(is.null(npsdata3$Survey_ID_H))
any(is.na(npsdata3$Survey_ID_H))

any(is.na(npsdata1$e_hy_gss_tier_I))
length(npsdata1$e_hy_gss_tier_I[npsdata1$e_hy_gss_tier_I=='NA'])

# Numberize variables
Numberize <- function(inputVector)
{
  # Get rid of commas
  inputVector<-gsub(",","", inputVector)
  # Get rid of spaces
  inputVector<-gsub(" ","", inputVector)
  inputVector<-gsub(".","", inputVector)
  inputVector<-gsub("*","", inputVector)
  
  return(as.numeric(inputVector))
}

npsdata2$NT_RATE_R <- Numberize(npsdata2$NT_RATE_R)

# remove columns
npsdata3 <- npsdata3[,-76:-117] #removal of columns 76 - 117, NPS_Type
npsdata3 <- npsdata3[,-71:-74] #removal of columns 71 - 74, Guest NPS Goal_PL
npsdata3 <- npsdata3[,-52:-68] #removal of columns 52 - 69, US_Region_PL
npsdata3 <- npsdata3[,-42:-49] #removal of columns 42 - 49, Likelihood_Recommend_H & Overall_Sat_H
npsdata3 <- npsdata3[,-39] #removal of 39, Gender_H & Age_Range_H Guest_State
npsdata3 <- npsdata3[,-34:-36] #removal of columns 34 - 36, Survery_ID_H
npsdata3 <- npsdata3[,-27:-32] #removal of columns 27 - 32, Length_Stay_H
npsdata3 <- npsdata3[,-21:-25] #removal of columns 21 - 25, REVENUE_USD_R_26
npsdata3 <- npsdata3[,-1:-19] #removal of columns 1 - 19, NT_Rate_R_20
str(npsdata3)

View(npsdata3)

npsdata3 <- npsdata3[,-5] #removal of guest_state_h
npsdata3 <- npsdata3[,-5] #removal of Gender_H

"



