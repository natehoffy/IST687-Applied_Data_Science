########## IST 687 - Text Mining HW: Median Income ##########
########## Nate Hoffelmeyer ##########
########## March 21, 2018 ##########

# Load necessary packages
library(tm)
library(wordcloud)
library(reshape)

# Read in AFINN
afinn <- "~/Desktop/IST 687/AFINN.txt"
p <- scan(afinn, character(0), sep = "\n")
head(p)
str(p)
p <- data.frame(p, stringsAsFactors = F)
p <- data.frame(do.call(rbind, strsplit(as.vector(p$p), split = "\t")))
names(p) <- c("Word","Score")
head(p)
str(p)

# Read in MLK Speech
mlk <- readLines("~/Desktop/IST 687/MLK_Speech.txt")  # read in text file "MLK"
str(mlk)
mlk[1]

# Build corpus of speech
words.vec <- VectorSource(mlk)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))

# Create a term-document matrix
tdm <- TermDocumentMatrix(words.corpus)
tdm

words <- as.matrix(tdm)
str(words)

# Calculate total number of words
wordCounts <- rowSums(words)
wordCounts <- sort(wordCounts, decreasing = T)
head(wordCounts)

df <- data.frame(word = names(wordCounts), freq = wordCounts, stringsAsFactors=FALSE)

overall_score <- 0 # Initialize to zero each time before running for loop

overall_score

str(p)
length(p$Word)

str(df)

for(i in 1:length(df$word))
{
  for(j in 1:length(p$Word))
  {
    if(df$word[i] == p$Word[j])
    {
      overall_score = overall_score + (as.numeric(df$freq[i]) * as.numeric(p$Score[j]))
    }
  }
}

print(paste("Overall Score: ",overall_score))

##################
# create a dummy df to store the values we need, since we need two values returned
dummyDF <- data.frame(c("a"))

# Create a function to do what we did above multiple times
posFunction <- function(mlk)
{
  
  words.vec <- VectorSource(mlk) # To treate each component of a vectoe as a document
  words.corpus <- Corpus(words.vec) # Creating the corpus(heart) of the document
  
  # Removing stop words 
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  
  
  tdm <- TermDocumentMatrix(words.corpus) # Does what it says. Its also called Document Term Matrix
  
  words <- as.matrix(tdm) # Matrix: It's easy to compute with matrix in this case
  
  
  wordCounts <- rowSums(words) # Number of Rows: Duh
  wordCounts <- sort(wordCounts, decreasing = T) # this will give you the major stock words, but we dont need this cause we already removed stock words
  
  
  
  df <- data.frame(word = names(wordCounts), freq = wordCounts, stringsAsFactors=FALSE) # Creating a df which has all the words and their frequency
  # Creating a word Cloud
  
  overall_score <- 0
  
  
  
  
  for(i in 1:length(df$word))
  {
    for(j in 1:length(p$Word))
    {
      if(df$word[i] == p$Word[j])
      {
        overall_score = overall_score + (as.numeric(df$freq[i]) * as.numeric(p$Score[j]))
      }
    }
  }
  
  print(paste("Overall Score: ",overall_score))
  
  
  dummyDF <- data.frame(overall_score)
  return(dummyDF)
  
}

posFunction(mlk)

len <- round(length(mlk)/4) # Dividing the Speech into 4 parts
l = 1 # set lower limit of the loop
dummyDF1 = data.frame(c("a"))

# another dummy df, but in fours for 4 values and that we'll plot later
x = data.frame(c("1"),c("2"),c("3"),c("4"))
for(i in 1:4) # loop to get us the four times score for each portion of the text
{
  lower = l
  upper = l + len
  x[i] <- posFunction(mlk[lower:upper])
  #dummyDF1[i] <- x[i]
  l = upper
}

x$Para1
str(x)
colnames(x) <- c("Para1","Para2","Para3","Para4") # Renaming the columns

finaldf = data.frame(  c("1st","2nd","3rd","4th"), c(x$Para1,x$Para2,x$Para3,x$Para4))
colnames(finaldf) = c("Para","value")

# Plot the results
barplot(finaldf$value ,names = finaldf$Para ,xlab = "Quarter" ,ylab = "Score")
