## Unstructured to Structured

# In this code, we create a corpus, clean the corpus and 
# Implement tokenization by creating DTM

# clear the environment
rm(list= ls())
# check and set working directory

setwd("C:/practice data")
require(data.table)   # for faster data operations

tweet <- fread("TweetsDataSet.csv", stringsAsFactors = F)

require(tm)           # for text mining

# reading the data set from current working directory
tweet <- fread('TweetsDataSet.csv', stringsAsFactors = F)

# checking structure and summary
dim(tweet)
str(tweet)

summary(tweet)

head(tweet)
tail(tweet)

## Preprocessing the tweets and cleaning the corpus

# user defined variables and functions

# Function for taking in the vector from tweet data set and do all preprocessing steps below:
# preprocessing steps- Case folding; Remove numbers, words and punctuation and perform stemming and stripping extra whitespaces


# vec2clean.corp function takes two arguments: x = vector to be cleaned, y = document number to show the process by printing
vec2clean.corp <- function(x){
  
  # As there are many languages used in the data, we consider stopwords of all the languages
  a = c(stopwords("danish"),stopwords("dutch"),stopwords("english"),
        stopwords("finnish"),stopwords("french"), stopwords('SMART'),
        stopwords("german"),stopwords("hungarian"),stopwords("italian"),
        stopwords("norwegian"),stopwords("portuguese"),stopwords("russian"),
        stopwords("spanish"),stopwords("swedish"))
  
  # Function to replace ' and " to spaces before removing punctuation to avoid different words from binding 
  AposToSpace = function(x){
    x= gsub("'", ' ', x)
    x= gsub('"', ' ', x)
    x =gsub('break','broke',x) # break may interrupt control flow in few functions
    return(x)
  }
  
  x = Corpus(VectorSource(x))
  x = tm_map(x, tolower)
  x = tm_map(x, removeNumbers)
  x = tm_map(x, removeWords, a)
  x = tm_map(x, AposToSpace)
  x = tm_map(x, removePunctuation)
  x = tm_map(x, stemDocument)
  x = tm_map(x, stripWhitespace)

  return(x)
  
}

# Calling the vec2clean.corp with tweets(x) and we desire to check the progress with document 3(y)

corp <- vec2clean.corp(tweet$tweet)
corp
writeLines(as.character(corp[[3]]))

# Creating Document Term Matrix from the corp with TF weighting
dtm <- DocumentTermMatrix(corp, control = list(weighting = weightTf))
dtm
# dtm has (documents: 91298, terms: 30554) with 100% sparsity


# Removing Sparse term and take out those words which are more relevant
sparse.dtm <- removeSparseTerms(dtm, 0.9982 )
sparse.dtm
# sparse.dtm has (documents: 91298, terms: 783)

# converting Document term matrix to a Data frame
tweets.df <- data.frame(as.matrix(sparse.dtm)) # tweets.df has 442 features(0.9982 sparsity)


# saving the necessary data structures to use in next segments
save(vec2clean.corp, dtm, file = 'step1_DS.dat')

