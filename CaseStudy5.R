# Name: Prateek Agrawal
# Computing ID: pa7sb

#***********************************************************************************************************
#
#                                           CASE STUDY-5
#
#************************************************************************************************************
library(ks)
library(RColorBrewer)
library(rgdal)
library(maptools)
library(lubridate)
library(topicmodels)
library(tm)
library(slam)
library(stringr)
source("Shapefile.R")
source("CrimeUtil.R")

set.seed(300)

city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# read a small sample of tweets
tweets <-  read.csv("C:/UVa/Data_Mining/Examples/Data/tweets_large_sample.csv", header = TRUE, stringsAsFactors = FALSE)

# reproject from degrees to meters
tweets.locations.lonlat <-  cbind(tweets$longitude, tweets$latitude)
meters.locations <-  project(tweets.locations.lonlat, proj="+init=epsg:26971")
tweets$x <-  meters.locations[,1]
tweets$y <-  meters.locations[,2]
# some tweets are posted from outside the bounds of the chicago projection and their reprojections are infinite. remove such tweets.
tweets <-  tweets[is.finite(tweets$x) & is.finite(tweets$y),] 

# filter all tweets to be in the city
points.in.boundary <-  points.within.boundary(tweets[,c("x", "y")], "+init=epsg:26971", city.boundary)
tweets <-  tweets[points.in.boundary,]

# convert timestamp string to date representation, and pull out hour, day of week, and month
tweets$timestamp <-  as.POSIXct(tweets$timestamp)
tweets$hour <-  hour(tweets$timestamp)
tweets$day.of.week <- wday(tweets$timestamp)
tweets$month <- month(tweets$timestamp)

##*************************************** Question 1 ******************************************

# plot the tweets for the city of chicago
plot(city.boundary)
points(tweets[,c("x", "y")], pch = ".", col= "Red")

# Tweets are not uniformly distributed as we can observe there is an heavy density of tweets 
# near the downtown region or very less or no tweets at the airport or south boundry of the city
# we can confirm the above using a KDE

kde.resolution.meters <- 200

# getting the x and y co-ordinates of the tweets
kde.tweets <- tweets[,c("x", "y")]

# getting estimation points of kde
kde.est.points <- get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot the kde
kde.est <- run.spatial.kde(kde.tweets, kde.est.points, 10000)
plot.spatial.kde(kde.est, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)
#
# The KDE confirm's our observation that the highet percentage of tweets come from downtown area
# of the city, probably because most of the landmarks are situated there, so more number of tourists
# and local people visiting those people tweet, as compared to residential area's of the city.
#--------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#
#
# Initial Guess:
#-----------------
# while there can be a difference between the content of the tweets between the weekdays and 
# weekends, we can compare them using a topic model method, LDA which distributes the content on basis
# of the different topics they belong to
#
##********************************************Question 2 *****************************************
#
# For Weekdays
#-------------
#
# creating a vector of tweets occuring on a weekend
wkday.tweets<- tweets$text[tweets$day.of.week==2|
              tweets$day.of.week==3|
              tweets$day.of.week==4|
              tweets$day.of.week==5|
              tweets$day.of.week==6]

# Creating a corpus for tweets on a weekday
corp.tweets.wkday <- Corpus(VectorSource(wkday.tweets))
# checking the corpus
summary(corp.tweets.wkday)

# checking the tweets inside the corpus
corp.tweets.wkday[[1]][1]
corp.tweets.wkday[[2]][1]
corp.tweets.wkday[[3]][1]
corp.tweets.wkday[[4]][1]

#  we can observe that there are many alphanumeric characters being used in each tweet
# Therefore we have to remove the irrerelavant alpha numeric characters from the corpuses

# Remove capitalization, punctuation, numbers, stopwords, white space; stem
tweet.wkday <- tm_map(corp.tweets.wkday,content_transformer(tolower))
# Create function, toSpace, using tm's content_transformer to remove some special characters
# occuring in the tweets
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
tweet.wkday <- tm_map(tweet.wkday, toSpace, "[â€œ@ðŸ˜Šš€?ðYˆ™ÿ.~#^:/]")
# due to different encoding in different laptops some symbols might change in the above
# expression, the actual expression is "â€œ@ðŸ˜Šš€?ðYˆ™ÿ.~#^:/" or look in the report and
# copy-paste, thee encoding used is 'UTF-8'

# rechecking the tweets
tweet.wkday[[1]][1]
tweet.wkday[[2]][1]
tweet.wkday[[3]][1]
tweet.wkday[[4]][1]

# removing the stopwords, and after observing the terms, there were some terms with low relevance 
# and high frequency, so removing those terms
tweet.wkday <- tm_map(tweet.wkday, removeWords, c(stopwords("english"), "chicago", "get", 
                                                  "http", "just")) 
tweet.wkday <- tm_map(tweet.wkday, stripWhitespace) # removing the white spaces
tweet.wkday <- tm_map(tweet.wkday, removeNumbers) # removing the numbers
tweet.wkday <- tm_map(tweet.wkday, removePunctuation) # removing punctuation
tweet.wkday <- tm_map(tweet.wkday, stemDocument) # Stemming the documents

# rechecking the tweets
tweet.wkday[[1]][1]
tweet.wkday[[2]][1]
tweet.wkday[[3]][1]
tweet.wkday[[4]][1]

# creating a document term matrix
tweets.dtm <- DocumentTermMatrix(tweet.wkday, control = list(weighting = weightTf))

# removing empty documents
removedoc <- row_sums(tweets.dtm, na.rm = T) # using the slam library, due to it's speed and 
# reliability over the apply function
# removedoc <- apply(tweets.dtm, 1, sum)
tweets.dtm <- tweets.dtm[removedoc >0, ]

# finding the most frequent terms in the matrix, whose frequency(>50)
findFreqTerms(tweets.dtm, lowfreq = 100)
# we go back to the document cleaning phase, and check the most frequent words again, which 
# are irrelevant

# creating a lda model
tweet.lda.wkday <- LDA(tweets.dtm, 5, control = list(seed= 300)) # creating 5 topics

# looking at the top 5 terms in each topic
terms(tweet.lda.wkday, 10)

# checking the sparcity of topic in each document
tweet.lda.wkday@gamma[1:5, ]
# we observe that all the topics have almost equal probability of occurences 

# Naming each topic as the combination of first 5 terms occuring in that topic
#------------------------------------------------------------------------------
# to perform this task, using the lda model and the document term matrix I create a matrix of 
# posterier probabilities and assign name to the topics columns
topic.wkday <- posterior(tweet.lda.wkday, tweets.dtm)$topics
# adding the column names
colnames(topic.wkday) <- apply(terms(tweet.lda.wkday, 5), 2, paste, collapse = ",")
# checking the columns and sparsity
head(topic.wkday)
#
# The topics names are:
# Topic 1: lol,love,like,amp,girl 
# Topic 2: lol,now,like,day,fuck
# Topic 3: shit,like,got,now,want
# Topic 4: feel,good,one,can,back
# Topic 5: love,time,like,come,need
#-------------------------------------------------------------------------------------------

# For Weekends
#---------------
wkend.tweets <- tweets$text[tweets$day.of.week ==1|
                                tweets$day.of.week == 7]

# now, we will create a corpus for the tweets on a weekend
corp.tweets.wkend <- Corpus(VectorSource(wkend.tweets))
# checking the corpus
summary(corp.tweets.wkend)

# checking the tweets inside the corpus
corp.tweets.wkend[[1]][1]
corp.tweets.wkend[[2]][1]
corp.tweets.wkend[[3]][1]
corp.tweets.wkend[[4]][1]

#  we can observe that there are many alphanumeric characters being used in each tweet
# Therefore we have to remove the irrerelavant alpha numeric characters from the corpuses

# Remove capitalization, punctuation, numbers, stopwords, white space; stem
tweets.wkend <- tm_map(corp.tweets.wkend,content_transformer(tolower))
# Create function, toSpace, using tm's content_transformer to remove some special characters
# occuring in the tweets
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
tweets.wkday <- tm_map(tweets.wkend, toSpace, "[â€œ@ðŸ˜Šš€?ðYˆ™ÿ.~#^:/]")

# rechecking the tweets
tweets.wkend[[1]][1]
tweets.wkend[[2]][1]
tweets.wkend[[3]][1]
tweets.wkend[[4]][1]

# removing the stopwords, and after observing the terms, there were some terms with low relevance and high freq
# so removing those terms
tweets.wkend <- tm_map(tweets.wkend, removeWords, c(stopwords("english"), "chicago", "get",
                                                    "http", "just")) 
tweets.wkend <- tm_map(tweets.wkend, stripWhitespace) # removing the white spaces
tweets.wkend <- tm_map(tweets.wkend, removeNumbers) # removing the numbers
tweets.wkend <- tm_map(tweets.wkend, removePunctuation) # removing punctuation
tweets.wkend <- tm_map(tweets.wkend, stemDocument) # Stemming the documents

# rechecking the tweets
tweets.wkend[[1]][1]
tweets.wkend[[2]][1]
tweets.wkend[[3]][1]
tweets.wkend[[4]][1]

# creating the term document matrix
tweets.dtm2 <- DocumentTermMatrix(tweets.wkend, control = list(weighting = weightTf))

# removing empty documents
removedoc <- row_sums(tweets.dtm2, na.rm = T) # using the slam library, due to it's speed and 
# reliability over the apply function
tweets.dtm2 <- tweets.dtm2[removedoc >0, ]

# finding the most frequent terms in the matrix, whose frequency(>50)
findFreqTerms(tweets.dtm2, lowfreq = 50)
# we go back to the document cleaning phase, and check the most frequent words again, which are irrelevant

# creating a lda model
tweet.lda.wkend <- LDA(tweets.dtm2, 5, control = list(seed= 300)) # creating 5 topics

# looking at the top 5 terms in each topic
terms(tweet.lda.wkend, 10)

# checking the sparcity of topic
tweet.lda.wkend@gamma[1:5, ]
# we observe that all the topics contains almost equal sparcity

# Naming each topic as the combination of first 5 terms occuring in that topic
#----------------------------------------------------------
# to perform this task, using the lda model and the document term matrix I create a matrix of 
# posterier probabilities and assign name to the topics columns
topic.wkend <- posterior(tweet.lda.wkend, tweets.dtm2)$topics
# adding the column names
colnames(topic.wkend) <- apply(terms(tweet.lda.wkend, 5), 2, paste, collapse = ",")
# checking the columns and sparsity
head(topic.wkend)
#
# The topics names are:
# Topic 1: love,need,day,got,back
# Topic 2: like,lol,fuck,girl,love
# Topic 3: lol,make,amp,like,time
# Topic 4: now,fuck,guy,work,can
# Topic 5: good,amp,like,love,one
#--------------------------
# 
# So, observing the words in topics we can infer that there is a change in the weekday and 
# weekend topic contents
#
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#
#*****************************************Question 3**********************************************************
#
# using the school shapefile
school.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/Schools/CPS_School_Locations_SY1415", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
school.points <- data.frame(school.points)
colnames(school.points) <- c("x","y")

# Rather than 50, I have used 100 meters as the distance from schools
tweet.x <- numeric(0)
tweet.y <- numeric(0)
tweet.text <- ""
samp <- data.frame(matrix(ncol = 3))
colnames(samp) <- c("x", "y", "text")
for( i in 1:nrow(school.points)){
  value <- 0
  value <- ((tweets$x - school.points$x[i])^2 + (tweets$y - school.points$y[i])^2 -(100)^2 <= 0)
  tweet.x <- tweets$x[value]
  tweet.y <- tweets$y[value]
  tweet.text <- tweets$text[value]
  samp1 <- data.frame(x= tweet.x, y=tweet.y, text= tweet.text )
  samp <- rbind(samp, samp1)
}

tweet.sample <- samp[-1, ]

# Performing the topic modelling analysis on the sample
#-------------------------------------------------------

# now, we will create a corpus for the tweets on a weekend
corp.tweets.sample <- Corpus(VectorSource(tweet.sample$text))
# inspecting the corpus
summary(corp.tweets.sample)

# checking the tweets inside the corpus
corp.tweets.sample[[1]][1]
corp.tweets.sample[[2]][1]
corp.tweets.sample[[3]][1]
corp.tweets.sample[[4]][1]

#  we can observe that there are many alphanumeric characters being used in each tweet
# Therefore we have to remove the irrerelavant alpha numeric characters from the corpuses

# Remove capitalization, punctuation, numbers, stopwords, white space; stem
tweets.sample <- tm_map(corp.tweets.sample,content_transformer(tolower))
# Create function, toSpace, using tm's content_transformer to remove some special characters
# occuring in the tweets
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
tweets.sample <- tm_map(tweets.sample, toSpace, "[â€œ@ðŸ˜Šš€?ðYˆ™ÿ.~#^:/]")

# rechecking the tweets
tweets.sample[[1]][1]
tweets.sample[[2]][1]
tweets.sample[[3]][1]
tweets.sample[[4]][1]

# removing the stopwords, and after observing the terms, there were some terms with low relevance and high freq
# so removing those terms
tweets.sample <- tm_map(tweets.sample, removeWords, c(stopwords("english"), "chicago", "get", "http")) 
tweets.sample <- tm_map(tweets.sample, stripWhitespace) # removing the white spaces
tweets.sample <- tm_map(tweets.sample, removeNumbers) # removing the numbers
tweets.sample <- tm_map(tweets.sample, removePunctuation) # removing punctuation
tweets.sample <- tm_map(tweets.sample, stemDocument) # Stemming the documents

# rechecking the tweets
tweets.sample[[1]][1]
tweets.sample[[2]][1]
tweets.sample[[3]][1]
tweets.sample[[4]][1]

# creating the term document matrix
tweets.dtm3 <- DocumentTermMatrix(tweets.sample, control = list(weighting = weightTf))

# removing empty documents
removedoc <- row_sums(tweets.dtm3, na.rm = T) # using the slam library, due to it's speed and 
# reliability over the apply function
tweets.dtm3 <- tweets.dtm3[removedoc >0, ]

# inspecting the sparsity of the matrix
tweets.dtm3
# finding the most frequent terms in the matrix, whose frequency(>20)
findFreqTerms(tweets.dtm3, 20)
# we go back to the document cleaning phase, and check the most frequent words again.

# creating a lda model
tweet.lda.schl <- LDA(tweets.dtm3, 5, control = list(seed = 300))
# looking at the top 10 terms in each topic
terms(tweet.lda.schl, 10)

# checking the sparcity of the topics
tweet.lda.schl@gamma[1:5, ]

# To calculate the topic-term matrix, i.e. the probability of each word for each topic
# we will use the model and the existing term document matrix and 
topic.schl <- posterior(tweet.lda.schl, tweets.dtm3)

# adding the names to each topic, using the first 5 most freq terms in each topic.
row.names(topic.schl$terms) <- apply(terms(tweet.lda.schl, 5), 2, paste, collapse = ",")

# checking the topic-term matrix
topic.schl$terms[1:5, 60:65]

# group documents by most likely topic and look at one of the document groups
document.most.likely.topic = topics(tweet.lda.schl, 1)
document.topic.clusters = split(corp.tweets.sample, document.most.likely.topic)
document.topic.clusters[[3]][[1]]$content
document.topic.clusters[[3]][[2]]$content
document.topic.clusters[[3]][[3]]$content

# Inspecting the most likely documents
document.topic.clusters

# Plotting the clusters with schools in the city of chicago
plot(city.boundary)
points(school.points, pch = "*", col= 'yellow')
points(tweet.sample[,c("x", "y")], pch = "*", 
       col= c('green', 'red', 'green', 'red', 'red')[document.most.likely.topic])

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



