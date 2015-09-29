## information for this file was pulled almost directly from the URL below.
##https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(needed, dependencies=TRUE)   

library(tm)
library(SnowballC)
library(ggplot2)   
library(wordcloud)

#Setup the file path to Pope Francis' speech
cname <- file.path("~", "Dropbox (Personal)", "Programming", "PopeFrancisSpeech", "texts")
cname  #check the path
dir(cname)  #list the files in the directory
docs <- Corpus(DirSource(cname))  #load texts into R


##PREPROCESSING THE DOCUMENTS##
docs <- tm_map(docs, removePunctuation)  #Remove punctuation
docs <- tm_map(docs, removeNumbers)  #Remove numbers
docs <- tm_map(docs, tolower)  #Convert all to lower
docs <- tm_map(docs, removeWords, stopwords("english"))  #Remove stopwords (a, and, also, the, etc.)
# docs <- tm_map(docs, removeWords, c("let", "like"))  #Remove particular words
docs <- tm_map(docs, stripWhitespace)  #Remove whitespace

#Remove common word endings (e.g., "ing", "es", "s", etc.) from library 'SnowballC'
docs <- tm_map(docs, stemDocument)

#Tell R that docs are now text documents
docs <- tm_map(docs, PlainTextDocument)


##STAGE THE DATA##
dtm <- DocumentTermMatrix(docs)  #Create a document term matrix



##EXPLORE THE DATA - DIFFERENT WAYS TO VIEW THE DATA##

freq <- colSums(as.matrix(dtm))
#freq

#Order the fequencies
ord <- order(freq)  
#ord

#Export to Excel for analysis
m <- as.matrix(dtm)   
dim(m)   
#write.csv(m, file="dtm.csv")


freq <- colSums(as.matrix(dtm))   
#freq 

wf <- data.frame(word=names(freq), freq=freq)   #Convert matrix to a data frame for easier viewing
#tail(wf, n = 20)
#head(wf, n = 20)
#write.csv(wf, file="dtm.csv")

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq,20)  #show the most frequent words used--first 20


#Show frequent terms for all of the speeches (only showing words used 6+ times)
findFreqTerms(dtm, lowfreq=6)


#lot words that have been used more than 10 times in all of the speeches
p <- ggplot(subset(wf, freq > 10), aes(word, freq))
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p <- p + labs(title = "Words used more than 10 times")
p   

#Generate the wordcloud
set.seed(1142)  #Keeps the layout the same each time the wordcloud is generated.  Can comment if layout does not matter
wordcloud(names(freq), freq, min.freq=5, scale=c(5, .1), colors=brewer.pal(6, "Spectral"))   
#display.brewer.all()  #This show all of the brewer color pallets available for use



