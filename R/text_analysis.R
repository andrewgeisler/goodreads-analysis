library(httr)
library(tidyverse)
library(xml2)
library(lubridate)
library(zoo)
library(wesanderson)
library(plotly)


library(tm)
library(SentimentAnalysis)
library(tidytext)
library(syuzhet)

# LOAD RAW DATA ---------------------------------------------------------------
books_read <- read_csv('data/books_read.csv')
top_shelves <- read_csv('data/top_shelves.csv')

text <- books_read$description

text <- Corpus(VectorSource(as.vector(text))) 

text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, content_transformer(removePunctuation))
text <- tm_map(text, content_transformer(removeNumbers))
text <- tm_map(text,  content_transformer(tolower)) 
text <- tm_map(text, content_transformer(stripWhitespace))
text <- tm_map(text, content_transformer(stemDocument), language = "english")


DTM <- DocumentTermMatrix(text)
inspect(DTM)

sent <- analyzeSentiment(DTM, language = "english")

sent[,1:4]
sent <- as.data.frame(sent)
summary(sent$SentimentGI)



sent2 <- as_tibble(get_nrc_sentiment(books_read$description))
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")

sent3 %>% arrange(desc(count))
