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
library(text2vec)
library(lsa)
library(chorddiag)

# LOAD RAW DATA ---------------------------------------------------------------
books_read <- read_csv('data/books_read.csv')
top_shelves <- read_csv('data/top_shelves.csv')

descriptions <- books_read %>%
  filter(year_read >= 2019) %>%
  pull(description) %>%
  as.vector() %>%
  VectorSource() %>% 
  Corpus()

doc_titles <-  books_read %>%
  filter(year_read >= 2019) %>%
  pull(title)

descriptions <- tm_map(descriptions, removeWords, stopwords("english"))
descriptions <- tm_map(descriptions, content_transformer(removePunctuation))
descriptions <- tm_map(descriptions, content_transformer(removeNumbers))
descriptions <- tm_map(descriptions,  content_transformer(tolower)) 
descriptions <- tm_map(descriptions, content_transformer(stripWhitespace))
descriptions <- tm_map(descriptions, content_transformer(stemDocument), language = "english")

DTM <- DocumentTermMatrix(descriptions)
inspect(DTM)

DTM$dimnames$Docs <- doc_titles

x <- as.matrix(DTM)
t(x[1:10, 1:10])
x <- cosine(t(x))
x[1:10,1:10]

y <- x


y <- as_tibble(y) %>%
  mutate(title = doc_titles) %>%
  gather(title2, cosine_score, -title) %>%
  arrange(title, desc(cosine_score)) %>%
  group_by(title) %>%
  mutate(cosine_score = case_when(cosine_score == 1~0, 
                                  row_number() >= 6~0,
                                  TRUE~cosine_score)) %>%
  spread(title2, cosine_score, fill = 0) %>%
  ungroup() 
rownames(y) <- y$title
y <- select(y, -title)

y <- as.matrix(y)


chorddiag(x)
chorddiag(y)
chorddiag(y[1:10,1:10])

chorddiag(x[1:10,1:10])

# Create dummy data
m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)

# A vector of 4 colors for 4 groups
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)
groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")

# Build the chord diagram:
p <- chorddiag(m, groupColors = groupColors, groupnamePadding = 20)
p
