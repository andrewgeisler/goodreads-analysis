library(httr)
library(tidyverse)
library(xml2)
library(lubridate)

## LOAD BOOKS READ
books_read <- read_csv('data/books_read.csv')

## LOAD HELPER FUNCTIONS 
source('R/helper_functions.R')


isbn <- books_read$isbn
isbn <- isbn[!is.na(isbn)]

parse_shelves <- function(isbn) {
  
  get_isbn <- function(isbn) {
    base_url <- str_c('https://www.goodreads.com/book/isbn/',isbn)
    query_parms <- list(key = Sys.getenv("GRKEY"))
    
    genre <- fetch_data(base_url, query_parms)
    genre <- xml_attrs(xml_find_all(genre, "//popular_shelves/shelf"))
    
    map(genre, function(x) {
      tibble(
        isbn = isbn,
        shelf = x['name'],
        users = as.numeric(x['count'])
      )
    }) %>%
      bind_rows()
  }

  map(isbn, get_isbn) %>%
    bind_rows
  
}

## EXECUTE TOP SHELVES LOOK UP
top_shelves <- parse_shelves(isbn)

## REMOVE CUSTOM SHELVES WITH READ IN TITLE
top_shelves <- top_shelves %>%
  filter(
    !str_detect(shelf, 'read'),
    !str_detect(shelf, 'favorites'),
    !str_detect(shelf, 'book-club'),
    !str_detect(shelf, 'owned'),
    !str_detect(shelf, 'books-i-own'),	
    !str_detect(shelf, 'audiobook'),
    !str_detect(shelf, 'vonnegut'),
    !str_detect(shelf, 'stephen-king'),	
    !str_detect(shelf, '2019'),
    !str_detect(shelf, '2018')
  )

## ADD RANK BY BOOK 
top_shelves <- top_shelves %>%
  group_by(isbn) %>%
  arrange(isbn, desc(users)) %>%
  mutate(rank = rank(desc(users), ties.method = 'min')) 

## JOIN READ YEAR 
top_shelves <- top_shelves %>%
  left_join(books_read) %>%
  select(isbn, title, shelf, users, rank, year_read)

write_csv(top_shelves, 'data/top_shelves.csv')

