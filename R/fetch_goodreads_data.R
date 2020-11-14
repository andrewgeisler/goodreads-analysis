library(httr)
library(tidyverse)
library(xml2)
library(lubridate)
library(syuzhet)


# ## FETCH BOOKS FUNCTION -------------------------------------------------

fetch_read_books <- function() {
  ## SET QUERY PARAMETERS FOR BOOKS READ
  base_url <-
    str_c('https://www.goodreads.com/review/list/',
          Sys.getenv('GRID'),
          '.xml')
  query_parms <- list(
    shelf = "read",
    v = 2,
    key = Sys.getenv("GRKEY"),
    per_page = 200,
    typename = "vaestoalue:kunta_vaki2017",
    outputFormat = "application/json"
  )
  
  ## FETCH XML DOCUMENT
  doc <- httr::GET(url = base_url, query = query_parms) %>%
    httr::content()
  
  ## PARSE XML DOCUMENT / EXTRACT REQUIRED FIELDS
  books_read <- xml_find_all(doc, "//review")
  
  ## SET PATHS TO EXTRACT
  paths <- c(
    "//book/id",
    "//work/id",
    "//isbn",
    "//isbn13",
    "//title",
    "//num_pages",
    "//publication_year",
    "//description",
    "//author/name",
    "//author/id",
    "//book/average_rating",
    "//published",
    "//read_at",
    "//started_at"
  )
  
  ## HELPER PARSING FUNCTION XML -> DF
  extract_text <- function(doc, path) {
    field_name <- path %>%
      str_replace_all('//', '') %>%
      str_replace_all('/', '_')
    
    
    values <- doc %>%
      xml_find_all(path) %>%
      xml_text() %>%
      tibble(field = .)
    
    colnames(values) <- field_name
    
    return(values)
    
  }
  
  ## ITERATE AND EXECUTE OVER BOOKS READ XML DOCUMENT
  paths %>%
    map( ~ extract_text(books_read, .)) %>%
    bind_cols() %>%
    mutate(
      read_at = as_date(parse_date_time(read_at, orders = '%a %b %d %T %z %Y')),
      started_at = as_date(parse_date_time(started_at, orders = '%a %b %d %T %z %Y')),
      days_read = as.numeric(read_at - started_at) + 1,
      year_read = year(read_at)
    ) %>%
    filter(year_read >= 2018)
  
}

### EXTRACT DATA FROM GOODREADS
books_read <- fetch_read_books()

## MISSING PAGES 

books_read <- books_read %>%
  mutate(
    num_pages = case_when(
      book_id == '78687' ~ '240',
      book_id == '43526443' ~ '288',
      book_id == '384009' ~ '224',
      book_id == '52222275' ~ '350',
      TRUE~num_pages
    )
  )


# FETCH BOOK GENRES -------------------------------------------------------

parse_shelves <- function(books) {
  isbn <- books$isbn13
  
  ### REQUEST SHELF DATA FOR EACH BOOK
  get_shelves_by_isbn <- function(isbn) {
    base_url <- str_c('https://www.goodreads.com/book/isbn/', isbn)
    query_parms <- list(key = Sys.getenv("GRKEY"))
    
    genre <- httr::GET(url = base_url, query = query_parms) %>%
      httr::content()
    
    ## PARSE SHELF DATA
    genre <-
      xml_attrs(xml_find_all(genre, "//popular_shelves/shelf"))
    
    ## EXTRACT SHELF AND USER COUNT DATA FOR EACH BOOK
    map(genre, function(x) {
      tibble(isbn13 = isbn,
             shelf = x['name'],
             users = as.numeric(x['count']))
    }) %>%
      bind_rows()
  }
  
  ## ITERATE OVER ALL ISBNS
  map(isbn, get_shelves_by_isbn) %>%
    bind_rows()
  
}

## EXECUTE TOP SHELVES LOOK UP
top_shelves <- parse_shelves(books_read)

## REMOVE SHELVES THAT DON'T REPRESENT GENRE
## NOT EXHAUSTIVE FILTER
top_shelves <- top_shelves %>%
  filter(
    !str_detect(shelf, 'read'),!str_detect(shelf, 'favorites'),!str_detect(shelf, 'book-club'),!str_detect(shelf, 'own'),!str_detect(shelf, 'books-i-own'),!str_detect(shelf, 'audiobook'),!str_detect(shelf, 'vonnegut'),!str_detect(shelf, 'stephen-king'),!str_detect(shelf, '2015'),!str_detect(shelf, '2016'),!str_detect(shelf, '2017'),!str_detect(shelf, '2019'),!str_detect(shelf, '2018'),!str_detect(shelf, '2020'),!str_detect(shelf, 'ebook'),!str_detect(shelf, 'library'),!str_detect(shelf, 'kindle'),!str_detect(shelf, 'audio'),!str_detect(shelf, 'audible'),!str_detect(shelf, 'wish-list'),!str_detect(shelf, 'next'),!str_detect(shelf, 'reviewed'),!str_detect(shelf, 'on-hold'),!str_detect(shelf, 'to-buy'),!str_detect(shelf, 'novels'),!str_detect(shelf, 'did-not-finish'),!str_detect(shelf, 'dnf'),!str_detect(shelf, 'maybe'),!str_detect(shelf, 'favourites'),!str_detect(shelf, 'e-book'),!str_detect(shelf, 'borrowed'),!str_detect(shelf, 'i-own')
  )

## ADD RANK BY BOOK
top_shelves <- top_shelves %>%
  group_by(isbn13) %>%
  arrange(isbn13, desc(users)) %>%
  mutate(rank = rank(desc(users), ties.method = 'min')) %>%
  left_join(books_read) %>%
  select(isbn13, isbn, title, shelf, users, rank, year_read) %>%
  ungroup()


# FETCH BOOK QUOTES -------------------------------------------------------

get_quotes <- function(work_id) {
  ### FETCH QUOTE DATA FROM HTML
  url <- str_c('https://www.goodreads.com/work/quotes/', work_id, '/')
  quotes <- GET(url = url) %>% httr::content()
  
  ## CLEAN UP HTML/XML DOCMENT BY REMOVE UNNEEDED TAGS/NODES
  
  quotes <- xml_find_all(quotes, "//div[@class='quoteText']")
  xr <- xml2::xml_find_all(quotes, "//span[@class='authorOrTitle']")
  xml_remove(xr)
  xr <- xml_find_all(quotes, "//a[@class='authorOrTitle']")
  xml_remove(xr)
  xr <- xml_find_all(quotes, "//script")
  xml_remove(xr)
  xr <- xml_find_all(quotes, "//span")
  xml_remove(xr)
  xr <- xml_find_all(quotes, "//br")
  xml_remove(xr)
  quotes <- xml_text(quotes)
  
  ## CLEAN UP TEXT
  quotes %>%
    str_replace_all('\n', '') %>%
    str_replace_all('\\"', '"') %>%
    str_replace_all('―', '') %>%
    str_replace_all('“|”|’s', '') %>%
    str_trim() %>%
    as_tibble() %>%
    mutate(work_id = work_id)
  
}

book_quotes <- map(books_read$work_id, get_quotes) %>%
  bind_rows() %>%
  inner_join(books_read) %>%
  select(book_id, work_id, isbn, title, year_read, quote_text = value)


book_sentiments <- book_quotes %>%
  mutate(sentiment = map(quote_text, get_nrc_sentiment)) %>%
  unnest(sentiment) %>%
  mutate(work_id = factor(work_id)) %>%
  select(-negative,-positive) %>%
  pivot_longer(anger:trust, names_to = 'sentiment', values_to = 'score') %>%
  group_by(year_read, work_id, sentiment) %>%
  summarize(score = sum(score))



## SAVE DATA --------
write_lines(today(), path = 'data/date_updated.txt')
write_csv(books_read, path = 'data/books_read.csv')
write_csv(top_shelves, 'data/top_shelves.csv')
write_csv(book_quotes, 'data/book_quotes.csv')
write_csv(book_sentiments, 'data/book_sentiments.csv')
