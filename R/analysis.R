library(httr)
library(tidyverse)
library(xml2)
library(lubridate)
library(zoo)
# library(ggwordcloud)


# LOAD DATA ---------------------------------------------------------------
books_read <- read_csv('data/books_read.csv')
top_shelves <- read_csv('data/top_shelves.csv')


# SUMMARY BOOKS READ ------------------------------------------------------

yearly_summary <- books_read %>%
  mutate(
    pages_per_day = as.numeric(num_pages)/days_read
  ) %>%
  group_by(
    year_read
  ) %>%
  summarize(
    books = n(),
    pages = sum(as.numeric(num_pages)),
    pages_per_book = pages/books,
    avg_pages_per_day = mean(pages_per_day),
    median_days_to_read = median(days_read),
    avg_goodreads_rating = mean(as.numeric(book_average_rating))
  )

top_authors_books_read <- books_read %>%
  group_by(year_read, author_name) %>%
  summarize(books = n()) %>%
  top_n(10, books)

top_authors_pages_read <- books_read %>%
  group_by(year_read, author_name) %>%
  summarize(pages = sum(num_pages)) %>%
  top_n(10, pages)


reading_pace <- books_read %>%
  select(isbn, title, read_at, started_at, num_pages, days_read) %>%
  group_by(isbn, title) %>%
  mutate(date = list(seq(from=started_at, to=read_at, by='day'))) %>% 
  unnest() %>%
  mutate(est_pages = num_pages/days_read) %>%
  filter(date >= '2018-01-01', est_pages > 10) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(
    rolling_avg = rollmean(est_pages, 14, fill = NA, align='right'),
    cumemean = cummean(est_pages)
  ) %>%
  select(title, date, est_pages, rolling_avg, cumemean) 

  



# TOP GENRES --------------------------------------------------------------
top_genres <- top_shelves %>%
  filter(rank <= 5) %>%
  group_by(year_read, isbn) %>%
  mutate(rank = rank(desc(rank))) %>%
  group_by(year_read, shelf) %>%
  summarize(
    books = n_distinct(isbn)
  ) %>%
  group_by(year_read) %>%
  arrange(year_read, desc(books)) %>% 
  top_n(20, books) %>% 
  filter(books > 1)
  
  
# #### OVERALL QUESTIONS
# 1 Year over year summary
#  - number of books read - x
#  - number of pages read - x
#  - avg length of book - x
#  - avg pages read per day - x 
#  - avg days to read a book - x
#  - avg good reads rating - x
#  
# 2. Top Author (num of books and pages read by year) - x
# 3. Desciption sentiment analysis?
# 4. Top Genres - X 
# 5. Change in reading pace




