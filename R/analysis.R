library(httr)
library(tidyverse)
library(lubridate)
library(zoo)
library(wesanderson)
library(plotly)

source('R/set_color_pal.R')

# UDPATE RAW DATA ---------------------------------------------------------
# source('R/fetch_goodreads_data.R')

date_updated <- read_lines('data/date_updated.txt') %>% ymd()

# LOAD RAW DATA ---------------------------------------------------------------
books_read <- read_csv('data/books_read.csv')
top_shelves <- read_csv('data/top_shelves.csv')
book_sentiments <- read_csv('data/book_sentiments.csv')
book_quotes <- read_csv('data/book_quotes.csv')

# INPUT MISSING PAGE NUMBERS ----------------------------------------------

books_read <- books_read %>%
  mutate(
    year_read = year(started_at),
    num_pages = case_when(
      title == 'Tender: Stories' ~ 288,
      title == 'Tabloid Dreams' ~ 244,
      title == 'Exhalation' ~ 350,
      TRUE ~ num_pages
    )
  )

## FILTER OUT BOOKS STARTED PRIOR TO 2018

books_read <- books_read %>%
  filter(started_at >= '2018-01-01')


# CREATE DAILY PAGES READ  ------------------------------------------------
### Daily pages read estimated for each book by divided total pages by total days

df_days_read <- tibble(date = seq(from = ymd('2018-01-01'), to = today(), by = 'day')) %>%
  crossing(books_read) %>%
  filter(
    date >= started_at,
    date <= read_at
  ) %>%
  arrange(title) %>%
  select(
    date,
    book_id, 
    title, 
    num_pages,
    author_name,
    days_read
  ) %>%
  mutate(pages = num_pages/days_read)

df_daily_cume_pages <- df_days_read %>%
  group_by(date) %>%
  summarize(
    pages = sum(pages)
  ) %>%
  ungroup() %>%
  right_join(tibble(date = seq(from = ymd('2018-01-01'), to = today(), by = 'day')), by = 'date') %>%
    mutate(pages = str_replace_na(pages,0)) %>%
  mutate(year = factor(year(date)), yday = yday(date)) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(total_pages = cumsum(pages)) %>%
  ungroup() %>%
  mutate(ppd_rolling = zoo::rollmean(x = as.numeric(pages), 30, align = "right", fill = NA))

df_daily_books_read <- df_days_read %>%
  group_by(book_id) %>%
  summarize(finished_at = max(date)) %>%
  group_by(finished_at) %>%
  summarize(books = n_distinct(book_id)) %>%
  right_join(tibble(finished_at = seq(from = ymd('2018-01-01'), to = today(), by = 'day')), by = 'finished_at') %>%
  mutate(books = str_replace_na(books,0)) %>%
  mutate(year = factor(year(finished_at)), yday = yday(finished_at)) %>%
  group_by(year) %>%
  arrange(finished_at) %>%
  mutate(books = cumsum(books)) %>%
  ungroup()


# Plot Pages and Books Read -----------------------------------------------

# Author Summarize --------------------------------------------------------
df_author_summary <- books_read %>%
  group_by(author_name) %>%
  summarize(total_books = n(), total_pages = sum(num_pages)) %>%
  ungroup()

top_genres <- top_shelves %>%
  filter(rank <= 25) %>%
  filter(
    !shelf %in% c(
      'abandoned',
      'didn-t-finish',
      'tbr',
      'default',
      'giveaways',
      'my-books',
      'series',
      'bookclub'
    )
  ) %>%
  group_by(isbn13) %>%
  mutate(rank = rank(desc(rank))) %>%
  group_by(shelf) %>%
  summarize(books = n_distinct(isbn13)) %>%
  arrange(desc(books)) %>%
  mutate(percent_of_books = books/length(unique(books_read$book_id))) %>%
  ungroup()


plotly_font <- list(
  #family = "CentraleSansRnd",
  size = 11,
  color = '#24281A'
)

