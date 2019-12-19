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

yearly_summary_long <- yearly_summary %>%
  gather(measure, value, books:avg_goodreads_rating) %>%
  spread(year_read, value) %>%
  mutate(
    percent_change = `2019`/`2018`,
    percent_change = percent_change -1,
    raw_diff = `2019`-`2018`
  )

top_authors_books_read <- books_read %>%
  group_by(year_read, author_name) %>%
  summarize(books = n()) %>%
  top_n(10, books)

top_authors_pages_read <- books_read %>%
  group_by(year_read, author_name) %>%
  summarize(pages = sum(num_pages)) %>%
  top_n(10, pages)

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