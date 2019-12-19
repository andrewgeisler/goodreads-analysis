
daily_reading_by_title <- books_read %>%
  select(isbn, title, read_at, started_at, num_pages, days_read) %>%
  group_by(isbn, title) %>%
  mutate(date = list(seq(from=started_at, to=read_at, by='day'))) %>% 
  unnest() %>%
  mutate(est_pages = num_pages/days_read) %>%
  ungroup() %>%
  select(title, date, est_pages) %>%
  filter(date >= '2018-01-01')

calculate_growth <- function() {
  
  daily_reading <- books_read %>%
    select(isbn, title, read_at, started_at, num_pages, days_read) %>%
    group_by(isbn, title) %>%
    mutate(date = list(seq(from=started_at, to=read_at, by='day'))) %>% 
    unnest() %>%
    mutate(est_pages = num_pages/days_read) %>%
    ungroup() %>%
    select(title, date, est_pages) %>%
    mutate(year = year(date), doy = yday(date)) %>%
    filter(date >= '2018-01-01') %>%
    group_by(year, doy, date) %>%
    summarize(total_pages = sum(est_pages)) %>%
    group_by(year) %>%
    left_join(yearly_summary, by = c('year'='year_read')) %>%
    mutate(
      total_pages = (total_pages/sum(total_pages))*pages
    ) %>%
    arrange(year, doy) %>%
    mutate(
      running_total_pages = round(cumsum(total_pages)),
      last_day = doy == max(doy)
    ) %>% 
    select(year, doy, date, total_pages, running_total_pages,last_day)
  
  df2018 <- map(daily_reading$date[daily_reading$year == 2018], function(x) {
    
    daily_reading_by_title %>%
      filter(
        date >= floor_date(x, 'year'),
        date <= x
      ) %>%
      mutate(date = x) %>%
      group_by(date) %>%
      summarize(
        unique_books = n_distinct(title)
      ) %>%
      ungroup()
  }) %>%
    bind_rows()
  
  df2018 <- df2018 %>%
    mutate(
      unique_books = (unique_books/max(unique_books))*yearly_summary$books[yearly_summary$year_read == 2018],
      unique_books = round(unique_books)
    )
  
  
  df2019 <- map(daily_reading$date[daily_reading$year == 2019], function(x) {
    
    daily_reading_by_title %>%
      filter(
        date >= floor_date(x, 'year'),
        date <= x
      ) %>%
      mutate(date = x) %>%
      group_by(date) %>%
      summarize(
        unique_books = n_distinct(title)
      ) %>%
      ungroup()
  }) %>%
    bind_rows()
  
  df2019 <- df2019 %>%
    mutate(
      unique_books = (unique_books/max(unique_books))*yearly_summary$books[yearly_summary$year_read == 2019],
      unique_books = round(unique_books)
    )
  
  book_growth <- bind_rows(df2018, df2019)
  
  daily_reading %>%
    left_join(book_growth)
}

daily_reading <- calculate_growth()

daily_reading <- daily_reading %>% 
  ungroup() %>%
  arrange(date) %>%
  mutate(
    avg_reading_pace_7day = rollmean(total_pages, 14, fill = NA, align='right')
  )
