library(httr)
library(tidyverse)
library(lubridate)
library(zoo)
library(wesanderson)
library(plotly)

source('R/set_color_pal.R')

# UDPATE RAW DATA ---------------------------------------------------------
# source('R/fetch_goodreads_data.R')


# LOAD RAW DATA ---------------------------------------------------------------
books_read <- read_csv('data/books_read.csv')
top_shelves <- read_csv('data/top_shelves.csv')
book_sentiments <- read_csv('data/book_sentiments.csv')
book_quotes <- read_csv('data/book_quotes.csv')


# SUMMARIZE BOOKS READ ----------------------------------------------------

yearly_summary <- books_read %>%
  mutate(pages_per_day = as.numeric(num_pages) / days_read) %>%
  group_by(year_read) %>%
  summarize(
    books = n(),
    pages = sum(as.numeric(num_pages)),
    pages_per_book = pages / books,
    avg_pages_per_day = mean(pages_per_day),
    median_days_to_read = median(days_read),
    avg_goodreads_rating = mean(as.numeric(book_average_rating))
  )

yearly_summary_long <- yearly_summary %>%
  pivot_longer(books:avg_goodreads_rating, 'measure', 'value') %>%
  pivot_wider(measure, names_from = year_read, values_from = value) %>%
  mutate(
    percent_change = `2019` / `2018`,
    percent_change = percent_change - 1,
    raw_diff = `2019` - `2018`
  )

monthly_summary <- books_read %>%
  mutate(
    pages_per_day = as.numeric(num_pages) / days_read,
    month = month(read_at, label = T)
  ) %>%
  group_by(year_read, month) %>%
  summarize(
    books = n(),
    pages = sum(as.numeric(num_pages)),
    pages_per_book = pages / books,
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

# TOP GENRES --------------------------------------------------------------

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
  group_by(year_read, isbn13) %>%
  mutate(rank = rank(desc(rank))) %>%
  group_by(year_read, shelf) %>%
  summarize(books = n_distinct(isbn13), weighted = sum(rank)) %>%
  group_by(year_read) %>%
  arrange(year_read, desc(books)) %>%
  # top_n(30, books) %>%
  left_join(yearly_summary %>% select(year_read, total_books = books), by = 'year_read') %>%
  mutate(percent_of_books = books / total_books)


# CREATE DAILY READING DATA FRAMES ----------------------------------------


daily_reading_by_title <- books_read %>%
  select(isbn, title, read_at, started_at, num_pages, days_read) %>%
  group_by(isbn, title) %>%
  mutate(date = list(seq(
    from = started_at, to = read_at, by = 'day'
  ))) %>%
  unnest() %>%
  mutate(est_pages = num_pages / days_read) %>%
  ungroup() %>%
  select(title, date, est_pages) %>%
  filter(date >= '2018-01-01')

calculate_growth <- function() {
  daily_reading <- books_read %>%
    select(isbn, title, read_at, started_at, num_pages, days_read) %>%
    group_by(isbn, title) %>%
    mutate(date = list(seq(
      from = started_at, to = read_at, by = 'day'
    ))) %>%
    unnest() %>%
    mutate(est_pages = num_pages / days_read) %>%
    ungroup() %>%
    select(title, date, est_pages) %>%
    mutate(year = year(date), doy = yday(date)) %>%
    filter(date >= '2018-01-01') %>%
    group_by(year, doy, date) %>%
    summarize(total_pages = sum(est_pages)) %>%
    group_by(year) %>%
    left_join(yearly_summary, by = c('year' = 'year_read')) %>%
    mutate(total_pages = (total_pages / sum(total_pages)) * pages) %>%
    arrange(year, doy) %>%
    mutate(running_total_pages = round(cumsum(total_pages)),
           last_day = doy == max(doy)) %>%
    select(year, doy, date, total_pages, running_total_pages, last_day)
  
  df2018 <-
    map(daily_reading$date[daily_reading$year == 2018], function(x) {
      daily_reading_by_title %>%
        filter(date >= floor_date(x, 'year'),
               date <= x) %>%
        mutate(date = x) %>%
        group_by(date) %>%
        summarize(unique_books = n_distinct(title)) %>%
        ungroup()
    }) %>%
    bind_rows()
  
  df2018 <- df2018 %>%
    mutate(
      unique_books = (unique_books / max(unique_books)) * yearly_summary$books[yearly_summary$year_read == 2018],
      unique_books = round(unique_books)
    )
  
  
  df2019 <-
    map(daily_reading$date[daily_reading$year == 2019], function(x) {
      daily_reading_by_title %>%
        filter(date >= floor_date(x, 'year'),
               date <= x) %>%
        mutate(date = x) %>%
        group_by(date) %>%
        summarize(unique_books = n_distinct(title)) %>%
        ungroup()
    }) %>%
    bind_rows()
  
  df2019 <- df2019 %>%
    mutate(
      unique_books = (unique_books / max(unique_books)) * yearly_summary$books[yearly_summary$year_read == 2019],
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
  mutate(avg_reading_pace_7day = rollmean(total_pages, 14, fill = NA, align =
                                            'right'))




# CREATE PLOTS ------------------------------------------------------------

plotly_font <- list(family = "CentraleSansRnd",
                    size = 11,
                    color = '#24281A')

p_pages_by_day <- plot_ly(
  daily_reading,
  x = ~ doy,
  y = ~ running_total_pages,
  color = ~ factor(year),
  colors = rev(color_pals[1:2]),
  type = 'scatter',
  mode = 'lines'
) %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    xaxis = list(title = 'Day Of Year'),
    yaxis = list(title = ''),
    annotations = list(
      x = subset(daily_reading, last_day == T)$doy,
      y = subset(daily_reading, last_day == T)$running_total_pages,
      text = scales::comma(subset(daily_reading, last_day == T)$running_total_pages),
      showarrow = F
    )
  )

p_books_by_day <- plot_ly(
  daily_reading,
  x = ~ doy,
  y = ~ unique_books,
  color = ~ factor(year),
  colors = rev(color_pals[1:2]),
  type = 'scatter',
  mode = 'lines'
) %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    xaxis = list(title = 'Day Of Year'),
    yaxis = list(title = ''),
    annotations = list(
      x = subset(daily_reading, last_day == T)$doy,
      y = subset(daily_reading, last_day == T)$unique_books,
      text = scales::comma(subset(daily_reading, last_day == T)$unique_books),
      showarrow = F
    )
  )

p_books_by_month <- plot_ly(
  subset(monthly_summary, year_read == 2019),
  x = ~ month,
  y = ~ books,
  color = I(color_pals[1]),
  type = 'bar',
  text = ~ books
) %>%
  add_text(textfont = list(color = '#404040'),
           textposition = "top") %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    xaxis = list(title = ''),
    yaxis = list(title = '', showticklabels = F)
  )


p_authors_pages <- top_authors_pages_read %>%
  filter(year_read == 2019) %>%
  mutate(author_name = factor(author_name),
         author_name = reorder(author_name, pages)) %>%
  plot_ly(
    x = ~ pages,
    y = ~ author_name,
    color = ~ author_name,
    colors = rev(color_pals[1:10]),
    text = ~ pages,
    type = 'bar'
  ) %>%
  add_text(textfont = list(color = '#404040'),
           textposition = "left") %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    xaxis = list(
      title = '',
      side = "top",
      showticklabels = F
    ),
    yaxis = list(title = '')
  )


p_authors_books <- top_authors_books_read %>%
  filter(year_read == 2019, books > 1) %>%
  mutate(author_name = factor(author_name),
         author_name = reorder(author_name, books)) %>%
  plot_ly(
    x = ~ books,
    y = ~ author_name,
    color = ~ author_name,
    colors = rev(color_pals[1:8]),
    text = ~ books,
    type = 'bar'
  ) %>%
  add_text(textfont = list(color = '#404040'),
           textposition = "left") %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    xaxis = list(
      title = '',
      side = "top",
      showticklabels = F
    ),
    yaxis = list(title = '')
  )

p_reading_pace <-
  ggplot(data = subset(daily_reading, year == 2019),
         aes(x = date, y = avg_reading_pace_7day)) +
  geom_line(
    data = subset(daily_reading_by_title, date >= '2019-01-01'),
    aes(y = est_pages, color = title),
    alpha = .5
  ) +
  geom_point(
    data = subset(daily_reading_by_title, date >= '2019-01-01'),
    aes(y = est_pages, color = title),
    alpha = .8,
    size = 1
  ) +
  geom_line(alpha = .8,  color = wes_palette("Moonrise2")[1]) +
  geom_smooth(se = F, color = wes_palette("Moonrise2")[2]) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = '2 month', date_labels = '%b-%y') +
  labs(y = 'Esimated Pages Per Day') +
  plot_fill +
  plot_color +
  scale_y_continuous(labels = scales::comma)


# top genre plot ----------------------------------------------------------

p_top_genres <- top_genres %>%
  filter(year_read == 2019, percent_of_books > .1) %>%
  # top_n(40, percent_of_books) %>%
  mutate(shelf = factor(shelf),
         shelf = reorder(shelf, percent_of_books)) %>%
  plot_ly(
    x = ~ shelf,
    y = ~ percent_of_books,
    color = ~ shelf,
    colors = rev(color_pals[1:8]),
    type = 'bar',
    text = ~ scales::percent(percent_of_books)
  ) %>%
  add_annotations(
    x = ~ shelf,
    y = ~ percent_of_books,
    text = ~ shelf,
    showarrow = F,
    textangle = 270,
    xanchor = 'center',
    yanchor = 'bottom'
  ) %>%
  # add_text(textfont = list(color='#404040'), textposition = "auto") %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    xaxis = list(
      title = '',
      autorange = "reversed",
      showticklabels = F
    ),
    yaxis = list(
      title = 'Percent of Books',
      tickformat = "%",
      showticklabels = T
    )
  )

# sentiment plot ----------------------------------------------------------

p_sentiments <- book_sentiments %>%
  filter(year_read == 2019) %>%
  plot_ly(
    y = ~ score,
    color = ~ sentiment,
    colors = rev(color_pals[1:8]),
    type = "box"
  ) %>%
  layout(
    showlegend = FALSE,
    font = plotly_font,
    title = "",
    xaxis = list(title = ''),
    yaxis = list(title = ''),
    annotations = list(
      x = 4,
      y = 77,
      text = "Based on quotes available on Goodreads",
      showarrow = F
    )
  )
