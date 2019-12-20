library(httr)
library(tidyverse)
library(xml2)
library(lubridate)
library(zoo)
library(wesanderson)
library(plotly)


# UDPATE RAW DATA ---------------------------------------------------------
# source('R/get_books_read.R')
# source('R/get_book_genres.R')


# LOAD RAW DATA ---------------------------------------------------------------
books_read <- read_csv('data/books_read.csv')
top_shelves <- read_csv('data/top_shelves.csv')

source('R/summarize_data.R')
source('R/summarize_daily_data.R')


# SET THEME FOR PLOTS -----------------------------------------------------
theme_set(
  theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.title = element_blank(),
      text = element_text(color = '#404040'),
      axis.text = element_text(color = '#404040', size = 8)
    )
)

color_pals <- c(
  "Moonrise2",
  "Moonrise1",
  "Moonrise3",
  "Royal1",
  "Royal2",
  "IsleofDogs1",
  "IsleofDogs2",
  "GrandBudapest1",
  "GrandBudapest2",
  "FantasticFox1",
  "Rushmore1",
  "Rushmore",
  "BottleRocket1",
  "BottleRocket2",
  "Zissou1",
  "Darjeeling1",   
  "Darjeeling2",
  "Chevalier1",
  "Cavalcanti1"
)

color_pals <- map(color_pals, wes_palette) %>% unlist()

plot_color <- scale_color_manual(values = color_pals)
plot_fill <- scale_fill_manual(values = color_pals)



# CREATE PLOTS ------------------------------------------------------------

p_pages_by_day <- plot_ly(
  daily_reading,
  x = ~doy, 
  y = ~running_total_pages,
  color = ~factor(year),
  colors = rev(color_pals[1:2]),
  type = 'scatter', mode = 'lines'
) %>%
  layout(showlegend = FALSE, xaxis=list(title='Day Of Year'), yaxis=list(title=''),
         annotations = list(
           x = subset(daily_reading, last_day == T)$doy,
           y =subset(daily_reading, last_day == T)$running_total_pages,
           text = scales::comma(subset(daily_reading, last_day == T)$running_total_pages),
           showarrow=F
         )) 

p_books_by_day <- plot_ly(
  daily_reading,
  x = ~doy, 
  y = ~unique_books,
  color = ~factor(year),
  colors = rev(color_pals[1:2]),
  type = 'scatter', mode = 'lines'
) %>%
  layout(showlegend = FALSE, xaxis=list(title='Day Of Year'), yaxis=list(title=''),
         annotations = list(
           x = subset(daily_reading, last_day == T)$doy,
           y =subset(daily_reading, last_day == T)$unique_books,
           text = scales::comma(subset(daily_reading, last_day == T)$unique_books),
           showarrow=F
         )) 

p_authors_pages <- top_authors_pages_read %>% 
  filter(year_read == 2019) %>% 
  mutate(
    author_name = factor(author_name),
    author_name = reorder(author_name, pages)
  ) %>%
plot_ly(
  x = ~pages, 
  y = ~author_name,
  color = ~author_name,
  colors = rev(color_pals[1:10]),
  text = ~pages,
  type = 'bar'
) %>%
  add_text(textfont = list(color='#404040'), textposition = "left") %>%
  layout(showlegend = FALSE, xaxis=list(title='', side ="top"), yaxis=list(title='')) 


p_authors_books <- top_authors_books_read %>% 
  filter(year_read == 2019, books > 1) %>% 
  mutate(
    author_name = factor(author_name),
    author_name = reorder(author_name, books)
  ) %>%
  plot_ly(
    x = ~books, 
    y = ~author_name,
    color = ~author_name,
    colors = rev(color_pals[1:8]),
    text = ~books,
    type = 'bar'
  ) %>%
  add_text(textfont = list(color='#404040'), textposition = "left") %>%
  layout(showlegend = FALSE, xaxis=list(title='', side ="top"), yaxis=list(title='')) 

p_reading_pace <- ggplot(data = daily_reading,aes(x=date,y=avg_reading_pace_7day)) +
  geom_line(data = daily_reading_by_title, aes(y=est_pages, color=title), alpha = .5) +
  geom_point(data = daily_reading_by_title, aes(y=est_pages, color=title), alpha = .8, size = 1) +
  geom_line(alpha = .8,  color = wes_palette("Moonrise2")[1]) +
  geom_smooth(se=F, color = wes_palette("Moonrise2")[2]) +
  theme(legend.position = 'none',axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = '2 month', date_labels = '%b-%y') +
  labs(y = 'Esimated Pages Per Day') +
  plot_fill +
  plot_color +
  scale_y_continuous(labels = scales::comma)



