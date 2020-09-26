library(shinydashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(DT)

source('R/analysis.R')

## CREATE A PERIODS TIBBLE
periods <- tibble(
  period = c(
    'Prior 14 Days',
    'Prior 30 Days',
    'Prior 60 Days',
    'Prior 90 Days',
    'Current Month', 
    'Current Year',
    'Lifetime'
  ),
  days_in_period = c(
    14, 30, 60, 90, day(date_updated)-1, yday(date_updated)-1, date_updated-ymd('2017-01-01')
  )
) %>%
  mutate(
    current_date = today(),
    start_current = case_when(
      period == 'Prior 14 Days' ~ current_date - days(days_in_period),
      period == 'Prior 30 Days' ~ current_date - days(days_in_period),
      period == 'Prior 60 Days' ~ current_date - days(days_in_period),
      period == 'Prior 90 Days' ~ current_date - days(days_in_period),
      period == 'Current Month' ~ floor_date(today(), 'month'),
      period == 'Current Year' ~ floor_date(today(), 'year'),
      period == 'Lifetime' ~ ymd('2017-01-01')
    ),
    end_current = start_current + (days_in_period+1),
    start_prior = case_when(
      period == 'Prior 14 Days' ~ current_date - days(days_in_period*2),
      period == 'Prior 30 Days' ~ current_date - days(days_in_period*2),
      period == 'Prior 60 Days' ~ current_date - days(days_in_period*2),
      period == 'Prior 90 Days' ~ current_date - days(days_in_period*2),
      period == 'Current Month' ~ floor_date(today()-months(1), 'month'),
      period == 'Current Year' ~ floor_date(today()-years(1), 'year')
    ),
    end_prior = start_prior + (days_in_period+1)
  )


## CREATE INPUT
input_select_period <- selectInput(
  "period",
  label = '',
  choices = list(
    "Prior 14 Days",
    "Prior 30 Days",
    "Prior 60 Days",
    "Prior 90 Days",
    "Current Month",
    "Current Year"
  ),
  selected = "Current Year",
  width = "150px",
  size = NULL
)

plotly_font <- list(
  #family = "CentraleSansRnd",
  size = 11,
  color = '#24281A'
)

