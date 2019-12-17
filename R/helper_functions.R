library(httr)
library(tidyverse)
library(xml2)
library(lubridate)


# HELPER FUNCTIONS --------------------------------------------------------


fetch_data <- function(base_url, query_parms) {
  
  httr::GET(
    url = base_url,
    query = query_parms
  ) %>%
    content()
  
}