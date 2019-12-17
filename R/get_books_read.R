
## LOAD HELPER FUNCTIONS
source('R/helper_functions.R')

## FETCH BOOKS FUNCTION
fetch_read_books <- function() {
  
  ## SET QUERY PARAMETERS FOR BOOKS READ
  base_url <- str_c('https://www.goodreads.com/review/list/',Sys.getenv('GRID'),'.xml')
  query_parms <- list(
    shelf = "read",
    v = 2,
    key = Sys.getenv("GRKEY"),
    per_page = 200,
    typename = "vaestoalue:kunta_vaki2017",
    outputFormat = "application/json"
  )
  
  ## FETCH XML DOCUMENT
  doc <- fetch_data(base_url,query_parms)
  
  ## PARSE XML DOCUMENT / EXTRACT REQUIRED FIELDS
  books_read <- xml_find_all(doc, "//review")
  
  ## SET PATHS TO EXTRACT
  paths <- c(
    "//isbn", 
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
    map(~extract_text(books_read, .)) %>%
    bind_cols() %>%
    mutate(
      read_at = as_date(parse_date_time(read_at, orders = '%a %b %d %T %z %Y')),
      started_at = as_date(parse_date_time(started_at, orders = '%a %b %d %T %z %Y')),
      days_read = as.numeric(read_at-started_at)+1,
      year_read = year(read_at)
    ) %>%
    filter(year_read >= 2018)
  
}

### EXTRACT DATA FROM GOODREADS
books_read <- fetch_read_books()

## SAVE DATA
write_csv(books_read, path='data/books_read.csv')

