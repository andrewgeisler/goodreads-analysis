header <- dashboardHeader(
  title = 'Good Reads Stats',
  titleWidth = 275
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    input_select_period,
    menuItem("Summary Stats", tabName = "summary_stats", icon = icon("dashboard")),
    menuItem("Book Calendar", icon = icon("calendar"), tabName = "calendar"),
    menuItem("Trends", icon = icon("line-chart"), tabName = "trends"),
    menuItem("Notes", icon = icon("book"), tabName = "notes")
  )
)

body <- dashboardBody(
  height='1500px', 
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tabItems(
    tabItem(
      
      tabName = "summary_stats",
      fluidRow(
        column(12, span(class='head', 'Summary Stats')),
        column(12,
          column(
            width = 3,
            box(class = "majorBox", width = 12, textOutput('current_books'), span(class='caption','Books')),
            box(class = "comparisonBox", width = 12, textOutput('prior_books'), span(class='caption','Prior Period'))
          ),
          column(
            width = 3,
            box(class = "majorBox", width = 12, textOutput('current_pages'), span(class='caption','Pages')),
            box(class = "comparisonBox", width = 12, textOutput('prior_pages'), span(class='caption','Prior Period'))
          ),
          column(
            width = 3,
            box(class = "majorBox", width = 12, textOutput('current_ppd'), span(class='caption','AVG Pages/Day')),
            box(class = "comparisonBox", width = 12, textOutput('prior_ppd'), span(class='caption','Prior Period'))
          ),
          column(
            width = 3,
            box(class = "majorBox", width = 12, textOutput('current_dc'), span(class='caption','AVG Days to Complete')),
            box(class = "comparisonBox", width = 12, textOutput('prior_dc'), span(class='caption','Prior Period'))
          )
      ),
      column(12,
        column(6, span(class='head','Top Authors by Total Books'), plotlyOutput('p_authors_books')),
        column(6, span(class='head','Top Authors by Total Pages'), plotlyOutput('p_authors_pages'))
      ),
      column(12,
        column(6, span(class='head','Top Genre Tags'), plotlyOutput('p_genres')),
        column(6, span(class='head','Sentiment Analysis'), plotlyOutput('p_sentiment'))
      )
    )),
    
    tabItem(
        tabName = 'calendar',
        fluidRow(
        column(12, span(class='head','Book Calendar'), plotlyOutput('p_calendar')),
        tags$p(),
        column(12, dataTableOutput('book_list'))
    )),
    
    tabItem(
      tabName = 'trends',
      fluidRow(
      column(12,
        column(6,span(class='head','Total Books'), plotlyOutput('p_books_read')),
        column(6,span(class='head','Total Pages'), plotlyOutput('p_pages_read'))
      ),
        column(12,span(class='head','Reading Pace (30 Day Rolling Average Pages Read Per Day'), plotlyOutput('p_reading_pace'))
      )
    ),
   tabItem(
     tabName = 'notes',
     column(12,#style = 'padding-left:10px;font-size:85%;', 
      tags$p(),
      "This dashboard is designed to monitor reading \n patterns based on data logged on goodreads.com. The data needs to be manually updated based on API authentication credentials and shinyapps.io.", 
       tags$p(),
       "The stats are updated by selected different time periods and comparisons are made against either the preceding number of days OR a comparable period/number of days as is the case when select current month or year.",
       tags$p(),
       "Pages read per day is estimated using the total number of pages and total days read. This assumes an static pace which is not accurate. However, this can be considered a decent proxy for estimating reading pace." 
     )  
    )
  )
)


dashboardPage(header,sidebar,body)