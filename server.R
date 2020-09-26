library(shiny)
library(shinythemes)
library(DT)
library(scales)



shinyServer(function(input, output) {
  
  ## REACTIVE FILTERED PERIOD DATES
  reactive_dates <- reactive({
    periods %>%
      filter(period == input$period)
  })
  
  ## CREATE REACTIVE BOOKS READ OBJECT
  books <- reactive({
    books_read %>%
      mutate(
        period = case_when(
          read_at >= reactive_dates()$start_current & read_at < reactive_dates()$end_current ~ 'current',
          read_at >= reactive_dates()$start_prior & read_at < reactive_dates()$end_prior ~ 'prior'
        )) %>%
      group_by(period) %>%
      summarize(
        books = n(),
        pages = sum(num_pages),
        est_pages_per_day = pages/reactive_dates()$days_in_period,
        days_read = reactive_dates()$days_in_period/books
      ) %>%
      pivot_longer(-period, names_to = 'measure') %>%
      pivot_wider(names_from=period, values_from=value) %>%
      mutate(
        percent_change = (current/prior)-1
      )
  })
  
  author_summary <- reactive({
    books_read %>%
      filter(read_at >= reactive_dates()$start_current & read_at < reactive_dates()$end_current) %>%
      group_by(author_name) %>%
      summarize(total_books = n(), total_pages = sum(num_pages)) %>%
      ungroup()
  })
  
  top_genres <- reactive({
    top_shelves %>%
      inner_join(books_read) %>%
      filter(read_at >= reactive_dates()$start_current & read_at < reactive_dates()$end_current) %>%
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
  })
  
  output$current_books <- renderText({
      books() %>%
        filter(measure == 'books') %>%
        pull(current) %>%
        scales::comma()
  })
  
  output$prior_books <- renderText({
    books() %>%
      filter(measure == 'books') %>%
      pull(percent_change) %>%
      scales::percent()
  })
  
  output$current_pages <- renderText({
    books() %>%
      filter(measure == 'pages') %>%
      pull(current) %>%
      scales::comma()
  })
  
  output$prior_pages <- renderText({
    books() %>%
      filter(measure == 'pages') %>%
      pull(percent_change) %>%
      scales::percent()
  })
  
  output$current_ppd <- renderText({
    books() %>%
      filter(measure == 'est_pages_per_day') %>%
      pull(current) %>%
      scales::comma()
  })
  
  output$prior_ppd <- renderText({
    books() %>%
      filter(measure == 'est_pages_per_day') %>%
      pull(percent_change) %>%
      scales::percent()
  })
  
  output$current_dc <- renderText({
    books() %>%
      filter(measure == 'days_read') %>%
      pull(current) %>%
      scales::comma()
  })
  
  output$prior_dc <- renderText({
    books() %>%
      filter(measure == 'days_read') %>%
      mutate(percent_change = percent_change*-1) %>%
      pull(percent_change) %>%
      scales::percent()
  })
  
  output$p_calendar <- renderPlotly({
    p_book_calendar <- books_read %>%
      ggplot(
        aes(
          x = started_at,
          y = round(num_pages/days_read,2),
          xend = read_at,
          yend = round(num_pages/days_read,2),
          color = title,
          text = str_c(
            'Title: ',title, '<br />',
            'Started At: ', started_at, '<br />',
            'Reading Pace: ', round(num_pages/days_read,2)
            )
        )
      ) +
      geom_segment(size=1, alpha = .5) +
      geom_point() +
      geom_point(aes(x=read_at)) +
      scale_y_continuous('Reading Pace', limits = c(0,150)) + 
      scale_x_date(date_labels = '%b-%y', date_breaks = '1 month') +
      theme_minimal() +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle=90),
        axis.title = element_blank(),
        panel.grid.minor = element_blank()
      )  +
      plot_fill +
      plot_color
    
    p_book_calendar %>%
      ggplotly(tooltip = c("text"))
      
  })
  
  output$p_books_read <- renderPlotly({
    plot_ly(
      df_daily_books_read,
      x = ~ floor_date(today(), 'year')+yday,
      y = ~ books,
      color = ~ factor(year),
      colors = rev(color_pals[1:2]),
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5
        ),  
        hovermode = "x unified",
        font = plotly_font,
        xaxis = list(title = ''),
        yaxis = list(title = '')
      )
  })
  
  output$p_pages_read <- renderPlotly({
    plot_ly(
      df_daily_cume_pages,
      x = ~ floor_date(today(), 'year')+yday,
      y = ~ round(total_pages),
      color = ~ factor(year),
      colors = rev(color_pals[1:2]),
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          xanchor = "center",
          x = 0.5
        ),  
        hovermode = "x unified",
        font = plotly_font,
        xaxis = list(title = ''),
        yaxis = list(title = '')
      )
  })
  
  output$p_reading_pace <- renderPlotly({
    plot_ly(
      df_daily_cume_pages,
      x = ~ date,
      y = ~ round(ppd_rolling,2),
      color = ~ factor(year),
      colors = rev(color_pals[1:2]),
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5
        ),  
        hovermode = "x unified",
        font = plotly_font,
        xaxis = list(title = ''),
        yaxis = list(title = 'Pages Per Day')
      )
  })
  
  output$p_authors_pages <- renderPlotly({
    author_summary() %>%
      top_n(20, total_pages) %>%
      mutate(
        author_name = factor(author_name),
        author_name = reorder(author_name, total_pages)
      ) %>%
      plot_ly(
        x = ~ total_pages,
        y = ~ author_name,
        color = ~ author_name,
        colors = rev(color_pals[1:10]),
        text = ~ total_pages,
        type = 'bar',
        hoverinfo = 'x'
      ) %>%
      add_text(
        textfont = list(color = '#404040'),
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
  })
  
  output$p_authors_books <- renderPlotly({
    author_summary() %>%
      arrange(desc(total_books),desc(total_pages)) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= 20) %>%
      mutate(
        author_name = factor(author_name),
        author_name = reorder(author_name, total_books)
      ) %>%
      plot_ly(
        x = ~ total_books,
        y = ~ author_name,
        color = ~ author_name,
        colors = rev(color_pals[1:10]),
        text = ~ total_books,
        type = 'bar',
        hoverinfo = 'x'
      ) %>%
      add_text(
        textfont = list(color = '#404040'),
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
  })
  
  output$p_genres <- renderPlotly({
    top_genres() %>%
      top_n(50, percent_of_books) %>%
      mutate(
        shelf = factor(shelf),
        shelf = reorder(shelf, percent_of_books)
      ) %>%
      plot_ly(
        x = ~ shelf,
        y = ~ percent_of_books,
        color = ~ shelf,
        colors = rev(color_pals[1:8]),
        type = 'bar',
        text = ~ scales::percent(percent_of_books),
        hoverinfo = 'y'
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
  })
  
  output$p_sentiment <- renderPlotly({
    book_sentiments %>%
      left_join(books_read) %>%
      filter(read_at >= reactive_dates()$start_current & read_at < reactive_dates()$end_current) %>%
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
  })
  
  output$book_list <- renderDataTable({
    books_read %>%
      mutate(
        `AVG Pages Read Per Day` = round(as.numeric(num_pages)/days_read,2)
      ) %>%
      select(
        Title = title,
        Author = author_name,
        `Publication Year` = publication_year,
        Pages = num_pages,
        `Good Reads Rating` = book_average_rating,
        Started = started_at, 
        Finished = read_at,
        `AVG Pages Read Per Day`
      )  %>%
      arrange(desc(Started)) %>%
      DT::datatable(
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 25,
          searching = TRUE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        )
      )
  })
  
})
