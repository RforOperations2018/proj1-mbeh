# Project 1
# By Min Yan BEH (mbeh)

library(shiny)
library(shinydashboard)
library(flexdashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(RColorBrewer)

# Set locale in case of issues loading dataset
Sys.setlocale('LC_ALL','C')

# ui configuration
sidebarWidth <- 250
plotlyDefaultFont <- list(
  family = "Arial",
  size = 18,
  color = "#000000"
)
# Helper function for formatting profit/budget/revenue data on plotly axes
format_financial_value <- function(amount){
  ifelse(amount > 1000000000, 
         paste0('$', round(amount/1000000000, 2), 'B'), # format billions
         ifelse(amount > 0,
                paste0('$', round(amount/1000000, 2), 'M'), # format millions
                paste0('-$', round(-amount/1000000, 2), 'M')) # format negative values
         ) 
}

# load dataset (to be filtered by reactive function later)
movies.load <- read.csv('data/movies.csv', stringsAsFactors=FALSE)
genres <- c('Action', 'Adventure', 'Animation', 'Comedy', 'Crime', 'Documentary', 'Drama', 
            'Family', 'Fantasy', 'History', 'Horror', 'Romance', 'Scifi', 'Thriller', 'War')
companies <- c('Pixar Animation Studios',	'Warner Bros', 'Paramount Pictures', 'Columbia Pictures',
               'Metro Goldwyn Mayer',	'Universal Pictures', 'Lucasfilm', 'Walt Disney Pictures', 
               'Marvel Studios',	'DC Comics')
# note: these companies do not represent all companies in the dataset, but are particularly popular ones
pdf(NULL)

# Define header, sidebar and body of shinydashboard
header <- dashboardHeader(title = span(tagList(icon("area-chart"), "MovieCharts")), titleWidth = sidebarWidth)
sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    # Sidebar Menu for 3 pages
    menuItem("Profitable Movies", icon = icon("dollar"), tabName = "profits"),
    menuItem("Battle of the Cos", icon = icon("bar-chart"), tabName = "companies"),
    menuItem("Genre Analysis", icon = icon("bar-chart"), tabName = "genres"),
    menuItem("Database", icon = icon("table"), tabName = "table"),
    # Range Slider for Movie Release Year
    sliderInput("yearSelect",
                "Year of Movie Release:",
                width = sidebarWidth - 50,
                min = min(movies.load$Year, na.rm = T),
                max = max(movies.load$Year, na.rm = T),
                value = c(min(movies.load$Year, na.rm = T), max(movies.load$Year, na.rm = T)),
                sep = '',
                step = 1),
    # Selection for Movie Genre
    selectInput("genreSelect",
                "Genre Filter:",
                width = sidebarWidth - 50,
                choices = sort(unique(genres)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Action", "Comedy")),
    # Button for selecting all genres
    actionButton("selectAllGenres", "Select All Genres", icon = icon("hand-pointer-o"))
  )
)
body <- dashboardBody(
  # Import custom css stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(rel = "shortcut icon", href = "favicon-chart.ico")
  ),
  tabItems(
    # Movie Profitability page, to be displayed when "Profitable Movies" is clicked on sidebar
    tabItem("profits",
            fluidPage(
              column(width = 4,
                     fluidRow(
                       # Gauge indicator of % of dataset being visualized on the scatterplot
                       box(title="% of Dataset Selected", gaugeOutput("percData", height = "120px"), width = 12),
                       # Info box for displaying Movie Title/Year with highest profit, and profit amount
                       infoBoxOutput("topProfit", width = 12),
                       # Value box for displaying average profit
                       valueBoxOutput("avgProfit")
                     )
              ),
              column(width = 8,
                     # Scatterplot of Movie Profitability and Average Ratings
                     plotlyOutput("profit_scatterplot")
              )
            )
    ),
    # Company Comparison page, to be displayed when "Battle of the Cos" is clicked on sidebar
    tabItem("companies",
            fluidPage(
              column(width = 4,
                     wellPanel(
                       # Selection of x-axis value
                       radioButtons("xaxisValue", label = "X-Axis Value:",
                                    choices = list("Movie Duration" = "Runtime", "Average Ratings" = "Rating"), 
                                    selected = "Runtime"),
                       # Selection for specific production companies that filtered movies must be produced by
                       selectInput("companySelect",
                                   "Production Company Filter:",
                                   choices = sort(unique(companies)),
                                   multiple = TRUE,
                                   selectize = TRUE,
                                   selected = c("Marvel Studios", "DC Comics")),
                       # Button for selecting all production companies
                       actionButton("selectAllCompanies", "Select All Companies", icon = icon("hand-pointer-o"))
                     )
              ),
              column(width = 8,
                     # Comparison of budget across companies on a scatterplot
                     plotlyOutput("company_profits")
              )
            )
    ),
    # Genre Breakdown page, to be displayed when "Genre Analysis" is clicked on sidebar
    tabItem("genres",
            fluidPage(
              column(width = 4,
                     wellPanel(
                       # Selection for Movie Genre
                       selectInput("companySelect",
                                   "Production Company Filter:",
                                   choices = sort(unique(companies)),
                                   multiple = TRUE,
                                   selectize = TRUE,
                                   selected = c("Marvel Studios", "DC Comics"))
                     )
              ),
              column(width = 8,
                     # Bar Chart of Genres
                     plotlyOutput("genres_barchart")
              )
            )
    ),
    # DataTable page, to be displayed when "Table" is clicked on sidebar
    tabItem("table",
            fluidPage(
              div(class = "btn-download", downloadButton("downloadMovieData","Download Data")),
              box(title = "Movie Selection", DT::dataTableOutput("moviesTable"), width = 12)
            )
    )
  )
)

# Define ui comprising of header, sidebar and body (defined above)
ui <- dashboardPage(title = "MovieCharts",
                    header, sidebar, body, skin = "purple")

# Define server logic
server <- function(input, output, session = session) {
  
  # :::REACTIVE METHOD::: Filter movie dataset based on user inputs
  movieData <- reactive({
    # Helper function for determining if a movie contains a genre within list of selected genres
    movieMatchesGenreInput <- function(genresString, selectedGenres){
      genresForEachRow = strsplit(genresString, ",")
      outcome = c()
      for(row in genresForEachRow){
        outcome <- c(outcome, length(intersect(row, selectedGenres)) > 0)
      }
      return(outcome)
    }
    # Slider Filter for Release Year of Movie
    movies <- movies.load %>% filter(Year >= input$yearSelect[1] & Year <= input$yearSelect[2])
    
    # Selection Filter for Movie Genre
    if (length(input$genreSelect) > 0){
      movies <- filter(movies, movieMatchesGenreInput(genres, input$genreSelect))
    }
    return(movies)
  })
  
  # :::GAUGES, VALUE/INFO BOXES::: Provide stats based on filtered result from reactive method
  # Info box that calculates most profitable movie based on user input filters
  output$topProfit <- renderInfoBox({
    most_profitable <- movieData() %>% arrange(desc(Profit)) %>% head(1)
    infoBox('Most Profitable Movie', value = paste0(most_profitable$Title, ' (', most_profitable$Year, ')'),
                                                    subtitle = paste("Profit:", format_financial_value(most_profitable$Profit)),
            icon = icon('trophy'), color = 'purple')
  })
  # Value box that calculates average profit of all movies within user input filters
  output$avgProfit <- renderValueBox({
    average_profit <- round(mean(movieData()$Profit, na.rm = T), 2)
    shinydashboard::valueBox(subtitle = "Average Profit (Revenue minus Budget)", 
                             value = format_financial_value(average_profit), icon = icon("dollar"), 
                             color = "purple", width = 12)
  })
  # Gauge indicator of % of dataset filtered vs original dataset
  output$percData <- renderGauge({
    percentage <- round(100 * nrow(movieData()) / nrow(movies.load),2)
    gauge(percentage, min = 0, max = 100, symbol = '%')
  })

  # :::PROFITABILITY CHART::: Scatterplot that displays filtered movies' profits vs average ratings
  output$profit_scatterplot <- renderPlotly({
    scatterplot <- ggplot(movieData(), aes(x = Rating, y = Profit/1000000,
                                           text = paste0("<b>", Title, " (", Year, ")</b>",
                                                         "<br>Revenue: ", format_financial_value(Revenue),
                                                         "<br>Budget: ", format_financial_value(Budget),
                                                         "<br>Profit: ", format_financial_value(Profit),
                                                         "<br>Rating: ", Rating))) + 
      geom_point(color="#605ca8") + 
      geom_hline(yintercept = 0, color = "red", size = 0.5) +
      ggtitle("Movie Profitability and Average Ratings") +
      xlab("Average Rating out of 10") +
      ylab("Profit (in Millions)") +
      scale_y_continuous(label=scales::dollar_format(big.mark=','))
    ggplotly(scatterplot, tooltip = "text", height = 400)
  })
  
  # :::COMPANY REACTIVE METHOD::: Filter movie dataset based on production company input
  movieDataWithCompanyFilter <- reactive({
    reactive_movie_data <- as.data.frame(movieData())  # retrieve movie data from reactive method
    company_selection <- companies         # use all companies if there were 0 companies selected
    if (length(input$companySelect) > 0){
      company_selection <- input$companySelect # retrieve selection filter input for Production Company
    }
    
    filtered_data <- head(reactive_movie_data, 0) # start with empty table populated with columns from dataset
    for(company_display_name in company_selection){
      # convert company display name to corresponding column name in the dataset
      colname <- paste(unlist(strsplit(tolower(company_display_name), ' ')), collapse='')
      movies_by_company <- reactive_movie_data %>% filter(reactive_movie_data[colname] == 1)
      movies_by_company$Company <- company_display_name
      filtered_data <- rbind(filtered_data, movies_by_company)
    }
    return(filtered_data)
  })
  
  # :::COMPANY COMPARISON CHART::: Scatterplot that shows budget comparison for selected production companies
  output$company_profits <- renderPlotly({
    filtered_data <- movieDataWithCompanyFilter() # retrieve filtered movie data from reactive method
    if(input$xaxisValue == "Runtime"){            # modify column for x-axis based on user input
      xaxis_data <- filtered_data$Runtime
      xaxis_label <- "Duration (in minutes)"
      tooltip_field <- "Duration"
    }else{
      xaxis_data <- filtered_data$Rating
      xaxis_label <- "Average Ratings (out of 10)"
      tooltip_field <- "Ratings"
    }
    scatterplot <- ggplot(filtered_data, aes(x = xaxis_data, y = Budget/1000000, color=Company,
                                             text = paste0("<b>", Title, " (", Year, ")</b>",
                                                           "<br>Company: ", Company,
                                                           "<br>", tooltip_field, ": ", xaxis_data,
                                                           "<br>Budget: ", format_financial_value(Profit)))) + 
      geom_point() + 
      ggtitle(paste("Movie", tooltip_field, "versus Budget: With Company Breakdown")) +
      xlab(xaxis_label) +
      ylab("Budget (in Millions)") +
      labs(color = "") + # hide legend for cleaner graphing
      scale_y_continuous(label=scales::dollar_format(big.mark=','))
    ggplotly(scatterplot, tooltip = "text", height = 400)
  })
  
  # :::GENRE BREAKDOWN CHART::: Bar chart that shows genre breakdown for each production company
  output$genres_barchart <- renderPlotly({
    scatterplot <- ggplot(movieDataWithCompanyFilter(), aes(x = Rating, y = Profit/1000000, color=Company,
                                                            text = paste0("<b>", Title, " (", Year, ")</b>",
                                                                          "<br>Rating: ", Rating,
                                                                          "<br>Profit: ", format_financial_value(Profit)))) + 
      geom_point() + 
      geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5) +
      ggtitle("Rating vs Profit") +
      xlab("Rating") +
      ylab("Profit (in Millions)") +
      scale_y_continuous(label=scales::dollar_format(big.mark=','))
    ggplotly(barchart, tooltip = "text", height = 400)
  })
  
  # A plot showing a line chart of movie budget and revenue over the years
  output$plot_budget_and_revenue <- renderPlotly({
    # Aggregate budget and revenue data by year
    aggregatedDataByYear <- movieData() %>% group_by(Year) %>% 
      summarise(Budget = mean(Budget), Revenue = mean(Revenue))
    # Plot movie budget and revenue over the years
    plot_ly(aggregatedDataByYear, x = ~Year, y = ~Revenue, name = 'Revenue', type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~Budget, name = 'Budget', mode = 'lines+markers') %>%
      layout(title = "Over the years: Average Revenue vs Budget for Blockbusters",
             xaxis = list(title = "Year", titlefont = plotlyDefaultFont),
             yaxis = list(title = "Amount in USD$", titlefont = plotlyDefaultFont),
             legend = list(orientation = "h", x = 0, y = -0.3),
             height = 400)
  })
  
  # A plot showing a bar chart of average ratings per genre
  output$plot_ratings <- renderPlotly({
    movies <- movieData()
    # Calculate the average rating for each selected genre and add to vector
    averageRatingForEachGenre = c()
    for(genre in input$genreSelect){
      print(genre)
      moviesForThisGenre <- filter(movies, as.logical(movies[[genre]]))
      print(nrow(moviesForThisGenre))
      averageRatingForEachGenre <- c(averageRatingForEachGenre, mean(moviesForThisGenre$Rating))
    }
    # Plot average ratings by MovieLens users for each genre
    plot_ly(x = input$genreSelect, y = averageRatingForEachGenre, type = 'bar', 
            marker = list(color = brewer.pal(length(input$genreSelect), "Greens"))) %>%
      layout(title = "Average Ratings by Genre",
             xaxis = list(title = "Genre", titlefont = plotlyDefaultFont),
             yaxis = list(title = "Average Ratings", titlefont = plotlyDefaultFont))
  })
  
  # A plot showing a histogram of movie durations
  output$plot_durations <- renderPlotly({
    plot_ly(x = movieData()$Runtime, type = 'histogram') %>%
      layout(title = "Histogram of Movie Durations",
             xaxis = list(title = "Duration in Seconds", titlefont = plotlyDefaultFont),
             yaxis = list(title = "Number of Movies", titlefont = plotlyDefaultFont))
    
  })
  
  # Data table of Movies (based on reactive selection)
  output$moviesTable <- DT::renderDataTable({
    subset(movieData() %>% arrange(desc(Year), desc(Revenue)), 
           select = c(Title, Year, Profit, Budget, Revenue, Rating))
  }
  # Customize column names of Data Table
  # colnames = c("Title", "Year", "Budget", "Revenue", "Ratings (/10)")
  )

  # Observe clicks on 'Select All Genres' button
  observeEvent(input$selectAllGenres, {
    # Send error notification if all genres have already been selected
    if (length(input$genreSelect) == length(genres)){
      showNotification("You've already selected all genres!", type = "error")
    }else{
      # Otherwise, update input for genre selection and send success notification
      updateSelectInput(session, "genreSelect", selected = genres)
      showNotification("Success! You selected all genres!", type = "message")
    }
  })

  # Observe clicks on 'Select All Companies' button
  observeEvent(input$selectAllCompanies, {
    # Send error notification if all genres have already been selected
    if (length(input$companySelect) == length(companies)){
      showNotification("Already selected all of them!", type = "error")
    }else{
      # Otherwise, update input for genre selection and send success notification
      updateSelectInput(session, "companySelect", selected = companies)
      showNotification("All companies are selected!", type = "message")
    }
  })
  
  # Download filtered data from the movie datatable
  output$downloadMovieData <- downloadHandler(
    filename = function() {
      paste("movies-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      dataForDownload = subset(movieData() %>% arrange(desc(Year), desc(revenue)),
                               select = c(title, genres, release_date, budget, revenue, Rating))
      write.csv(dataForDownload, file, row.names=FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)