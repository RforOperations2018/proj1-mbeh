# Project 1
# By Min Yan BEH (mbeh)

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(RColorBrewer)

# ui configuration
sidebarWidth <- 300
plotlyDefaultFont <- list(
  family = "Arial",
  size = 18,
  color = "#000000"
)

# load dataset (to be filtered by reactive function later)
movies.load <- read.csv('data/movies.csv', stringsAsFactors=FALSE)
genres <- c('Action', 'Animation', 'Comedy', 'Crime', 'Drama', 'Family', 'Fantasy', 
            'History', 'Mystery', 'Romance', 'Science Fiction', 'Thriller', 'War')
# note: these genres do not represent all genres in the dataset, but are particularly popular ones
pdf(NULL)

# Define header, sidebar and body of shinydashboard
header <- dashboardHeader(title = span(tagList(icon("area-chart"), "MovieCharts")), titleWidth = sidebarWidth)
sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    # Sidebar Menu fo DataTable & Plots
    menuItem("Database", icon = icon("table"), tabName = "table"),
    menuItem("Visualizations", icon = icon("bar-chart"), tabName = "plots"),
    # Range Slider for Movie Release Year
    sliderInput("yearSelect",
                "Year of Movie Release:",
                width = sidebarWidth - 60,
                min = min(movies.load$release_year, na.rm = T),
                max = max(movies.load$release_year, na.rm = T),
                value = c(min(movies.load$release_year, na.rm = T), max(movies.load$release_year, na.rm = T)),
                sep = '',
                step = 1),
    # Numeric Input for Minimum Movie Revenue
    numericInput("revenueMinimum", 
                 "Minimum Revenue in USD",
                 width = sidebarWidth - 60,
                 min = min(movies.load$revenue, na.rm = T),
                 max = max(movies.load$revenue, na.rm = T),
                 value = min(movies.load$revenue, na.rm = T),
                 step = 100000),
    # Selection for Movie Genre
    selectInput("genreSelect",
                "Genre:",
                width = sidebarWidth - 50,
                choices = sort(unique(genres)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Action", "Animation", "Comedy", "Drama", "Fantasy", "Romance")),
    # Button for selecting all genres
    actionButton("selectAllGenres", "Select All Genres", icon = icon("hand-pointer-o"))
  )
)
body <- dashboardBody(
  # import custom css stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(rel = "shortcut icon", href = "favicon-chart.ico")
  ),
  width = 400,
  tabItems(
    # Plots page, to be displayed when "Plots" is clicked on sidebar
    tabItem("plots",
            fluidRow(
              # Tab Panel for 3 plots
              tabBox(title = "",
                     width = 12,
                     tabPanel("Revenue vs Budget", plotlyOutput("plot_budget_and_revenue")),
                     tabPanel("Ratings by Genre", plotlyOutput("plot_ratings")),
                     tabPanel("Movie Duration", plotlyOutput("plot_durations")))
            )
    ),
    # DataTable page, to be displayed when "Table" is clicked on sidebar
    tabItem("table",
            fluidPage(
              div(class = "btn-download", downloadButton("downloadMovieData","Download Data")),
              box(title = "Movie Selection", DT::dataTableOutput("moviesTable"), width = 12))
    )
  )
)

# Define ui comprising of header, sidebar and body (defined above)
ui <- dashboardPage(title = "MovieCharts",
                    header, sidebar, body, skin = "purple")

# Define server logic
server <- function(input, output, session = session) {
  
  # Filtered movie data using reactive method
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
    movies <- movies.load %>% filter(release_year >= input$yearSelect[1] & release_year <= input$yearSelect[2],
                                     # Numeric Filter for Minimum Revenue
                                     revenue >= input$revenueMinimum)
    
    # Selection Filter for Movie Genre
    if (length(input$genreSelect) > 0){
      movies <- filter(movies, movieMatchesGenreInput(genres, input$genreSelect))
    }
    return(movies)
  })
  
  # A plot showing a line chart of movie budget and revenue over the years
  output$plot_budget_and_revenue <- renderPlotly({
    # Aggregate budget and revenue data by year
    aggregatedDataByYear <- movieData() %>% group_by(release_year) %>% 
      summarise(budget = mean(budget), revenue = mean(revenue))
    # Plot movie budget and revenue over the years
    plot_ly(aggregatedDataByYear, x = ~release_year, y = ~revenue, name = 'Revenue', type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~budget, name = 'Budget', mode = 'lines+markers') %>%
      layout(title = "Over the years: Average Revenue vs Budget for Blockbusters",
             xaxis = list(title = "Year", titlefont = plotlyDefaultFont),
             yaxis = list(title = "Amount in USD$", titlefont = plotlyDefaultFont),
             legend = list(orientation = "h", x = 0, y = -0.3),
             height = 400)
  })
  
  # A plot showing a bar chart of average ratings per genre
  output$plot_ratings <- renderPlotly({
    movies <- movieData()
    # Helper method for converting genre name to its column name in the dataset
    convertGenreToColname <- function(genre){
      col_name = tolower(genre)
      if(genre == 'Science Fiction'){
        col_name = 'scifi'
      }
      col_name
    }
    # Calculate the average rating for each selected genre and add to vector
    averageRatingForEachGenre = c()
    for(genre in input$genreSelect){
      print(genre)
      moviesForThisGenre <- filter(movies, as.logical(movies[[convertGenreToColname(genre)]]))
      print(nrow(moviesForThisGenre))
      averageRatingForEachGenre <- c(averageRatingForEachGenre, mean(moviesForThisGenre$Rating))
    }
    # Plot average ratings by MovieLens users for each genre
    plot_ly(x = input$genreSelect, y = averageRatingForEachGenre, type = 'bar', 
            marker = list(color = brewer.pal(length(input$genreSelect), "Greens"))) %>%
      layout(title = "Average Ratings by Genre",
             xaxis = list(title = "Genre", titlefont = plotlyDefaultFont),
             yaxis = list(title = "Average Ratings", titlefont = plotlyDefaultFont),
             height = 400)
  })
  
  # A plot showing a histogram of movie durations
  output$plot_durations <- renderPlotly({
    plot_ly(x = movieData()$runtime, type = 'histogram') %>%
      layout(title = "Histogram of Movie Durations",
             xaxis = list(title = "Duration in Seconds", titlefont = plotlyDefaultFont),
             yaxis = list(title = "Number of Movies", titlefont = plotlyDefaultFont),
             height = 400)
    
  })
  
  # Data table of Movies (based on reactive selection)
  output$moviesTable <- DT::renderDataTable({
    subset(movieData() %>% arrange(desc(release_year), desc(revenue)), 
           select = c(title, genres, release_date, budget, revenue, Rating))
  }, 
  # Customize column names of Data Table
  colnames = c("Title", "Genre", "Release Date", "Budget", "Revenue", "Ratings (/10)")
  )

  # Observe clicks on 'Select All Genres' button
  observeEvent(input$selectAllGenres, {
    # Send error notification if all genres have already been selected
    if (length(input$genreSelect) == length(genres)){
      showNotification("You have already selected all genres!", type = "error")
      # Otherwise, update input for genre selection and send success notification
    }else{
      updateSelectInput(session, "genreSelect", selected = genres)
      showNotification("Success! You have selected all genres!", type = "message")
    }
  })
  
  # Download filtered data from the movie datatable
  output$downloadMovieData <- downloadHandler(
    filename = function() {
      paste("movies-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      dataForDownload = subset(movieData() %>% arrange(desc(release_year), desc(revenue)),
                               select = c(title, genres, release_date, budget, revenue, Rating))
      write.csv(dataForDownload, file, row.names=FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)