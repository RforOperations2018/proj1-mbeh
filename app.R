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
    menuItem("Genre Analysis", icon = icon("table"), tabName = "genres"),
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
                       selectInput("companySelect2",
                                   "Production Company Filter:",
                                   choices = sort(unique(companies)),
                                   multiple = TRUE,
                                   selectize = TRUE,
                                   selected = c("Marvel Studios", "DC Comics")),
                       # Button for selecting all production companies
                       actionButton("selectAllCompaniesPage2", "Select All Companies", icon = icon("hand-pointer-o"))
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
              fluidRow(
                column(width = 4,
                       wellPanel(
                         # Selection for Movie Genre
                         selectInput("companySelect3",
                                     "Production Company Filter:",
                                     choices = sort(unique(companies)),
                                     multiple = TRUE,
                                     selectize = TRUE,
                                     selected = c("Walt Disney Pictures", "Warner Bros", "Marvel Studios", "DC Comics")),
                         # Button for selecting all production companies
                         actionButton("selectAllCompaniesPage3", "Select All Companies", icon = icon("hand-pointer-o"))
                       )
                ),
                column(width = 8,
                       # Bar Chart of Genres
                       plotlyOutput("genres_barchart")
                )
              ),
              fluidRow(
                box(title = "Genre Counts in Tabular Format", DT::dataTableOutput("genreCountsTable"), width = 12)
              )
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
  
  # :::COMPANY REACTIVE METHOD for Page 2::: Filter movie dataset based on production company input
  movieDataWithCompanyFilterPage2 <- reactive({
    reactive_movie_data <- as.data.frame(movieData())  # retrieve movie data from reactive method
    company_selection <- companies         # use all companies if there were 0 companies selected
    if (length(input$companySelect2) > 0){
      company_selection <- input$companySelect2 # retrieve selection filter input for Production Company
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
  
  # :::COMPANY REACTIVE METHOD for Page 3::: Similar to method above but for Page 3 inputs/outputs
  movieDataWithCompanyFilterPage3 <- reactive({
    reactive_movie_data <- as.data.frame(movieData()) 
    company_selection <- companies
    if (length(input$companySelect3) > 0){
      company_selection <- input$companySelect3 # retrieve selection filter input for Production Company
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
  
  # :::GENRE PAGE REACTIVE DATA METHOD::: Melt movie dataset into genre counts for barchart usage
  movieDataCountsByGenre <- reactive({
    movieDataWithCompanyFilterPage3() %>% select(Company, genres, everything()) %>% 
      group_by(Company) %>% 
      summarize(Total=n(), Action=sum(Action), Adventure=sum(Adventure), Animation=sum(Animation), 
                Comedy=sum(Comedy), Crime=sum(Crime), Documentary=sum(Documentary), Drama=sum(Drama), 
                Family=sum(Family), Fantasy=sum(Fantasy), History=sum(History), Horror=sum(Horror),
                Romance=sum(Romance), Scifi=sum(Scifi), Thriller=sum(Thriller), War=sum(War)) %>% 
      select(Company, input$genreSelect) # only select genres picked by the user
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
  
  # :::COMPANY COMPARISON CHART::: Scatterplot that shows budget comparison for selected production companies
  output$company_profits <- renderPlotly({
    filtered_data <- movieDataWithCompanyFilterPage2() # retrieve filtered movie data from reactive method
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
    melted_genre_counts <- movieDataCountsByGenre() %>% melt(id.vars = 1, variable.name = 'Genre', value.name = 'Counts')
    barchart <- ggplot(melted_genre_counts, aes(x = Genre, y = Counts, fill = Company,
                                                text = paste0("<b>", Company, "</b>",
                                                              "<br>Genre: ", Genre,
                                                              "<br>Count: ", Counts))) +
                  geom_bar(stat="identity", position = "dodge") +
                  ggtitle("Genre Profiles for Each Company") +
                  labs(fill = "") # hide legend for cleaner graphing
    ggplotly(barchart, tooltip = "text", height = 350)
  })
  
  # Data Table of Genre Counts (based on reactive selection)
  output$genreCountsTable <- DT::renderDataTable({
    reactive_data <- movieDataCountsByGenre()
    counts <- as.data.frame(t(reactive_data[,-1]))
    colnames(counts) <- reactive_data$Company
    counts
  })

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
  observeEvent(input$selectAllCompaniesPage2, {
    # Send error notification if all genres have already been selected
    if (length(input$companySelect2) == length(companies)){
      showNotification("Already selected all of them!", type = "error")
    }else{
      # Otherwise, update input for genre selection and send success notification
      updateSelectInput(session, "companySelect2", selected = companies)
      showNotification("All companies are selected!", type = "message")
    }
  })
  
  # Observer - similar to above method but for page 3
  observeEvent(input$selectAllCompaniesPage3, {
    # Send error notification if all genres have already been selected
    if (length(input$companySelect3) == length(companies)){
      showNotification("Already selected all of them!", type = "error")
    }else{
      # Otherwise, update input for genre selection and send success notification
      updateSelectInput(session, "companySelect3", selected = companies)
      showNotification("All companies are selected!", type = "message")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)