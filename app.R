library(shiny)
library(tidyverse)
library(arrow)
library(DT)

nba <- read_parquet("data/nba_2025_26.parquet")

# Create a more readable game location variable
nba <- nba |>
  mutate(
    location = if_else(home, "Home", "Away")
  )


# User Interface
ui <- fluidPage(
  
  titlePanel("NBA Player Performance Explorer"),
  
  sidebarLayout(
    
    # Sidebar
    sidebarPanel(
      
      h4("Filter Data"),
      
      p(
        "Select the desired filters and click Apply Filters to update the data."
      ),
      
      # Categorical filter 1
      selectInput(
        "team_filter",
        "Team:",
        choices = c("All", sort(unique(nba$team))),
        selected = "All"
      ),
      
      # Categorical filter 2
      radioButtons(
        "location_filter",
        "Game Location:",
        choices = c("All", "Home", "Away"),
        selected = "All"
      ),
      
      hr(),
      
      # Numeric variable 1
      selectInput(
        "numeric_var1",
        "Numeric Variable 1:",
        choices = c(
          "Points" = "pts",
          "Rebounds" = "reb",
          "Assists" = "ast",
          "Minutes" = "minutes",
          "Steals" = "stl",
          "Blocks" = "blk",
          "Turnovers" = "tov",
          "Plus/Minus" = "plus_minus"
        ),
        selected = "pts"
      ),
      
      # Dynamic slider for numeric variable 1
      uiOutput("numeric_slider1"),
      
      # Numeric variable 2
      selectInput(
        "numeric_var2",
        "Numeric Variable 2:",
        choices = c(
          "Points" = "pts",
          "Rebounds" = "reb",
          "Assists" = "ast",
          "Minutes" = "minutes",
          "Steals" = "stl",
          "Blocks" = "blk",
          "Turnovers" = "tov",
          "Plus/Minus" = "plus_minus"
        ),
        selected = "ast"
      ),
      
      # Dynamic slider for numeric variable 2
      uiOutput("numeric_slider2"),
      
      br(),
      
      # Apply filters button
      actionButton(
        "apply_filters",
        "Apply Filters",
        class = "btn-primary"
      )
    ),
    
    
    # Main Panel
    mainPanel(
      
      tabsetPanel(
        
        # About Tab
        tabPanel(
          "About",
          
          h3("About This App"),
          
          p(
            "This application allows users to explore player performance
            during the 2025-26 NBA season. Users can filter the data and
            investigate player performance using numerical summaries,
            tables, and graphical visualizations."
          ),
          
          h4("Data"),
          
          p(
            "The dataset contains individual player-game observations from
            the 2025-26 NBA season, including points, rebounds, assists,
            minutes played, shooting statistics, and other performance
            measures."
          ),
          
          p(
            "The data were obtained from the NBA Game Logs dataset available
            on Hugging Face. ",
            
            tags$a(
              href = paste0(
                "https://huggingface.co/datasets/",
                "cbratkovics/nba-game-logs"
              ),
              "View the data source",
              target = "_blank"
            )
          ),
          
          h4("Using the App"),
          
          p(
            "Use the sidebar to filter the dataset by team, game location,
            and two selected numeric variables. After choosing the desired
            filters, press the Apply Filters button to update the data used
            throughout the application."
          ),
          
          tags$ul(
            tags$li(
              strong("About: "),
              "Provides information about the application and dataset."
            ),
            
            tags$li(
              strong("Data Download: "),
              "Displays the filtered data and allows the user to download it."
            ),
            
            tags$li(
              strong("Data Exploration: "),
              "Provides interactive numerical summaries, contingency tables,
              and graphical visualizations."
            )
          ),
          
          h4("NBA Player Performance"),
          
          tags$img(
            src = "basketball.png.avif",
            width = "350px",
            alt = "Basketball"
          )
        ),
        
        
        # Data Download Tab
        tabPanel(
          "Data Download",
          
          h3("View and Download Data"),
          
          p(
            "The filtered NBA data will be displayed here."
          )
        ),
        
        
        # Data Exploration Tab
        tabPanel(
          "Data Exploration",
          
          h3("Explore the Data"),
          
          p(
            "Interactive summaries and visualizations will be displayed here."
          )
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  output$numeric_slider1 <- renderUI({
    
    req(input$numeric_var1)
    
    variable <- nba[[input$numeric_var1]]
    
    sliderInput(
      "numeric_range1",
      "Select Range:",
      min = min(variable, na.rm = TRUE),
      max = max(variable, na.rm = TRUE),
      value = c(
        min(variable, na.rm = TRUE),
        max(variable, na.rm = TRUE)
      )
    )
  })
  
  
  output$numeric_slider2 <- renderUI({
    
    req(input$numeric_var2)
    
    variable <- nba[[input$numeric_var2]]
    
    sliderInput(
      "numeric_range2",
      "Select Range:",
      min = min(variable, na.rm = TRUE),
      max = max(variable, na.rm = TRUE),
      value = c(
        min(variable, na.rm = TRUE),
        max(variable, na.rm = TRUE)
      )
    )
  })
  
  
  filtered_data <- eventReactive(input$apply_filters, {
    
    req(
      input$numeric_range1,
      input$numeric_range2
    )
    
    data <- nba
    
    
    # Filter by team
    if (input$team_filter != "All") {
      
      data <- data |>
        filter(team == input$team_filter)
    }
    
    
    # Filter by game location
    if (input$location_filter != "All") {
      
      data <- data |>
        filter(location == input$location_filter)
    }
    
    
    # Filter by first numeric variable
    data <- data |>
      filter(
        .data[[input$numeric_var1]] >= input$numeric_range1[1],
        .data[[input$numeric_var1]] <= input$numeric_range1[2]
      )
    
    
    # Filter by second numeric variable
    data <- data |>
      filter(
        .data[[input$numeric_var2]] >= input$numeric_range2[1],
        .data[[input$numeric_var2]] <= input$numeric_range2[2]
      )
    
    
    data
    
  }, ignoreNULL = FALSE)
  
}


# Run Application
shinyApp(ui = ui, server = server)