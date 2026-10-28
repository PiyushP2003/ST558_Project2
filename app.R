library(shiny)
library(tidyverse)
library(arrow)
library(DT)


# Load and prepare data
nba <- read_parquet("data/nba_2025_26.parquet")

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
      
      selectInput(
        "team_filter",
        "Team:",
        choices = c("All", sort(unique(nba$team))),
        selected = "All"
      ),
      
      radioButtons(
        "location_filter",
        "Game Location:",
        choices = c("All", "Home", "Away"),
        selected = "All"
      ),
      
      hr(),
      
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
      
      uiOutput("numeric_slider1"),
      
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
      
      uiOutput("numeric_slider2"),
      
      br(),
      
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
            "The table below displays the data based on the filters selected
            in the sidebar."
          ),
          
          downloadButton(
            "download_data",
            "Download Filtered Data"
          ),
          
          br(),
          br(),
          
          DT::dataTableOutput("data_table")
        ),
        
        
        # Data Exploration Tab
        tabPanel(
          "Data Exploration",
          
          h3("Explore the Data"),
          
          p(
            "The summaries below are based on the filtered data."
          ),
          
          h4("Numerical Summary"),
          
          DT::dataTableOutput("numerical_summary"),
          
          br(),
          
          h4("One-Way Contingency Table"),
          
          p("Number of player-game observations by team."),
          
          DT::dataTableOutput("one_way_table"),
          
          br(),
          
          h4("Two-Way Contingency Table"),
          
          p("Number of player-game observations by team and game location."),
          
          DT::dataTableOutput("two_way_table")
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  # Dynamic numeric sliders
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
  
  
  # Filter data when Apply Filters is pressed
  filtered_data <- eventReactive(input$apply_filters, {
    
    req(
      input$numeric_range1,
      input$numeric_range2
    )
    
    data <- nba
    
    if (input$team_filter != "All") {
      data <- data |>
        filter(team == input$team_filter)
    }
    
    if (input$location_filter != "All") {
      data <- data |>
        filter(location == input$location_filter)
    }
    
    data <- data |>
      filter(
        .data[[input$numeric_var1]] >= input$numeric_range1[1],
        .data[[input$numeric_var1]] <= input$numeric_range1[2]
      )
    
    data <- data |>
      filter(
        .data[[input$numeric_var2]] >= input$numeric_range2[1],
        .data[[input$numeric_var2]] <= input$numeric_range2[2]
      )
    
    data
    
  }, ignoreNULL = FALSE)
  
  
  # Data table
  output$data_table <- DT::renderDataTable({
    
    filtered_data()
    
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  
  # Download filtered data
  output$download_data <- downloadHandler(
    
    filename = function() {
      "filtered_nba_data.csv"
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
  
  
  # Numerical summary
  output$numerical_summary <- DT::renderDataTable({
    
    data <- filtered_data()
    
    var1 <- data[[input$numeric_var1]]
    var2 <- data[[input$numeric_var2]]
    
    summary_table <- tibble(
      Variable = c(input$numeric_var1, input$numeric_var2),
      Mean = c(
        mean(var1, na.rm = TRUE),
        mean(var2, na.rm = TRUE)
      ),
      Median = c(
        median(var1, na.rm = TRUE),
        median(var2, na.rm = TRUE)
      ),
      SD = c(
        sd(var1, na.rm = TRUE),
        sd(var2, na.rm = TRUE)
      ),
      Min = c(
        min(var1, na.rm = TRUE),
        min(var2, na.rm = TRUE)
      ),
      Max = c(
        max(var1, na.rm = TRUE),
        max(var2, na.rm = TRUE)
      )
    ) |>
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      )
    
    summary_table
    
  }, options = list(
    dom = "t"
  ), rownames = FALSE)
  
  
  # One-way contingency table
  output$one_way_table <- DT::renderDataTable({
    
    filtered_data() |>
      count(team, name = "Count") |>
      arrange(desc(Count))
    
  }, options = list(
    pageLength = 10
  ), rownames = FALSE)
  
  
  # Two-way contingency table
  output$two_way_table <- DT::renderDataTable({
    
    filtered_data() |>
      count(team, location) |>
      pivot_wider(
        names_from = location,
        values_from = n,
        values_fill = 0
      )
    
  }, options = list(
    pageLength = 10
  ), rownames = FALSE)
}


# Run Application
shinyApp(ui = ui, server = server)