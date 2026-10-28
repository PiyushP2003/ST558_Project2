library(shiny)
library(tidyverse)
library(arrow)
library(DT)

# Load data
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
        "Use the options below to filter the NBA data."
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
            "Use the sidebar to filter the dataset by categorical and numeric
            variables. After choosing the desired filters, press the Apply
            Filters button to update the data used throughout the application."
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
  
}


# Run Application
shinyApp(ui = ui, server = server)