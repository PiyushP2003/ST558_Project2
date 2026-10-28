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


numeric_choices <- c(
  "Points" = "pts",
  "Rebounds" = "reb",
  "Assists" = "ast",
  "Minutes" = "minutes",
  "Field Goals Made" = "fgm",
  "Field Goal Attempts" = "fga",
  "Three-Pointers Made" = "fg3m",
  "Three-Point Attempts" = "fg3a",
  "Free Throws Made" = "ftm",
  "Free Throw Attempts" = "fta",
  "Offensive Rebounds" = "oreb",
  "Defensive Rebounds" = "dreb",
  "Steals" = "stl",
  "Blocks" = "blk",
  "Turnovers" = "tov",
  "Personal Fouls" = "pf",
  "Plus/Minus" = "plus_minus"
)


categorical_choices <- c(
  "Team" = "team",
  "Opponent" = "opponent",
  "Game Location" = "location"
)


get_label <- function(variable) {
  
  all_choices <- c(
    numeric_choices,
    categorical_choices
  )
  
  names(all_choices)[
    match(variable, all_choices)
  ]
}


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
        choices = numeric_choices,
        selected = "pts"
      ),
      
      uiOutput("numeric_slider1"),
      
      selectInput(
        "numeric_var2",
        "Numeric Variable 2:",
        choices = numeric_choices,
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
            "Use the sidebar to subset the NBA data. The Data Download tab
            displays and downloads the filtered observations. The Data
            Exploration tab allows you to choose variables for numerical,
            categorical, and graphical summaries."
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
              "Provides customizable numerical summaries, contingency tables,
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
            "Choose variables below to explore the currently filtered data."
          ),
          
          tabsetPanel(
            
            # Numerical Summaries
            tabPanel(
              "Numerical Summaries",
              
              br(),
              
              selectInput(
                "summary_numeric",
                "Numeric Variable:",
                choices = numeric_choices,
                selected = "pts"
              ),
              
              selectInput(
                "summary_group",
                "Summarize Across:",
                choices = categorical_choices,
                selected = "team"
              ),
              
              h4("Summary Statistics"),
              
              DT::dataTableOutput("numerical_summary")
            ),
            
            
            # Categorical Summaries
            tabPanel(
              "Categorical Summaries",
              
              br(),
              
              selectInput(
                "one_way_var",
                "One-Way Table Variable:",
                choices = categorical_choices,
                selected = "team"
              ),
              
              h4("One-Way Contingency Table"),
              
              DT::dataTableOutput("one_way_table"),
              
              br(),
              
              selectInput(
                "two_way_var1",
                "Two-Way Table Variable 1:",
                choices = categorical_choices,
                selected = "team"
              ),
              
              selectInput(
                "two_way_var2",
                "Two-Way Table Variable 2:",
                choices = categorical_choices,
                selected = "location"
              ),
              
              h4("Two-Way Contingency Table"),
              
              DT::dataTableOutput("two_way_table")
            ),
            
            
            # Graphical Summaries
            tabPanel(
              "Graphical Summaries",
              
              br(),
              
              selectInput(
                "plot_numeric1",
                "Primary Numeric Variable:",
                choices = numeric_choices,
                selected = "pts"
              ),
              
              selectInput(
                "plot_numeric2",
                "Second Numeric Variable:",
                choices = numeric_choices,
                selected = "ast"
              ),
              
              selectInput(
                "plot_group",
                "Color / Group Variable:",
                choices = categorical_choices,
                selected = "location"
              ),
              
              selectInput(
                "plot_facet",
                "Facet Variable:",
                choices = categorical_choices,
                selected = "team"
              ),
              
              selectInput(
                "plot_average_group",
                "Average Across:",
                choices = categorical_choices,
                selected = "team"
              ),
              
              hr(),
              
              h4("Histogram"),
              plotOutput("histogram_plot"),
              
              br(),
              
              h4("Box Plot"),
              plotOutput("boxplot_plot"),
              
              br(),
              
              h4("Scatter Plot"),
              plotOutput("scatter_plot"),
              
              br(),
              
              h4("Faceted Histogram"),
              plotOutput("facet_plot"),
              
              br(),
              
              h4("Average by Category"),
              plotOutput("average_plot"),
              
              br(),
              
              h4("Category Comparison"),
              plotOutput("bar_plot"),
              
              br(),
              
              h4("Heatmap"),
              plotOutput("heatmap_plot")
            )
          )
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
        .data[[input$numeric_var1]] <= input$numeric_range1[2],
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
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    data |>
      group_by(.data[[input$summary_group]]) |>
      summarize(
        Count = sum(!is.na(.data[[input$summary_numeric]])),
        Mean = mean(
          .data[[input$summary_numeric]],
          na.rm = TRUE
        ),
        Median = median(
          .data[[input$summary_numeric]],
          na.rm = TRUE
        ),
        SD = sd(
          .data[[input$summary_numeric]],
          na.rm = TRUE
        ),
        Min = min(
          .data[[input$summary_numeric]],
          na.rm = TRUE
        ),
        Max = max(
          .data[[input$summary_numeric]],
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      )
    
  }, options = list(
    pageLength = 10
  ), rownames = FALSE)
  
  
  # One-way contingency table
  output$one_way_table <- DT::renderDataTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    data |>
      count(
        .data[[input$one_way_var]],
        name = "Count"
      ) |>
      arrange(desc(Count))
    
  }, options = list(
    pageLength = 10
  ), rownames = FALSE)
  
  
  # Two-way contingency table
  output$two_way_table <- DT::renderDataTable({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      ),
      need(
        input$two_way_var1 != input$two_way_var2,
        "Choose two different categorical variables."
      )
    )
    
    data |>
      count(
        .data[[input$two_way_var1]],
        .data[[input$two_way_var2]]
      ) |>
      pivot_wider(
        names_from = all_of(input$two_way_var2),
        values_from = n,
        values_fill = 0
      )
    
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ), rownames = FALSE)
  
  
  # Histogram
  output$histogram_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    numeric1_label <- get_label(input$plot_numeric1)
    
    ggplot(
      data,
      aes(
        x = .data[[input$plot_numeric1]]
      )
    ) +
      geom_histogram(
        bins = 30,
        color = "white"
      ) +
      labs(
        title = paste(
          "Distribution of",
          numeric1_label
        ),
        x = numeric1_label,
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  
  # Box plot
  output$boxplot_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    numeric1_label <- get_label(input$plot_numeric1)
    group_label <- get_label(input$plot_group)
    
    ggplot(
      data,
      aes(
        x = .data[[input$plot_group]],
        y = .data[[input$plot_numeric1]],
        fill = .data[[input$plot_group]]
      )
    ) +
      geom_boxplot() +
      labs(
        title = paste(
          numeric1_label,
          "by",
          group_label
        ),
        x = group_label,
        y = numeric1_label,
        fill = group_label
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      ),
      need(
        input$plot_numeric1 != input$plot_numeric2,
        "Choose two different numeric variables for the scatter plot."
      )
    )
    
    numeric1_label <- get_label(input$plot_numeric1)
    numeric2_label <- get_label(input$plot_numeric2)
    group_label <- get_label(input$plot_group)
    
    ggplot(
      data,
      aes(
        x = .data[[input$plot_numeric1]],
        y = .data[[input$plot_numeric2]],
        color = .data[[input$plot_group]]
      )
    ) +
      geom_point(alpha = 0.5) +
      labs(
        title = paste(
          numeric1_label,
          "vs",
          numeric2_label
        ),
        x = numeric1_label,
        y = numeric2_label,
        color = group_label
      ) +
      theme_minimal()
  })
  
  
  # Faceted histogram
  output$facet_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    numeric1_label <- get_label(input$plot_numeric1)
    facet_label <- get_label(input$plot_facet)
    
    ggplot(
      data,
      aes(
        x = .data[[input$plot_numeric1]]
      )
    ) +
      geom_histogram(
        bins = 30,
        color = "white"
      ) +
      facet_wrap(
        vars(
          .data[[input$plot_facet]]
        )
      ) +
      labs(
        title = paste(
          "Distribution of",
          numeric1_label,
          "by",
          facet_label
        ),
        x = numeric1_label,
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  
  # Average by category
  output$average_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    numeric1_label <- get_label(input$plot_numeric1)
    average_group_label <- get_label(input$plot_average_group)
    
    plot_data <- data |>
      group_by(
        .data[[input$plot_average_group]]
      ) |>
      summarize(
        average = mean(
          .data[[input$plot_numeric1]],
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    ggplot(
      plot_data,
      aes(
        x = reorder(
          .data[[input$plot_average_group]],
          average
        ),
        y = average
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste(
          "Average",
          numeric1_label,
          "by",
          average_group_label
        ),
        x = average_group_label,
        y = paste(
          "Average",
          numeric1_label
        )
      ) +
      theme_minimal()
  })
  
  
  # Side-by-side bar chart
  output$bar_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      ),
      need(
        input$plot_group != input$plot_facet,
        "Choose different variables for Color / Group and Facet."
      )
    )
    
    group_label <- get_label(input$plot_group)
    facet_label <- get_label(input$plot_facet)
    
    plot_data <- data |>
      count(
        .data[[input$plot_group]],
        .data[[input$plot_facet]]
      )
    
    ggplot(
      plot_data,
      aes(
        x = .data[[input$plot_group]],
        y = n,
        fill = .data[[input$plot_facet]]
      )
    ) +
      geom_col(
        position = "dodge"
      ) +
      labs(
        title = paste(
          group_label,
          "by",
          facet_label
        ),
        x = group_label,
        y = "Count",
        fill = facet_label
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  
  # Heatmap
  output$heatmap_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations match the current filters."
      )
    )
    
    numeric1_label <- get_label(input$plot_numeric1)
    
    heatmap_data <- data |>
      group_by(
        team,
        opponent
      ) |>
      summarize(
        average = mean(
          .data[[input$plot_numeric1]],
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    ggplot(
      heatmap_data,
      aes(
        x = opponent,
        y = team,
        fill = average
      )
    ) +
      geom_tile() +
      labs(
        title = paste(
          "Average",
          numeric1_label,
          "by Team and Opponent"
        ),
        x = "Opponent",
        y = "Team",
        fill = paste(
          "Average",
          numeric1_label
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
}


# Run Application
shinyApp(ui = ui, server = server)