library(shiny)        # builds the Shiny app
library(vroom)        # reads data files quickly
library(tidyverse)    # data wrangling and plotting
library(rsconnect)    # deploys app to shinyapps.io

# Data -------------------------------------------------------------------

dir.create("neiss", showWarnings = FALSE)

download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom("neiss/injuries.tsv.gz", show_col_types = FALSE)
products <- vroom("neiss/products.tsv", show_col_types = FALSE)
population <- vroom("neiss/population.tsv", show_col_types = FALSE)

# Helpers ----------------------------------------------------------------

prod_codes <- setNames(products$prod_code, products$title)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq(as.factor({{ var }})), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)), .groups = "drop") %>%
    arrange(desc(n))
}

# UI ---------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f7f7;
        font-family: Arial, sans-serif;
      }
      
      .app-title {
        font-size: 34px;
        font-weight: 700;
        margin-top: 20px;
      }
      
      .subtitle {
        color: #555;
        margin-bottom: 20px;
      }
      
      .card {
        background: white;
        border: 1px solid #ddd;
        border-radius: 10px;
        padding: 18px;
        margin-bottom: 15px;
        min-height: 110px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      }
      
      .card h4 {
        margin-top: 0;
        font-weight: 700;
      }
      
      .sidebar-card {
        background: white;
        border: 1px solid #ddd;
        border-radius: 10px;
        padding: 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      }
    "))
  ),
  
  div(class = "app-title", "NEISS Injury Explorer"),
  div(class = "subtitle", "Pick a product and explore national injury patterns using NEISS emergency room data."),
  
  sidebarLayout(
    
    sidebarPanel(
      div(
        class = "sidebar-card",
        
        selectInput(
          "code",
          "Choose a product:",
          choices = prod_codes,
          selected = 649
        ),
        
        selectInput(
          "y",
          "Plot y-axis:",
          choices = c(
            "Estimated injury count" = "count",
            "Injury rate per 10,000 people" = "rate"
          )
        ),
        
        checkboxGroupInput(
          "sex_filter",
          "Select sex:",
          choices = sort(unique(injuries$sex)),
          selected = sort(unique(injuries$sex))
        ),
        
        sliderInput(
          "age_filter",
          "Select age range:",
          min = min(injuries$age, na.rm = TRUE),
          max = max(injuries$age, na.rm = TRUE),
          value = c(
            min(injuries$age, na.rm = TRUE),
            max(injuries$age, na.rm = TRUE)
          )
        )
      )
    ),
    
    mainPanel(
      
      fluidRow(
        column(4, div(class = "card", h4("Total Estimated Injuries"), textOutput("summary_text"))),
        column(4, div(class = "card", h4("Most Common Diagnosis"), textOutput("top_diag"))),
        column(4, div(class = "card", h4("Most Injured Body Part"), textOutput("top_body")))
      ),
      
      hr(),
      
      tabsetPanel(
        
        tabPanel(
          "Tables",
          br(),
          fluidRow(
            column(4, h4("Top diagnoses"), tableOutput("diag")),
            column(4, h4("Top body parts"), tableOutput("body_part")),
            column(4, h4("Top locations"), tableOutput("location"))
          )
        ),
        
        tabPanel(
          "Age & Sex Trends",
          br(),
          plotOutput("age_sex", height = "450px")
        ),
        
        tabPanel(
          "Diagnosis Breakdown",
          br(),
          plotOutput("diag_plot", height = "450px")
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  selected <- reactive({
    injuries %>%
      filter(
        prod_code == as.numeric(input$code),
        sex %in% input$sex_filter,
        age >= input$age_filter[1],
        age <= input$age_filter[2]
      )
  })
  
  output$summary_text <- renderText({
    total_injuries <- selected() %>%
      summarise(total = as.integer(sum(weight))) %>%
      pull(total)
    
    paste(format(total_injuries, big.mark = ","), "injuries")
  })
  
  output$top_diag <- renderText({
    top_value <- selected() %>%
      count(diag, wt = weight, sort = TRUE) %>%
      slice(1) %>%
      pull(diag)
    
    ifelse(length(top_value) == 0, "No data", top_value)
  })
  
  output$top_body <- renderText({
    top_value <- selected() %>%
      count(body_part, wt = weight, sort = TRUE) %>%
      slice(1) %>%
      pull(body_part)
    
    ifelse(length(top_value) == 0, "No data", top_value)
  })
  
  output$diag <- renderTable({
    count_top(selected(), diag)
  })
  
  output$body_part <- renderTable({
    count_top(selected(), body_part)
  })
  
  output$location <- renderTable({
    count_top(selected(), location)
  })
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      ggplot(summary(), aes(age, n, colour = sex)) +
        geom_line(linewidth = 1.2) +
        labs(x = "Age", y = "Estimated injuries", colour = "Sex") +
        theme_minimal(base_size = 14)
    } else {
      ggplot(summary(), aes(age, rate, colour = sex)) +
        geom_line(linewidth = 1.2, na.rm = TRUE) +
        labs(x = "Age", y = "Injuries per 10,000", colour = "Sex") +
        theme_minimal(base_size = 14)
    }
  })
  
  output$diag_plot <- renderPlot({
    diag_data <- selected() %>%
      count(diag, wt = weight, sort = TRUE) %>%
      slice_head(n = 10)
    
    ggplot(diag_data, aes(x = reorder(diag, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Diagnosis", y = "Estimated injuries") +
      theme_minimal(base_size = 14)
  })
}

# Run app ----------------------------------------------------------------

shinyApp(ui, server)