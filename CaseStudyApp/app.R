library(shiny)      
library(vroom)      
library(tidyverse) 
library(rsconnect)

# Data

dir.create("neiss", showWarnings = FALSE)  # make folder

download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"  # base URL
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)  # save file
}

download("injuries.tsv.gz")  # injuries
download("population.tsv")   # population
download("products.tsv")     # products

injuries <- vroom::vroom("neiss/injuries.tsv.gz", show_col_types = FALSE)  # read injuries
products <- vroom::vroom("neiss/products.tsv", show_col_types = FALSE)     # read products
population <- vroom::vroom("neiss/population.tsv", show_col_types = FALSE) # read population

# Data Exploration

selected <- injuries %>% filter(prod_code == 649)  # filter product

nrow(selected)  # row count

selected %>% count(location, wt = weight, sort = TRUE)     # by location
selected %>% count(body_part, wt = weight, sort = TRUE)    # by body part
selected %>% count(diag, wt = weight, sort = TRUE)         # by diagnosis

summary <- selected %>% count(age, sex, wt = weight)  # age-sex counts

summary %>%
  ggplot(aes(age, n, colour = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")  # plot counts

summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4)  # add rate

summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")  # plot rate

# Helpers

prod_codes <- setNames(products$prod_code, products$title)  # menu options

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%  # top levels
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)), .groups = "drop") %>%  # sum weights
    arrange(desc(n))  # sort
}

# UI

ui <- fluidPage(
  titlePanel("NEISS Injury Explorer"),  # title
  
  p("Pick a product and explore injury patterns."),  # description
  
  fluidRow(
    column(
      8,
      selectInput("code", "Choose a product:", choices = prod_codes,
                  selected = 649, width = "100%")  # product menu
    ),
    column(
      4,
      selectInput("y", "Plot y-axis:",
                  choices = c("Estimated injury count" = "count",
                              "Injury rate per 10,000 people" = "rate"))  # y menu
    )
  ),
  
  fluidRow(
    column(12, strong(textOutput("summary_text")))  # summary text
  ),
  
  hr(),
  
  fluidRow(
    column(4, h4("Top diagnoses"), tableOutput("diag")),      # diag table
    column(4, h4("Top body parts"), tableOutput("body_part")),# body table
    column(4, h4("Top locations"), tableOutput("location"))   # location table
  ),
  
  hr(),
  
  fluidRow(
    column(12, h4("Injuries by age and sex"), plotOutput("age_sex"))  # plot
  )
)

# Server

server <- function(input, output, session) {
  
  selected <- reactive({
    injuries %>% filter(prod_code == as.numeric(input$code))  # filter reactive
  })
  
  output$summary_text <- renderText({
    total_injuries <- selected() %>%
      summarise(total = as.integer(sum(weight))) %>%
      pull(total)  # total injuries
    
    paste("Estimated total injuries for this product:", total_injuries)  # text
  })
  
  output$diag <- renderTable({
    count_top(selected(), diag)  # diag table
  })
  
  output$body_part <- renderTable({
    count_top(selected(), body_part)  # body table
  })
  
  output$location <- renderTable({
    count_top(selected(), location)  # location table
  })
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)  # summary data
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line(linewidth = 1) +
        labs(x = "Age", y = "Estimated injuries", colour = "Sex") +
        theme_minimal(base_size = 14)  # count plot
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(linewidth = 1, na.rm = TRUE) +
        labs(x = "Age", y = "Injuries per 10,000", colour = "Sex") +
        theme_minimal(base_size = 14)  # rate plot
    }
  })
}

shinyApp(ui, server)  # run app

