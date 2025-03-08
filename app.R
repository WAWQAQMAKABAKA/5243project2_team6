library(shiny)
library(DT)
library(tidyverse)
library(shinythemes)
library(corrplot)
library(palmerpenguins)
library(data.table)

#max file upload size to 1GB
options(shiny.maxRequestSize = 1024 * 1024^2)  # 1024MB = 1GB

ui <- navbarPage(
  title = "Data Analysis Suite",
  id = "nav",
  theme = shinythemes::shinytheme("flatly"),
  
  # Welcome Page
  # could make this more dynamic, but here is what i have so far
  # briefly explain the app
  tabPanel("Welcome",
           div(class = "jumbotron",
               h1("Data Analysis Suite", class = "display-4"),
               p("DESCRIPTION PLACE HOLDER", class = "lead"),
               hr(class = "my-4"),
               h3("Key Features:"),
               tags$ul(
                 tags$li("Fast data loading (up to 1GB)"),
                 tags$li("Preprocessing & cleaning tools"),
                 tags$li("Dynamic visualizations"),
                 tags$li("Multiple file format support")
               ),
               h3("Contributors:"),
               p("Ziyue Gao, Keito Taketomi, Anqi Wu, Yixin Xiao."),
               h3("License:"),
               p("MIT License | © 2024 Data Analytics Inc.")
           )
  ),
  
  # data upload
  # support multiple format
  # have built in dataset
  
  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source Selection"),
               radioButtons("data_source", "Choose Input Method:",
                            choices = c("Upload File" = "upload",
                                        "Use Example Dataset" = "example"),
                            selected = "upload"),
               
               conditionalPanel(
                 condition = "input.data_source == 'upload'",
                 fileInput("file_upload", "Choose File:",
                           accept = c(".csv", ".xls", ".xlsx", ".json", ".rds")),
                 helpText("Max file size: 1GB")
               ),
               
               conditionalPanel(
                 condition = "input.data_source == 'example'",
                 selectInput("example_data", "Select Example Dataset:",
                             choices = c("Iris" = "iris",
                                         "Motor Trends Cars" = "mtcars",
                                         "Penguins" = "penguins"))
               )
             ),
             
             mainPanel(
               h3("Data Preview"),
               DTOutput("data_preview"),
               uiOutput("dataset_info")
             )
           )
  ),
  
  # data preprocessing
  tabPanel("Preprocessing",
           sidebarLayout(
             sidebarPanel(
               h4("Data Cleaning Tools"),
               checkboxGroupInput("preprocess_steps", "Select Operations:",
                                  choices = c("Handle Missing Values" = "na",
                                              "Normalize Features" = "normalize")),
               
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('na')",
                 selectInput("na_handling", "Missing Value Strategy:",
                             choices = c("Remove Rows" = "rm_rows",
                                         "Mean Imputation" = "mean",
                                         "Median Imputation" = "median"))
               )
             ),
             
             mainPanel(
               h3("Processed Data Preview"),
               DTOutput("processed_data")
             )
           )
  ),
  
  # EDA
  tabPanel("Visualization",
           sidebarLayout(
             sidebarPanel(
               selectInput("plot_type", "Choose Visualization Type:",
                           choices = c("Scatter Plot" = "scatter",
                                       "Histogram" = "hist",
                                       "Box Plot" = "box",
                                       "Correlation Matrix" = "corr")),
               uiOutput("plot_params"),
               sliderInput("plot_size", "Plot Size:",
                           min = 400, max = 1000, value = 600),
               downloadButton("download_plot", "Download Plot")
             ),
             
             mainPanel(
               h3("Data Visualization"),
               plotOutput("main_viz", height = "auto")
             )
           )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL
  )
  
  #######optimized for large files
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    file_path <- input$file_upload$datapath
    file_ext <- tools::file_ext(input$file_upload$name)
    
    rv$raw_data <- switch(file_ext,
                          "csv" = fread(file_path), #effcient reading specially for csv file
                          "xls" = readxl::read_excel(file_path),
                          "xlsx" = readxl::read_excel(file_path),
                          "json" = jsonlite::fromJSON(file_path),
                          "rds" = readRDS(file_path),
                          NULL)
    
    if (is.null(rv$raw_data)) {
      showNotification("Unsupported file format!", type = "error")
    }
  })
  
  # load example datasets
  ##### need to change
  observeEvent(input$example_data, {
    rv$raw_data <- switch(input$example_data,
                          "iris" = iris,
                          "mtcars" = mtcars,
                          "penguins" = palmerpenguins::penguins)
  })
  
  # dataset info
  output$dataset_info <- renderUI({
    req(input$example_data)
    desc <- switch(input$example_data,
                   "iris" = "Measurements of 3 iris species (n = 150)",
                   "mtcars" = "32 cars' performance specs (1974)",
                   "penguins" = "Penguin measurements from Palmer Station (n = 344)")
    HTML(paste0("<div class='alert alert-info'><strong>Dataset Info:</strong> ", desc, "</div>"))
  })
  
  # data preview
  output$data_preview <- renderDT({
    req(rv$raw_data)
    datatable(rv$raw_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # preprocessing
  observe({
    req(rv$raw_data)
    data <- rv$raw_data
    
    # handle missing values
    if ("na" %in% input$preprocess_steps) {
      if (input$na_handling == "rm_rows") {
        data <- na.omit(data)
      } else {
        data <- data %>%
          mutate(across(where(is.numeric), ~ ifelse(is.na(.),
                                                    if (input$na_handling == "mean") mean(., na.rm = TRUE) else median(., na.rm = TRUE),
                                                    .)))
      }
    }
    
    rv$processed_data <- data
  })
  
  output$processed_data <- renderDT({
    req(rv$processed_data)
    datatable(rv$processed_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # visualization UI
  output$plot_params <- renderUI({
    req(rv$processed_data)
    vars <- names(rv$processed_data)
    
    switch(input$plot_type,
           "scatter" = tagList(
             selectInput("x_var", "X Variable:", vars),
             selectInput("y_var", "Y Variable:", vars)
           ),
           "hist" = selectInput("hist_var", "Variable:", vars),
           "box" = selectInput("box_var", "Variable:", vars)
    )
  })
  
  # visualization render plot
  output$main_viz <- renderPlot({
    req(rv$processed_data)
    data <- rv$processed_data
    
    if (input$plot_type == "scatter") {
      req(input$x_var, input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() + theme_minimal()
      
    } else if (input$plot_type == "hist") {
      req(input$hist_var)
      ggplot(data, aes_string(x = input$hist_var)) +
        geom_histogram(fill = "skyblue", bins = 30) + theme_minimal()
    }
  }, height = function() { input$plot_size })
}

shinyApp(ui, server)
