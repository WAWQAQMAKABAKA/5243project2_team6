library(shiny)
library(DT)
library(tidyverse)
library(shinythemes)
library(corrplot)
library(palmerpenguins)
library(data.table)
library(readxl)
library(jsonlite)

# Max upload size to 1GB
options(shiny.maxRequestSize = 1024 * 1024^2)

ui <- navbarPage(
  title = "Data Analysis Suite",
  id = "nav",
  theme = shinythemes::shinytheme("flatly"),

  tabPanel("Welcome",
    div(class = "jumbotron",
        h1("Data Analysis Suite", class = "display-4"),
        p("This app provides an interactive data analysis pipeline including uploading, cleaning, visualization, and feature engineering.", class = "lead"),
        hr(),
        h3("Key Features:"),
        tags$ul(
          tags$li("Support for multiple file formats (CSV, Excel, JSON, RDS)"),
          tags$li("Data preprocessing and cleaning tools"),
          tags$li("Dynamic data visualizations including correlation matrix"),
          tags$li("Feature engineering tools for modeling"),
          tags$li("Downloadable plots")
        ),
        h3("Contributors:"),
        p("Ziyue Gao, Keito Taketomi, Anqi Wu, Yixin Xiao")
    )
  ),

  tabPanel("Data Upload",
    sidebarLayout(
      sidebarPanel(
        radioButtons("data_source", "Choose Data Source:",
                     choices = c("Upload File" = "upload", "Use Example Dataset" = "example")),
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          fileInput("file_upload", "Choose File:",
                    accept = c(".csv", ".xls", ".xlsx", ".json", ".rds")),
          helpText("Max file size: 1GB")
        ),
        conditionalPanel(
          condition = "input.data_source == 'example'",
          selectInput("example_data", "Select Example Dataset:",
                      choices = c("Iris" = "iris", "Motor Trends Cars" = "mtcars", "Penguins" = "penguins"))
        )
      ),
      mainPanel(
        h3("Data Preview"),
        DTOutput("data_preview"),
        uiOutput("dataset_info")
      )
    )
  ),

  tabPanel("Preprocessing",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("preprocess_steps", "Select Preprocessing Steps:",
                           choices = c("Handle Missing Values" = "na",
                                       "Normalize Numeric Features" = "normalize")),
        conditionalPanel(
          condition = "input.preprocess_steps.includes('na')",
          selectInput("na_handling", "Missing Value Strategy:",
                      choices = c("Remove Rows" = "rm_rows", "Mean Imputation" = "mean", "Median Imputation" = "median"))
        )
      ),
      mainPanel(
        h3("Processed Data Preview"),
        DTOutput("processed_data")
      )
    )
  ),

  tabPanel("Feature Engineering",
    sidebarLayout(
      sidebarPanel(
        h4("Add New Features"),
        checkboxGroupInput("feature_steps", "Select Feature Engineering Steps:",
                           choices = c("Add Text Length Feature" = "textlen",
                                       "Create Upvote/Comment Ratio" = "ratio"))
      ),
      mainPanel(
        h3("Feature Engineered Data"),
        DTOutput("featured_data")
      )
    )
  ),

  tabPanel("Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("plot_type", "Plot Type:",
                    choices = c("Scatter Plot" = "scatter",
                                "Histogram" = "hist",
                                "Box Plot" = "box",
                                "Correlation Matrix" = "corr")),
        uiOutput("plot_params"),
        sliderInput("plot_size", "Plot Height (px):", min = 300, max = 1000, value = 500),
        downloadButton("download_plot", "Download Plot")
      ),
      mainPanel(
        h3("Visualization Output"),
        plotOutput("main_plot", height = "auto")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(raw_data = NULL, processed_data = NULL, featured_data = NULL)

  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    path <- input$file_upload$datapath
    rv$raw_data <- switch(ext,
                          csv = fread(path),
                          xls = read_excel(path),
                          xlsx = read_excel(path),
                          json = fromJSON(path),
                          rds = readRDS(path),
                          NULL)
  })

  observeEvent(input$example_data, {
    rv$raw_data <- switch(input$example_data,
                          iris = iris,
                          mtcars = mtcars,
                          penguins = palmerpenguins::penguins)
  })

  output$data_preview <- renderDT({ req(rv$raw_data); datatable(rv$raw_data) })

  output$dataset_info <- renderUI({
    req(input$example_data)
    info <- switch(input$example_data,
                   iris = "Iris dataset: 150 flowers with 4 measurements",
                   mtcars = "Motor Trend Car Road Tests dataset",
                   penguins = "Palmer Station Penguin data")
    HTML(paste("<strong>Dataset Info:</strong>", info))
  })

  observe({
    req(rv$raw_data)
    df <- rv$raw_data

    if ("na" %in% input$preprocess_steps) {
      if (input$na_handling == "rm_rows") {
        df <- na.omit(df)
      } else {
        df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.),
                                                              if (input$na_handling == "mean") mean(., na.rm = TRUE) else median(., na.rm = TRUE), .)))
      }
    }
    if ("normalize" %in% input$preprocess_steps) {
      df <- df %>% mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))
    }

    rv$processed_data <- df
  })

  output$processed_data <- renderDT({ req(rv$processed_data); datatable(rv$processed_data) })

  observe({
    req(rv$processed_data)
    df <- rv$processed_data

    if (is.null(df)) return(NULL)
    if ("textlen" %in% input$feature_steps && "name" %in% colnames(df)) {
      df$text_length <- nchar(as.character(df$name))
    }
    if ("ratio" %in% input$feature_steps && all(c("upvotes", "comments") %in% colnames(df))) {
      df$upvote_comment_ratio <- df$upvotes / (df$comments + 1)
    }

    rv$featured_data <- df
  })

  output$featured_data <- renderDT({ req(rv$featured_data); datatable(rv$featured_data) })

  output$plot_params <- renderUI({
    req(rv$processed_data)
    vars <- names(rv$processed_data)
    switch(input$plot_type,
           scatter = tagList(selectInput("xvar", "X Variable", vars), selectInput("yvar", "Y Variable", vars)),
           hist = selectInput("histvar", "Variable", vars),
           box = selectInput("boxvar", "Variable", vars),
           NULL)
  })

  output$main_plot <- renderPlot({
    req(rv$processed_data)
    df <- rv$processed_data
    if (input$plot_type == "scatter") {
      ggplot(df, aes_string(x = input$xvar, y = input$yvar)) + geom_point() + theme_minimal()
    } else if (input$plot_type == "hist") {
      ggplot(df, aes_string(x = input$histvar)) + geom_histogram(bins = 30, fill = "steelblue") + theme_minimal()
    } else if (input$plot_type == "box") {
      ggplot(df, aes_string(y = input$boxvar)) + geom_boxplot(fill = "orange") + theme_minimal()
    } else if (input$plot_type == "corr") {
      num_df <- df %>% select(where(is.numeric))
      cmat <- cor(num_df, use = "complete.obs")
      corrplot(cmat, method = "circle")
    }
  }, height = function() input$plot_size)

  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 8, height = 6)
    }
  )
}

shinyApp(ui, server)

