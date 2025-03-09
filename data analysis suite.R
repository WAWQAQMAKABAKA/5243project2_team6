library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(data.table)
library(readxl)
library(jsonlite)
library(corrplot)
library(palmerpenguins)

# Set maximum file upload size to 1GB
options(shiny.maxRequestSize = 1024 * 1024^2)  # 1024MB = 1GB

ui <- navbarPage(
  title = "Data Analysis Suite",
  id = "nav",
  theme = shinythemes::shinytheme("flatly"),
  
  # 1) Welcome / Guide
  tabPanel("Welcome",
           fluidPage(
             h1("Data Analysis Suite", class = "display-4"),
             p("Welcome to the enhanced version of the Data Analysis Suite. 
               This application allows you to upload and explore datasets, 
               preprocess and clean them (including outlier handling), 
               engineer new features, and perform interactive data analysis 
               (with filtering and advanced visualizations).", class = "lead"),
             hr(),
             h3("How to Use:"),
             tags$ol(
               tags$li("Go to 'Data Upload' to either upload your own file or select an example dataset."),
               tags$li("Use 'Preprocessing' to handle missing data, duplicates, categorical encoding, outliers, and scaling."),
               tags$li("In 'Feature Engineering', create new columns based on arithmetic operations. These will re-calculate whenever preprocessing changes."),
               tags$li("Finally, explore 'Visualization & EDA' to filter data, view summary stats, generate plots, and download them.")
             ),
             h3("Contributors:"),
             p("Ziyue Gao, Keito Taketomi, Anqi Wu, Yixin Xiao."),
             h3("License:"),
             p("MIT License | © 2025 Data Analytics Inc.")
           )
  ),
  
  # 2) Data Upload Tab
  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               h4("Data Source"),
               radioButtons("data_source", "Choose Input Method:",
                            choices = c("Upload File" = "upload",
                                        "Use Example Dataset" = "example"),
                            selected = "upload"),
               conditionalPanel(
                 condition = "input.data_source == 'upload'",
                 fileInput("file_upload", "Choose File:",
                           accept = c(".csv", ".xls", ".xlsx", ".json", ".rds")),
                 helpText("Supported: CSV, Excel (.xls/.xlsx), JSON, RDS. Max 1GB.")
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
               h3("Raw Data Preview"),
               DTOutput("data_preview"),
               uiOutput("dataset_info")
             )
           )
  ),
  
  # 3) Preprocessing Tab
  tabPanel("Preprocessing",
           sidebarLayout(
             sidebarPanel(
               h4("Data Cleaning & Transformation"),
               checkboxGroupInput("preprocess_steps", "Select Operations:",
                                  choices = c("Handle Missing Values" = "na",
                                              "Remove Duplicates" = "duplicates",
                                              "Encode Categoricals" = "encode",
                                              "Handle Outliers" = "outliers",
                                              "Normalize Features" = "normalize")),
               
               # Missing value strategy
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('na')",
                 selectInput("na_handling", "Missing Value Strategy:",
                             choices = c("Remove Rows" = "rm_rows",
                                         "Mean Imputation" = "mean",
                                         "Median Imputation" = "median"))
               ),
               
               # Outlier handling
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('outliers')",
                 selectInput("outlier_method", "Outlier Method:",
                             choices = c("Remove if beyond threshold" = "remove",
                                         "Winsorize (cap values)" = "winsor")),
                 numericInput("outlier_threshold", "Threshold (SD from mean):", 
                              value = 3, min = 1, step = 0.5)
               )
             ),
             
             mainPanel(
               h3("Processed Data Preview"),
               DTOutput("processed_data")
             )
           )
  ),
  
  # 4) Feature Engineering Tab
  tabPanel("Feature Engineering",
           sidebarLayout(
             sidebarPanel(
               h4("Create or Modify Features"),
               selectInput("fe_var1", "Select Variable 1:", choices = NULL),
               selectInput("fe_var2", "Select Variable 2:", choices = NULL),
               selectInput("fe_operation", "Operation:",
                           choices = c("Addition" = "add",
                                       "Subtraction" = "sub",
                                       "Multiplication" = "mul",
                                       "Division" = "div")),
               textInput("fe_newname", "New Feature Name:", value = "new_feature"),
               actionButton("create_feature", "Create Feature"),
               hr(),
               p("Use arithmetic operations on two numeric columns. If you adjust the 
                  preprocessing steps later, these new features will be recalculated 
                  automatically.")
             ),
             mainPanel(
               h3("Engineered Data Preview"),
               DTOutput("engineered_data")
             )
           )
  ),
  
  # 5) Visualization & EDA
  tabPanel("Visualization & EDA",
           sidebarLayout(
             sidebarPanel(
               h4("Data Filtering"),
               uiOutput("filter_ui"),   # Dynamic UI for filtering
               hr(),
               h4("Plots"),
               selectInput("plot_type", "Choose Plot Type:",
                           choices = c("Scatter Plot" = "scatter",
                                       "Histogram" = "hist",
                                       "Box Plot" = "box",
                                       "Correlation Matrix" = "corr")),
               uiOutput("plot_params"),
               sliderInput("plot_size", "Plot Height (pixels):",
                           min = 400, max = 1000, value = 600),
               downloadButton("download_plot", "Download Plot")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Visualization",
                          h3("Data Visualization"),
                          plotOutput("main_viz", height = "auto")
                 ),
                 tabPanel("Summary Stats",
                          h3("Summary Statistics (Numeric Columns Only)"),
                          DTOutput("summary_stats")
                 )
               )
             )
           )
  )
)

# SERVER LOGIC
server <- function(input, output, session) {
  
  # Reactive Values 
  rv <- reactiveValues(
    raw_data = NULL,           # Data as initially loaded
    feature_ops = data.frame(  # Stores user-defined feature operations
      var1 = character(),
      var2 = character(),
      op   = character(),
      newname = character(),
      stringsAsFactors = FALSE
    )
  )

  # 1) Data Upload
  
  # Monitor file uploads
  observeEvent(input$file_upload, {
    req(input$file_upload)
    file_path <- input$file_upload$datapath
    file_ext  <- tools::file_ext(input$file_upload$name)
    
    # Load data according to file extension
    data_loaded <- switch(tolower(file_ext),
                          "csv"  = fread(file_path),
                          "xls"  = read_excel(file_path),
                          "xlsx" = read_excel(file_path),
                          "json" = fromJSON(file_path),
                          "rds"  = readRDS(file_path),
                          NULL)
    if (is.null(data_loaded)) {
      showNotification("Unsupported file format!", type = "error")
    } else {
      rv$raw_data <- as.data.frame(data_loaded)
    }
  })
  
  # Monitor example dataset selection
  observeEvent(input$example_data, {
    if (input$data_source == "example") {
      rv$raw_data <- switch(input$example_data,
                            "iris"     = iris,
                            "mtcars"   = mtcars,
                            "penguins" = palmerpenguins::penguins)
    }
  })
  
  # Provide dataset info
  output$dataset_info <- renderUI({
    req(rv$raw_data)
    if (input$data_source == "example") {
      desc <- switch(input$example_data,
                     "iris"     = "Measurements of 3 iris species (n = 150)",
                     "mtcars"   = "32 cars' performance specs (1974)",
                     "penguins" = "Palmer Penguins dataset (n ~ 344)")
    } else {
      desc <- paste("Dataset loaded with", nrow(rv$raw_data), "rows and", 
                    ncol(rv$raw_data), "columns.")
    }
    HTML(paste0("<div class='alert alert-info'><strong>Dataset Info:</strong> ", desc, "</div>"))
  })
  
  # Show raw data preview
  output$data_preview <- renderDT({
    req(rv$raw_data)
    datatable(rv$raw_data, options = list(scrollX = TRUE, pageLength = 8))
  })
  
  # 2) Reactive: Preprocessed Data
  preprocessed_data <- reactive({
    req(rv$raw_data)
    data <- rv$raw_data
    
    # 1) Handle Missing Values
    if ("na" %in% input$preprocess_steps) {
      if (input$na_handling == "rm_rows") {
        data <- na.omit(data)
      } else {
        # Imputation on numeric columns
        data <- data %>%
          mutate(across(where(is.numeric), ~ ifelse(
            is.na(.),
            if (input$na_handling == "mean") mean(., na.rm = TRUE) else median(., na.rm = TRUE),
            .
          )))
      }
    }
    
    # 2) Remove Duplicates
    if ("duplicates" %in% input$preprocess_steps) {
      data <- distinct(data)
    }
    
    # 3) Encode Categoricals
    if ("encode" %in% input$preprocess_steps) {
      cat_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
      data[cat_cols] <- lapply(data[cat_cols], function(x) as.numeric(as.factor(x)))
    }
    
    # 4) Handle Outliers (numeric only)
    if ("outliers" %in% input$preprocess_steps) {
      req(input$outlier_method, input$outlier_threshold)
      num_cols <- names(data)[sapply(data, is.numeric)]
      
      if (length(num_cols) > 0) {
        for (col in num_cols) {
          col_data <- data[[col]]
          mean_val <- mean(col_data, na.rm = TRUE)
          sd_val   <- sd(col_data, na.rm = TRUE)
          upper    <- mean_val + input$outlier_threshold * sd_val
          lower    <- mean_val - input$outlier_threshold * sd_val
          
          if (input$outlier_method == "remove") {
            # Remove rows where col_data is beyond threshold
            keep_mask <- !((col_data < lower) | (col_data > upper))
            data <- data[keep_mask, ]
          } else if (input$outlier_method == "winsor") {
            # Winsorize (cap values at lower/upper)
            data[[col]] <- pmin(pmax(col_data, lower), upper)
          }
        }
      }
    }
    
    # 5) Normalize numeric features
    if ("normalize" %in% input$preprocess_steps) {
      num_cols <- sapply(data, is.numeric)
      data[num_cols] <- scale(data[num_cols])
    }
    
    data
  })
  
  # Display processed data
  output$processed_data <- renderDT({
    datatable(preprocessed_data(), options = list(scrollX = TRUE, pageLength = 8))
  })

  # 3) Reactive: Engineered Data
  engineered_data <- reactive({
    req(preprocessed_data())
    data <- preprocessed_data()
    
    # Apply each saved feature operation in rv$feature_ops:
    if (nrow(rv$feature_ops) > 0) {
      for (i in seq_len(nrow(rv$feature_ops))) {
        var1    <- rv$feature_ops$var1[i]
        var2    <- rv$feature_ops$var2[i]
        op      <- rv$feature_ops$op[i]
        newname <- rv$feature_ops$newname[i]
        
        # Validate columns still exist & are numeric
        if (! (var1 %in% names(data) && var2 %in% names(data))) {
          showNotification(paste("Cannot find", var1, "or", var2, 
                                 "in data. Skipping feature", newname), type = "error")
          next
        }
        if (!is.numeric(data[[var1]]) || !is.numeric(data[[var2]])) {
          showNotification(paste("Feature operation skipped: columns not numeric"),
                           type = "error")
          next
        }
        
        v1 <- data[[var1]]
        v2 <- data[[var2]]
        new_feat <- switch(op,
                           "add" = v1 + v2,
                           "sub" = v1 - v2,
                           "mul" = v1 * v2,
                           "div" = ifelse(v2 == 0, NA, v1 / v2))
        
        data[[newname]] <- new_feat
      }
    }
    
    data
  })
  
  # Display final engineered data
  output$engineered_data <- renderDT({
    datatable(engineered_data(), options = list(scrollX = TRUE, pageLength = 8))
  })
  
  # 4) Creating New Feature
  observeEvent(input$create_feature, {
    req(input$fe_var1, input$fe_var2, input$fe_newname)
    # Add a new row to rv$feature_ops
    new_row <- data.frame(
      var1    = input$fe_var1,
      var2    = input$fe_var2,
      op      = input$fe_operation,
      newname = input$fe_newname,
      stringsAsFactors = FALSE
    )
    rv$feature_ops <- rbind(rv$feature_ops, new_row)
    
    showNotification(paste("Feature", input$fe_newname, "added to pipeline."), type = "message")
  })
  
  # Update selectable variables for FE whenever preprocessed_data changes
  observe({
    req(preprocessed_data())
    vars <- names(preprocessed_data())
    updateSelectInput(session, "fe_var1", choices = vars)
    updateSelectInput(session, "fe_var2", choices = vars)
  })

  observeEvent(rv$raw_data, {
    if (!is.null(rv$raw_data)) {
      vars <- names(rv$raw_data)
      updateSelectInput(session, "fe_var1", choices = vars)
      updateSelectInput(session, "fe_var2", choices = vars)
    }
  })
  
  # 5) Visualization & EDA
  
  # (a) Dynamic Filter UI
  output$filter_ui <- renderUI({
    req(engineered_data())
    data <- engineered_data()
    # List numeric columns
    num_vars <- names(data)[sapply(data, is.numeric)]
    
    if (length(num_vars) == 0) {
      tagList(
        p("No numeric columns available for filtering."),
        checkboxInput("apply_filter", "Apply Filter", value = FALSE, disabled = TRUE)
      )
    } else {
      tagList(
        checkboxInput("apply_filter", "Apply Filter?", value = FALSE),
        conditionalPanel(
          condition = "input.apply_filter == true",
          selectInput("filter_var", "Select Numeric Column to Filter:",
                      choices = num_vars),
          uiOutput("filter_range_ui")
        )
      )
    }
  })
  
  # (b) Dynamic UI for the filter range, based on selected numeric column
  output$filter_range_ui <- renderUI({
    req(input$filter_var, input$apply_filter)
    if (!input$apply_filter) return(NULL)
    data <- engineered_data()
    
    the_col <- data[[input$filter_var]]
    min_val <- floor(min(the_col, na.rm = TRUE))
    max_val <- ceiling(max(the_col, na.rm = TRUE))
    
    sliderInput("filter_range", "Filter Range:",
                min = min_val, max = max_val,
                value = c(min_val, max_val))
  })
  
  # (c) Filtered Data (for EDA)
  filtered_data <- reactive({
    req(engineered_data())
    data <- engineered_data()
    
    if (input$apply_filter && !is.null(input$filter_var) && !is.null(input$filter_range)) {
      var_name <- input$filter_var
      rng <- input$filter_range
      data <- data[data[[var_name]] >= rng[1] & data[[var_name]] <= rng[2], ]
    }
    data
  })
  
  # (d) Dynamic UI for Plot Parameters
  output$plot_params <- renderUI({
    req(filtered_data()) # ensures data is available
    vars <- names(filtered_data())
    
    switch(input$plot_type,
           "scatter" = tagList(
             selectInput("x_var", "X Variable:", vars),
             selectInput("y_var", "Y Variable:", vars)
           ),
           "hist" = selectInput("hist_var", "Histogram Variable:", vars),
           "box"  = selectInput("box_var", "Box Plot Variable:", vars),
           "corr" = NULL
    )
  })
  
  # (e) Main Plot Render
  output$main_viz <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    if (input$plot_type == "scatter") {
      req(input$x_var, input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "blue", alpha = 0.6) +
        theme_minimal()
      
    } else if (input$plot_type == "hist") {
      req(input$hist_var)
      ggplot(data, aes_string(x = input$hist_var)) +
        geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
        theme_minimal()
      
    } else if (input$plot_type == "box") {
      req(input$box_var)
      ggplot(data, aes_string(y = input$box_var)) +
        geom_boxplot(fill = "lightgreen", alpha = 0.7) +
        theme_minimal()
      
    } else if (input$plot_type == "corr") {
      # Check numeric columns for correlation
      num_data <- data %>% select(where(is.numeric))
      if (ncol(num_data) < 2) {
        showNotification("Not enough numeric columns for correlation matrix!", 
                         type = "warning")
        return(NULL)
      }
      corr_mat <- cor(num_data, use = "complete.obs")
      corrplot(corr_mat, method = "color", tl.cex = 0.8)
    }
  }, height = function() { input$plot_size })
  
  # (f) Summary Statistics (numeric columns)
  output$summary_stats <- renderDT({
    req(filtered_data())
    data <- filtered_data()
    num_data <- data %>% select(where(is.numeric))
    if (ncol(num_data) == 0) {
      return(datatable(data.frame(Note = "No numeric columns to summarize.")))
    }
    
    # Basic summary: min, median, mean, max, sd, n_missing
    summ <- num_data %>%
      summarize(across(everything(),
                       list(
                         Min = ~min(., na.rm = TRUE),
                         Median = ~median(., na.rm = TRUE),
                         Mean = ~mean(., na.rm = TRUE),
                         Max = ~max(., na.rm = TRUE),
                         SD = ~sd(., na.rm = TRUE),
                         Missing = ~sum(is.na(.))
                       ), 
                       .names = "{.col}_{.fn}"))
    
    # Tidy into a more readable format
    summ_tidy <- summ %>%
      pivot_longer(cols = everything(), 
                   names_to = c("Variable","Metric"), 
                   names_sep = "_", 
                   values_to = "Value") %>%
      pivot_wider(names_from = "Metric", values_from = "Value")
    
    datatable(summ_tidy, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # 6) Download Plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot-", Sys.Date(), ".png")
    },
    content = function(file) {
      req(filtered_data())
      data <- filtered_data()
      png(file, width = 800, height = 600)
      
      # same logic as renderPlot
      if (input$plot_type == "scatter") {
        req(input$x_var, input$y_var)
        p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(color = "blue", alpha = 0.6) +
          theme_minimal()
        print(p)
        
      } else if (input$plot_type == "hist") {
        req(input$hist_var)
        p <- ggplot(data, aes_string(x = input$hist_var)) +
          geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
          theme_minimal()
        print(p)
        
      } else if (input$plot_type == "box") {
        req(input$box_var)
        p <- ggplot(data, aes_string(y = input$box_var)) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7) +
          theme_minimal()
        print(p)
        
      } else if (input$plot_type == "corr") {
        num_data <- data %>% select(where(is.numeric))
        if (ncol(num_data) >= 2) {
          corr_mat <- cor(num_data, use = "complete.obs")
          corrplot(corr_mat, method = "color", tl.cex = 0.8)
        } else {
          showNotification("Not enough numeric columns for correlation matrix!", type = "warning")
        }
      }
      
      dev.off()
    }
  )
}

shinyApp(ui, server)
