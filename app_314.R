library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(data.table)
library(readxl)
library(jsonlite)
library(corrplot)
library(palmerpenguins)
library(colourpicker)       
library(mgcv) 

# Set maximum file upload size to 1GB
options(shiny.maxRequestSize = 1024 * 1024^2)  # 1024MB = 1GB

ui <- navbarPage(
  title = "BlueStats",
  id = "nav",
  theme = shinythemes::shinytheme("cosmo"),
  tags$head(
    tags$style(HTML("
    body { font-family: 'Arial', sans-serif; background-color: #f8f9fa; }
    .navbar { background-color: #007BFF !important; color: white !important; }
    .nav-link { color: white !important; }
    h1 { color: #007BFF; }
    .sidebar { background-color: #f8f9fa; padding: 15px; border-radius: 10px; }
    .main-panel { background-color: white; padding: 20px; border-radius: 10px; box-shadow: 2px 2px 5px grey; }
    .btn-primary { background-color: #007BFF; border-color: #007BFF; }
    .btn-primary:hover { background-color: #0056b3; }
    .table { border-radius: 10px; overflow: hidden; }
  "))
  ),
  
  # 1) Welcome / Guide
  tabPanel("Welcome",
           fluidPage(
             h1("BlueStats", class = "display-4"),
             p("Welcome to our Columbia University project for GRSTAT5243. 
   This app lets you easily upload, explore, and analyze datasets. 
   Perform data cleaning, handle outliers, engineer features, and create 
   interactive visualizations—all in one place."),
             hr(),
             h3("How to Use:"),
             tags$ol(
               tags$li("Go to 'Data Upload' to either upload your own file or select an example dataset."),
               tags$li("Use 'Preprocessing' to handle missing data, duplicates, categorical encoding, outliers, and scaling."),
               tags$li("In 'Feature Engineering', create new columns based on arithmetic operations. These will re-calculate whenever preprocessing changes."),
               tags$li("Finally, explore 'Visualization & EDA' to filter data, view summary stats, generate plots, and download them.")
             ),
             h3("Project Information:"),
             p(strong("Course: "), "GRSTAT5243 - Applied Data Science"),
             p(strong("Institution: "), "Columbia University"),
             p(strong("Team: "), "Team 6"),
             hr(),
             h3("Contributors:"),
             p("Ziyue Gao, Keito Taketomi, Anqi Wu, Yixin Xiao."),
             h3("License:"),
             p("MIT License | © 2025 Columbia University GRSTAT5243 Project 2 - Team 6.")
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
                 helpText("Max 1GB. Supported: CSV, Excel, JSON, RDS")
               ),
               conditionalPanel(
                 condition = "input.data_source == 'example'",
                 selectInput("example_data", "Select Example Dataset:",
                             choices = c("Iris" = "iris",
                                         "mtcars" = "mtcars",
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
               div(style = "margin-bottom: 20px;",
                   checkboxGroupInput("preprocess_steps", "Select Operations:",
                                      choices = c("Handle Missing Values" = "na",
                                                  "Remove Duplicates" = "duplicates",
                                                  "Encode Categoricals" = "encode",
                                                  "Handle Outliers" = "outliers",
                                                  "Normalize Features" = "normalize"),
                                      width = "100%")
               ),
               
               # Missing Values Section
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('na')",
                 div(style = "border-left: 4px solid #007BFF; padding-left: 10px;",
                     h5("Missing Value Handling"),
                     radioButtons("na_handling", "Method:",
                                  choices = c("Remove Rows with NAs" = "rm_rows",
                                              "Impute with Mean" = "mean",
                                              "Impute with Median" = "median"),
                                  selected = "rm_rows"),
                     helpText(HTML(
                       "<b>What this does:</b>",
                       "<ul>",
                       "<li><b>Remove Rows:</b> Deletes any rows containing missing values</li>",
                       "<li><b>Impute Mean/Median:</b> Fills missing numeric values with column averages</li>",
                       "</ul>"
                     ))
                 )
               ),
               
               # Outlier Handling Section
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('outliers')",
                 div(style = "border-left: 4px solid #28a745; padding-left: 10px; margin-top: 15px;",
                     h5("Outlier Handling"),
                     radioButtons("outlier_method", "Method:",
                                  choices = c("Remove Outliers" = "remove",
                                              "Winsorize (Cap Values)" = "winsor"),
                                  selected = "remove"),
                     numericInput("outlier_threshold", "Standard Deviation Threshold:", 
                                  value = 3, min = 1, max = 5),
                     helpText(HTML(
                       "<b>How this works:</b>",
                       "<ul>",
                       "<li><b>Remove:</b> Deletes data points beyond ±X SDs from the mean</li>",
                       "<li><b>Winsorize:</b> Caps extreme values at the threshold boundaries</li>",
                       "<li>Typical threshold: 3 SDs covers 99.7% of normal distribution</li>",
                       "</ul>"
                     ))
                 )
               ),
               
               # Categorical Encoding Section
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('encode')",
                 div(style = "border-left: 4px solid #ffc107; padding-left: 10px; margin-top: 15px;",
                     h5("Categorical Encoding"),
                     helpText(HTML(
                       "<b>What this does:</b>",
                       "<ul>",
                       "<li>Converts text/factor columns to numeric codes</li>",
                       "<li>Example: 'Red', 'Blue', 'Green' becomes 1, 2, 3</li>",
                       "<li>Necessary for many machine learning algorithms</li>",
                       "</ul>"
                     ))
                 )
               ),
               
               # Normalization Section
               conditionalPanel(
                 condition = "input.preprocess_steps.includes('normalize')",
                 div(style = "border-left: 4px solid #dc3545; padding-left: 10px; margin-top: 15px;",
                     h5("Feature Scaling"),
                     helpText(HTML(
                       "<b>Why normalize:</b>",
                       "<ul>",
                       "<li>Scales all numeric features to similar ranges</li>",
                       "<li>Mean = 0, Standard Deviation = 1</li>",
                       "<li>Improves performance for distance-based algorithms</li>",
                       "</ul>"
                     ))
                 )
               )
             ),
             
             mainPanel(
               h3("Processed Data Preview"),
               DTOutput("processed_data"),
               hr(),
               h4("Processing Report"),
               uiOutput("preprocess_summary"),
               hr(),
               div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                   h5("What's Changed?"),
                   htmlOutput("change_explanation")
               )
             )
           )
  ),
  
  # 4) Feature Engineering Tab
  tabPanel("Feature Engineering",
           sidebarLayout(
             sidebarPanel(
               h4("Create Features"),
               selectInput("fe_var1", "Variable 1:", choices = NULL),
               selectInput("fe_var2", "Variable 2:", choices = NULL),
               selectInput("fe_operation", "Operation:",
                           choices = c("Add" = "add",
                                       "Subtract" = "sub",
                                       "Multiply" = "mul",
                                       "Divide" = "div")),
               textInput("fe_newname", "New Feature Name:", "new_feature"),
               actionButton("create_feature", "Create Feature")
             ),
             mainPanel(
               h3("Modified Data"),
               DTOutput("engineered_data")
             )
           )
  ),
  
  # 5) Visualization Tab
  tabPanel("Visualization & EDA",
           sidebarLayout(
             sidebarPanel(
               h4("Options"),
               uiOutput("filter_ui"),
               hr(),
               selectInput("plot_type", "Plot Type:",
                           choices = c("Scatter" = "scatter",
                                       "Histogram" = "hist",
                                       "Boxplot" = "box",
                                       "Correlation" = "corr")),
               # New visualization controls
               conditionalPanel(
                 condition = "input.plot_type == 'scatter'",
                 checkboxInput("add_jitter", "Add Jitter", value = FALSE),
                 selectInput("reg_type", "Add Regression Line:",
                             choices = c("None" = "none",
                                         "Linear" = "lm",
                                         "Spline" = "spline")),
                 colourInput("point_color", "Point Color:", value = "#007BFF")  # Now works
               ),
               
               conditionalPanel(
                 condition = "input.plot_type == 'hist'",
                 colourInput("hist_color", "Bar Color:", value = "#1E90FF")
               ),
               
               conditionalPanel(
                 condition = "input.plot_type == 'box'",
                 colourInput("box_color", "Box Color:", value = "#228B22")
               ),
               uiOutput("plot_params"),
               sliderInput("plot_size", "Plot Height:", 400, 1000, 600),
               downloadButton("download_plot", "Download Plot")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Visualization", plotOutput("main_viz")),
                 tabPanel("Statistics", 
                          h4("Summary Statistics"),
                          DTOutput("summary_stats"))
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
    datatable(rv$raw_data, 
              options = list(scrollX = TRUE, pageLength = 8),
              class = "table table-striped table-hover table-bordered")
  })
  
  # Reactive: Preprocessed Data
  preprocessed_data <- reactive({
    req(rv$raw_data)  # Ensure data is available before processing
    data <- rv$raw_data
    changes_log <- list()  # Log preprocessing steps
    
    # Check if preprocessing steps are selected
    if (is.null(input$preprocess_steps) || length(input$preprocess_steps) == 0) {
      return(data)  # Return original data if no preprocessing is selected
    }
    
    # 1) Handle Missing Values
    if ("na" %in% input$preprocess_steps) {
      req(input$na_handling)  # Ensure method is selected
      
      na_count <- sum(is.na(data))
      if (na_count > 0) {
        if (input$na_handling == "rm_rows") {
          data <- na.omit(data)
          changes_log <- append(changes_log, paste("Removed", na_count, "rows with NAs."))
        } else {
          # Impute only numeric columns
          num_cols <- names(data)[sapply(data, is.numeric)]
          if (length(num_cols) > 0) {
            data[num_cols] <- data[num_cols] %>%
              mutate(across(
                everything(),
                ~ ifelse(is.na(.),
                         if (input$na_handling == "mean") mean(., na.rm = TRUE)
                         else median(., na.rm = TRUE),
                         .)
              ))
            changes_log <- append(changes_log, 
                                  paste("Imputed NAs in", length(num_cols), "numeric columns using", 
                                        input$na_handling, "."))
          }
          
          # Handle categorical columns (convert to "Unknown")
          cat_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
          if (length(cat_cols) > 0) {
            data[cat_cols] <- data[cat_cols] %>%
              mutate(across(
                everything(),
                ~ replace_na(., "Unknown")
              ))
            changes_log <- append(changes_log, 
                                  paste("Converted NAs to 'Unknown' in", length(cat_cols), "categorical columns."))
          }
        }
      }
    }
    
    
    # 2) Remove Duplicates
    if ("duplicates" %in% input$preprocess_steps) {
      initial_rows <- nrow(data)
      data <- distinct(data)
      removed_dupes <- initial_rows - nrow(data)
      if (removed_dupes > 0) {
        changes_log <- append(changes_log, paste(removed_dupes, "duplicate rows removed."))
      }
    }
    
    # 3) Encode Categorical Variables
    if ("encode" %in% input$preprocess_steps) {
      cat_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
      if (length(cat_cols) > 0) {
        data[cat_cols] <- lapply(data[cat_cols], function(x) as.numeric(as.factor(x)))
        changes_log <- append(changes_log, paste(length(cat_cols), "categorical variables encoded as numeric."))
      }
    }
    
    # 4) Handle Outliers
    if ("outliers" %in% input$preprocess_steps) {
      req(input$outlier_method, input$outlier_threshold)
      
      num_cols <- names(data)[sapply(data, is.numeric)]
      outlier_count <- 0
      
      if (length(num_cols) > 0) {
        # Track outlier indices across all columns
        outlier_mask <- rep(FALSE, nrow(data))
        
        for (col in num_cols) {
          col_data <- data[[col]]
          mean_val <- mean(col_data, na.rm = TRUE)
          sd_val <- sd(col_data, na.rm = TRUE)
          upper <- mean_val + input$outlier_threshold * sd_val
          lower <- mean_val - input$outlier_threshold * sd_val
          
          # Identify outliers
          col_outliers <- col_data < lower | col_data > upper | is.na(col_data)
          outlier_mask <- outlier_mask | col_outliers
          
          if (input$outlier_method == "winsor") {
            # Winsorize (cap values)
            data[[col]] <- pmin(pmax(col_data, lower, na.rm = TRUE), upper, na.rm = TRUE)
            outlier_count <- outlier_count + sum(col_outliers, na.rm = TRUE)
          }
        }
        
        if (input$outlier_method == "remove") {
          # Remove all rows with outliers in ANY column
          data <- data[!outlier_mask, ]
          outlier_count <- sum(outlier_mask)
        }
        
        if (outlier_count > 0) {
          action <- ifelse(input$outlier_method == "remove", "removed", "winsorized")
          changes_log <- append(changes_log, 
                                paste(action, outlier_count, "outliers using", input$outlier_threshold, "SDs"))
        }
      }
    }
    
    # 5) Normalize numeric features
    if ("normalize" %in% input$preprocess_steps) {
      num_cols <- names(data)[sapply(data, is.numeric)]
      if (length(num_cols) > 0) {
        data[num_cols] <- scale(data[num_cols])
        changes_log <- append(changes_log, paste("All", length(num_cols), "numeric columns were normalized (mean = 0, SD = 1)."))
      }
    }
    
    # Store the log for later use
    rv$preprocessing_log <- changes_log
    
    return(data)
  })
  
  
  # Display processed data
  output$processed_data <- renderDT({
    datatable(preprocessed_data(), 
              options = list(scrollX = TRUE, pageLength = 8),
              class = "table table-striped table-hover table-bordered")
  })
  
  # Display preprocessing report
  output$preprocess_summary <- renderUI({
    req(rv$preprocessing_log)
    if (length(rv$preprocessing_log) > 0) {
      HTML(paste0("<ul><li>", paste(rv$preprocessing_log, collapse = "</li><li>"), "</li></ul>"))
    } else {
      HTML("<p>No preprocessing steps were applied.</p>")
    }
  })
  output$change_explanation <- renderUI({
    changes <- list()
    
    if ("na" %in% input$preprocess_steps) {
      changes$na <- switch(input$na_handling,
                           "rm_rows" = "Removed all rows with missing values",
                           "mean" = "Imputed missing numeric values with column means",
                           "median" = "Imputed missing numeric values with column medians")
    }
    
    if ("outliers" %in% input$preprocess_steps) {
      changes$outliers <- paste(
        switch(input$outlier_method,
               "remove" = "Removed outliers beyond",
               "winsor" = "Capped outliers at"),
        paste0(input$outlier_threshold, " SDs")
      )
    }
    
    if ("encode" %in% input$preprocess_steps) {
      changes$encode <- "Encoded categorical variables to numeric codes"
    }
    
    if ("normalize" %in% input$preprocess_steps) {
      changes$normalize <- "Normalized numeric features (mean = 0, SD = 1)"
    }
    
    HTML(paste(
      "<b>Current Modifications:</b>",
      "<ul>",
      paste0("<li>", unlist(changes), "</li>", collapse = ""),
      "</ul>"
    ))
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
  output$data_overview <- renderUI({
    req(filtered_data())  # Ensure data is available
    
    data <- filtered_data()
    col_info <- tibble(
      Column = names(data),
      Type = sapply(data, function(x) {
        if (is.numeric(x)) "Numeric"
        else if (is.factor(x) || is.character(x)) "Categorical"
        else "Other"
      })
    )
    
    # Convert to HTML
    html_output <- paste0(
      "<ul>",
      paste("<li><b>", col_info$Column, "</b>: ", col_info$Type, "</li>", collapse = ""),
      "</ul>"
    )
    
    HTML(html_output)
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
    
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    if (isTRUE(input$apply_filter) && !is.null(input$filter_var) &&
        input$filter_var %in% names(data) && !is.null(input$filter_range)) {
      
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
    
    plot_obj <- switch(input$plot_type,
                       "scatter" = {
                         req(input$x_var, input$y_var)
                         p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
                           theme_minimal()
                         
                         if(input$add_jitter) {
                           p <- p + geom_jitter(color = input$point_color, alpha = 0.6, 
                                                width = 0.1, height = 0.1)
                         } else {
                           p <- p + geom_point(color = input$point_color, alpha = 0.6)
                         }
                         
                         if(input$reg_type != "none") {
                           if(input$reg_type == "lm") {
                             p <- p + geom_smooth(method = "lm", color = "red", se = FALSE)
                           } else {
                             p <- p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                                                  color = "red", se = FALSE)
                           }
                         }
                         p
                       },
                       
                       "hist" = {
                         req(input$hist_var)
                         ggplot(data, aes_string(x = input$hist_var)) +
                           geom_histogram(fill = input$hist_color, bins = 30, alpha = 0.7) +
                           theme_minimal()
                       },
                       
                       "box" = {
                         req(input$box_var)
                         ggplot(data, aes_string(y = input$box_var)) +
                           geom_boxplot(fill = input$box_color, alpha = 0.7) +
                           theme_minimal()
                       },
                       
                       "corr" = {
                         num_data <- data %>% select(where(is.numeric))
                         if (ncol(num_data) < 2) {
                           showNotification("Not enough numeric columns for correlation matrix!", 
                                            type = "warning")
                           return(NULL)
                         }
                         corr_mat <- cor(num_data, use = "complete.obs")
                         corrplot(corr_mat, method = "color", tl.cex = 0.8)
                       }
    )
    
    plot_obj
  }, height = function() { input$plot_size })
  
  # (f) Summary Statistics (numeric columns)
  output$summary_stats <- renderDT({
    req(filtered_data())
    data <- filtered_data()
    
    num_data <- data %>% select(where(is.numeric))
    
    if (ncol(num_data) == 0) {
      return(datatable(data.frame(Note = "No numeric columns available for summary.")))
    }
    
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
      
      plot_obj <- switch(input$plot_type,
                         "scatter" = {
                           req(input$x_var, input$y_var)
                           p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
                             theme_minimal()
                           
                           if(input$add_jitter) {
                             p <- p + geom_jitter(color = input$point_color, alpha = 0.6, 
                                                  width = 0.1, height = 0.1)
                           } else {
                             p <- p + geom_point(color = input$point_color, alpha = 0.6)
                           }
                           
                           if(input$reg_type != "none") {
                             if(input$reg_type == "lm") {
                               p <- p + geom_smooth(method = "lm", color = "red", se = FALSE)
                             } else {
                               p <- p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                                                    color = "red", se = FALSE)
                             }
                           }
                           p
                         },
                         
                         "hist" = {
                           req(input$hist_var)
                           ggplot(data, aes_string(x = input$hist_var)) +
                             geom_histogram(fill = input$hist_color, bins = 30, alpha = 0.7) +
                             theme_minimal()
                         },
                         
                         "box" = {
                           req(input$box_var)
                           ggplot(data, aes_string(y = input$box_var)) +
                             geom_boxplot(fill = input$box_color, alpha = 0.7) +
                             theme_minimal()
                         },
                         
                         "corr" = {
                           num_data <- data %>% select(where(is.numeric))
                           corr_mat <- cor(num_data, use = "complete.obs")
                           corrplot(corr_mat, method = "color", tl.cex = 0.8)
                         }
      )
      
      ggsave(file, plot = plot_obj, device = "png", 
             width = 10, height = input$plot_size/100, dpi = 300)
    }
  )
}


shinyApp(ui, server)

