
#libraries
library(shiny)
library(dplyr)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(survival)
library(lubridate)



# UI for app
ui <- fluidPage(
  useShinyjs(), 
  tags$head(
    tags$style(HTML("
      .background-on {
        background: url('background.jpg') no-repeat center center fixed; 
        background-size: cover;
      }
      .background-off {
        background: none !important;
      }
       .centered-title, .centered-buttons {
        text-align: center;
      }
      .centered-buttons {
        width: 100%;
        position: absolute;
        top: 50%;
        transform: translateY(-50%);
      }
      
      #btn-group .btn:last-child {
        margin-right: 0; /* removes the space to the right of the last button */
      }
      #logo {
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px; /* space between the logo and the title */
      }
      
      #btn-group .btn {
        margin-right:50px; /* adds space to the right of each button */
        padding: 20px 30px; /* larger padding increases button size */
        font-size: 28px; /* larger font size for the text in the button */
        color: white; /* text color */
        background-color: #FF5733; /* button color, a shade of orange for contrast */
        border: none; /* no border */
        box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25); /* optional shadow for depth */
        text-shadow: 0px 2px 2px rgba(0, 0, 0, 0.5); /* shadow for text to make it stand out */
      }
    "))
  ),
  uiOutput("pageContent")
)


# UI for the first app (ui1)
ui1 <- function() {
  fluidPage(
    img(src = "Metro.jpg", id = "logo", height = "100px"),
    actionButton("toggleBg", "Background Switch", class = "btn btn-secondary"),
    actionButton("switchUI", "App Page", class = "btn btn-primary"),
    div(class = "centered-buttons",
        div(id = "btn-group",
            actionButton("doc", "Documentation", class = "btn btn-primary"),
            actionButton("runApp", "Run App", class = "btn btn-primary")
        )
    ),
    
  )
}

# UI for the second app (ui2)
ui2 <- function() {
  dashboardPage(
    dashboardHeader(title = "Analytics Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data Input", tabName = "dataInput", icon = icon("database")),
        menuItem("Regression Analysis", tabName = "regressionAnalysis", icon = icon("chart-line")),
        menuItem("Bus Analytics", tabName = "busAnalytics", icon = icon("bus")),
        menuItem("Settings", tabName = "settings", icon = icon("sliders"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "home",
                fluidRow(
                  box(title = "Information", width = 12,
                      p("Upload your data, run regression to get the bus analytics section working")
                  )
                )
        ),
        tabItem(tabName = "dataInput",
                fluidRow(
                  box(title = "Data Input", width = 12,
                      fileInput("file", "Choose a CSV or TXT data file", accept = c(".csv", ".txt")),
                      
                      br(),
                      fileInput("testFile", "Upload Test or Current Dataset", 
                                accept = c(".csv", ".txt", ".xls", ".xlsx"))
                  )
                )
        ),
        tabItem(tabName = "busAnalytics",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("buso", "Choose Bus Column:", choices = NULL),
                    textInput("bus_number", "Enter Bus Number", value = "")
                  ),
                  mainPanel(
                    valueBoxOutput("bus_count"),
                    valueBoxOutput("unique_drivers"),
                    valueBoxOutput("list_routes"),
                    valueBoxOutput("total_mileage"),
                    valueBoxOutput("mileage_difference"),
                    valueBoxOutput("predicted_risk_box"),
                    box(title = "Date and Corresponding Driver", width = 12, tableOutput("date_driver")),
                    box(title = "Top 10 buses with High probability of breakdown", width = 12, tableOutput("top_10_buses"))
                  )
                )
        ),#
        
        tabItem(tabName = "settings",
                fluidRow(
                  box(title = "Bus Analytics section Settings", status = "primary", solidHeader = TRUE, width = 12,
                      selectInput("selected_date_column", "Select Date Variable", choices = NULL),
                      selectInput("selected_bus_column", "Select Bus Variable", choices = NULL),
                      selectInput("selected_driver_column", "Select Driver Variable", choices = NULL),
                      selectInput("selected_route_column", "Select Route Variable", choices = NULL),
                      selectInput("selected_mileage_column", "Select Mileage Variable", choices = NULL),
                      br(),
                      br(),
                      
                      box(title = "Test Data Settings", status = "primary", solidHeader = TRUE, width = 12,
                          selectInput("selected_date", "Select Date", choices = NULL),
                          selectInput("selected_bus", "Select Bus", choices = NULL),
                          selectInput("selected_mileage", "Select Mileage", choices = NULL)),

                      br(),
                      br(),
                      box(title = "Change Data Types", status = "primary", solidHeader = TRUE, width = 12,
                          selectInput("types", "Select Datatypes", choices = c("numeric","integer", "character")),
                          
                          selectInput("datatypes", "List the variables you want to change the Datatypes:", choices = NULL, multiple = TRUE),
                          actionButton("change_data", "Change", class = "btn btn-info"))
                      
                  )
                )
        ),
        #
        tabItem(tabName = "regressionAnalysis",
                sidebarLayout(
                  sidebarPanel(
                    #  input controls for  analysis here
                    selectInput("date", "Choose Date:", choices = NULL),
                    selectInput("group", "Choose Group:", choices = NULL),
                    actionButton("sum", "Summary", class = "btn btn-info"),
                    actionButton("sum2", "Summary22", class = "btn btn-info"),
                    selectInput("num", "Choose at least one Numeric Predictors X:", choices = NULL, multiple = TRUE),
                    selectInput("cat", "Choose One Categorical Predictor:", choices = NULL),
                    selectInput("regType", "Choose Regression Type:",choices = c("Linear" = "linear", "Cox" = "cox")),
                    actionButton("reg", "Run Analysis"),
                    
                  ), 
                  mainPanel(
                    #  output elements for analysis here
                    verbatimTextOutput("sta"),
                    verbatimTextOutput("sta2"),
                    verbatimTextOutput("regSummary"),
                    verbatimTextOutput("coxSummary"),
                    verbatimTextOutput("predict_breakdown"),
                    verbatimTextOutput("predictionResult")
                  )
                )
        )
      )
    )
  )
}




# Server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)  # This sets the maximum size to 30MB
  
  # reactive values to manage state
  appState <- reactiveValues(currentUI = "ui1", backgroundOn = TRUE)
  
  # To Observe the current UI and update the background accordingly
  observe({
    if(appState$currentUI == "ui1" && appState$backgroundOn) {
      shinyjs::addClass(selector = "body", class = "background-on")
    } else {
      shinyjs::removeClass(selector = "body", class = "background-on")
    }
  })
  
  # Toggle background on and off
  observeEvent(input$toggleBg, {
    appState$backgroundOn <- !appState$backgroundOn
  })
  
  # Switch UI function
  observeEvent(input$switchUI, {
    appState$currentUI <- ifelse(appState$currentUI == "ui1", "ui2", "ui1")
    # When switching to ui2
    if(appState$currentUI == "ui2") {
      appState$backgroundOn <- FALSE
    }
  })
  
  # Dynamically update UI based on current state
  output$pageContent <- renderUI({
    if(appState$currentUI == "ui1") {
      ui1()
    } else {
      ui2()
    }
  })
  
  
  observeEvent(input$runApp, {
    appState$currentUI <- "ui2"
  })
  
  ##########
  observe({
    
    if (req(input$sidebarItemExpanded) == "home") {
      appState$currentUI <- "ui"
      
    }
  })
  #######
  
  
  
  # Rest of the server logic
  observeEvent(input$doc, {
    showModal(modalDialog(
      title = "Documentation",
      htmlOutput("docContent"), 
      size = "l" # Large modal size
    ))
  })
  
  # Render the HTML content for the documentation
  output$docContent <- renderUI({
    HTML(documentationHTML)
  })
  
  
 

  
  # Reactive to store the uploaded train data
  uploaded_data <- reactiveVal(data.frame())
  
  # Update the observer to handle Excel files
  observeEvent(input$file, {
    req(input$file)
    
    # Determine the file extension
    ext <- tools::file_ext(input$file$datapath)
    
    # Read the file based on the extension
    df <- switch(ext,
                 csv = read.csv(input$file$datapath, stringsAsFactors = FALSE),
                 txt = read.table(input$file$datapath, sep = ",", header = TRUE, stringsAsFactors = FALSE),
                 xls = read_excel(input$file$datapath),
                 xlsx = read_excel(input$file$datapath),
                 # Stop execution if the file is not supported
                 stop("Unsupported file type: ", ext)  
    )
    
    uploaded_data(df)
  })
  
  
  
  
  # Reactive to store the uploaded current data
  current_data <- reactiveVal(data.frame())
  
  # Update the observer to handle Excel files
  observeEvent(input$testFile, {
    req(input$testFile)
    
    # Determine the file extension
    ext <- tools::file_ext(input$testFile$datapath)
    
    # Read the file based on the extension
    current_df <- switch(ext,
                         csv = read.csv(input$testFile$datapath, stringsAsFactors = FALSE),
                         txt = read.table(input$testFile$datapath, sep = ",", header = TRUE, stringsAsFactors = FALSE),
                         xls = read_excel(input$testFile$datapath),
                         xlsx = read_excel(input$testFile$datapath),
                         # Stop execution if the file is not supported
                         stop("Unsupported file type: ", ext)  
    )
    
    current_data(current_df)
  })
  
  
  
  
  
  

  
  # Reactive for Data processing # FIXED THE DATA FORMAT ISSUE AND JOB PLAN
  processed_data <- reactive({
    req(input$file) # Ensure that a file has been uploaded
    
    
    # Assuming 'uploaded_data()' is a function you've defined to read the input file
    ndata <- uploaded_data() # 'uploaded_data()' holds the original uploaded data frame
    
    
    
    # Drop the 'jobplan' column if it exists
    if("jobplan" %in% names(ndata)) {
      ndata <- ndata %>% select(-jobplan)
    }
    
    # Convert the specified date column to Date format and calculate duration
    if (input$date != "") {
      ndata$Date = as.Date(ndata[[input$date]], format = "%Y-%m-%d %H:%M:%OS")
      
      ndata = ndata[order(ndata[[input$group]], ndata$Date), ]
      lag_by_group = function(x) c(NA, head(x, -1))
      ndata$datelag = ave(as.character(ndata$Date), ndata[[input$group]], FUN = lag_by_group)
      ndata$datelag = as.Date(ndata$datelag)
      
      # Calculate duration in days
      ndata$duration = as.numeric(ndata$Date-as.Date(ndata$datelag))
      
      #censor variable (uncomment and adjust the condition as needed)
      ndata$censor <- 1
      
      # Drop NA values if needed (uncomment if you want to omit NA values)
      ndata <- na.omit(ndata)
    }
    
    # Drop rows where any column has 'NULL' as a character string
    character_cols <- sapply(ndata, is.character)
    ndata <- ndata[rowSums(sapply(ndata[, character_cols, drop = FALSE], function(x) x == "NULL")) == 0, ]
    
    return(ndata) # Return the processed data
  })
  
  
  
  
  
  
  
  # Reactive for Data test or current processing
  current_processed_data <- reactive({
    req(input$testFile) # Ensure that a file has been uploaded
    
    
    # Assuming 'uploaded_data()' is a function you've defined to read the input file
    ndata <- current_data() 
    
    # Keep only the selected columns
    if (input$selected_mileage != "" && input$selected_bus != "" && input$selected_date != "") {
      ndata <- ndata %>% select(all_of(input$selected_mileage), all_of(input$selected_bus), all_of(input$selected_date))
    }
    
    # Drop the 'jobplan' column if it exists
    if("jobplan" %in% names(ndata)) {
      ndata <- ndata %>% select(-jobplan)
    }
    
    ndata$censor = 0
    # Convert the specified date column to Date format and calculate duration
    if (input$selected_date != "") {
      ndata$Date = as.Date(ndata[[input$selected_date]], format = "%Y-%m-%d %H:%M:%OS")
      
      ndata = ndata[order(ndata[[input$selected_bus]], ndata$Date), ]
      lag_by_group = function(x) c(NA, head(x, -1))
      ndata$datelag = ave(as.character(ndata$Date), ndata[[input$selected_bus]], FUN = lag_by_group)
      ndata$datelag = as.Date(ndata$datelag)
      
      # Calculate duration in days
      ndata$duration = as.numeric(ndata$Date-as.Date(ndata$datelag))
      
      #censor variable (uncomment and adjust the condition as needed)
      #ndata$censor <- 1
      
      # Drop NA values if needed (uncomment if you want to omit NA values)
      ndata <- na.omit(ndata)
    }
    
    # Drop rows where any column has 'NULL' as a character string
    character_cols <- sapply(ndata, is.character)
    ndata <- ndata[rowSums(sapply(ndata[, character_cols, drop = FALSE], function(x) x == "NULL")) == 0, ]
    
    
    ndata <- ndata %>%
      dplyr::select(-all_of(c(input$selected_date, "datelag")))
    
    
    return(ndata) # Return the processed data
  })
  
  

  

    
 
  
  
  # update input
  observe({
    req(uploaded_data())
    # for regression analytics
    updateSelectInput(session, "date", choices = names(uploaded_data()))
    updateSelectInput(session, "group", choices = names(uploaded_data()))
    updateSelectInput(session, "num", choices = names(uploaded_data()))
    updateSelectInput(session, "cat", choices = names(uploaded_data()))
    updateSelectInput(session, "buso", choices = names(uploaded_data()))
    
    # for settings
    updateSelectInput(session, "selected_date_column", choices = names(uploaded_data()))
    updateSelectInput(session, "selected_bus_column", choices = names(uploaded_data()))
    updateSelectInput(session, "selected_driver_column", choices = names(uploaded_data()))
    updateSelectInput(session, "selected_route_column", choices = names(uploaded_data()))
    updateSelectInput(session, "selected_mileage_column", choices = names(uploaded_data()))
    
  }) 
  
  # update input
  observe({
    req(current_data())
    
    # for settings
    updateSelectInput(session, "selected_date", choices = names(current_data()))
    updateSelectInput(session, "selected_bus", choices = names(current_data()))
    updateSelectInput(session, "selected_mileage", choices = names(current_data()))
    
  })
  
  
  
  
  
  
  # Output for Descriptive Statistics
  output$sta <- renderPrint({
    req(input$sum)
    ndata <- processed_data()
    cdata <- current_processed_data()
    
    f = function(x) {
      return(c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T),min=min(x,na.rm=T),max=max(x,na.rm=T)))
    }
    cat("The date variable is", as.character(input$date),"\n")
    cat("     ","","\n")
    cat("The group variable is", as.character(input$group),"\n")
    
    cat("First six obs are", "","\n")
    print(head(ndata))
    m = f(ndata$duration)
    cat("     ","","\n")
    cat("Descriptive Statistics of Duration is","","\n")
    print(m)
    cat("     ","","\n")
    cat("class of variables are","","\n")
    print(sapply(ndata,class))
    
    cat("The number of observation are","","\n")
    nrow(ndata)
    
    cat("class of variables are","","\n")
    print(sapply(cdata,class))
    
    cat("First six obs for current data", "","\n")
    print(head(cdata))
  })
  
  
  
  
  
  
  
  # Linear and Survival model output regression
  modelOutput <- reactiveValues(reg = NULL, cox = NULL)
  
  observeEvent(input$reg, {
    req(input$regType) # regression type 
    
    sdata <- processed_data() # pre-processed data
    
    # Clear the previous model output to refresh
    modelOutput$reg <- NULL
    modelOutput$cox <- NULL
    
    if (input$regType == "linear") {
      # Linear regression logic
      formula_str <- paste("duration", "~", paste(c(input$num, input$group), collapse = "+"))
      if (!is.na(input$cat) && input$cat != "") {
        formula_str <- paste(formula_str, "+", "factor(", input$cat, ")", sep = "")
      }
      
      lr <- lm(as.formula(formula_str), data = sdata)
      modelOutput$reg <- summary(lr)$coef
      
    } else if (input$regType == "cox") {
      # Cox regression logic
      # variables for the formula
      num_vars <- paste(input$num, collapse = " + ")
      group_var <- ifelse(input$group != "", paste(" + ", input$group), "")
      cat_var <- ifelse(!is.na(input$cat) && input$cat != "", paste(" + factor(", input$cat, ")"), "")
      
      formula_cox <- paste("Surv(duration, censor)", "~", num_vars, group_var, cat_var)
      cox_model <- coxph(as.formula(formula_cox), data = sdata)
      modelOutput$cox <- summary(cox_model)$coef
      
    }
  })
  
  
  #  the linear regression summary if it exists
  output$regSummary <- renderPrint({
    req(modelOutput$reg) 
    cat("Summary of Linear Model", "\n")
    print(modelOutput$reg)
  })
  
  # the Cox regression summary if it exists
  output$coxSummary <- renderPrint({
    req(modelOutput$cox) 
    cat("Summary of Cox Regression Model", "\n")
    print(modelOutput$cox)
  })
  
  

  
 
  
  
  
  
  # Bus Analytics Sections
  
  filtered_data <- reactive({
    req(input$bus_number, processed_data())
    data <- processed_data()
    selected_bus <- rlang::sym(input$selected_bus_column)
    data %>% dplyr::filter(!!selected_bus == input$bus_number)
  })
  
  
  
  # output for the number of times the bus number appears
  output$bus_count <- renderValueBox({
    data <- filtered_data()
    count <- nrow(data)
    valueBox(
      formatC(count, format = "d", big.mark = ','),
      subtitle = "Bus Repair",
      icon = icon("bus"),
      color = "yellow"
    )
  })
  
  #  output for the count of unique drivers
  output$unique_drivers <- renderValueBox({
    data <- filtered_data()
    selected_driver <- rlang::sym(input$selected_driver_column) 
    unique_drivers <- length(unique(data[[selected_driver]]))
    valueBox(
      formatC(unique_drivers, format = "d", big.mark = ','),
      subtitle = "Unique Drivers",
      icon = icon("id-badge"),
      color = "green"
    )
  })
  
  
  #  output for the list of routes
  output$list_routes <- renderValueBox({
    data <- filtered_data()
    selected_route <- rlang::sym(input$selected_route_column)
    routes <- unique(data[[selected_route]])
    valueBox(
      paste(routes, collapse = ", "),
      subtitle = "List of Routes",
      icon = icon("route"),
      color = "blue"
    )
  })
  
  
  output$total_mileage <- renderValueBox({
    data <- filtered_data()
    selected_mileage <- rlang::sym(input$selected_mileage_column)
    # Ensure data is sorted if necessary to make this assumption valid
    last_mileage <- tail(data[[selected_mileage]], n = 1)
    valueBox(
      formatC(last_mileage, format = "f", big.mark = ','),
      subtitle = "Total Mileage",
      icon = icon("tachometer-alt"),
      color = "red"
    )
  })
  
  
  
  output$mileage_difference <- renderValueBox({
    data <- filtered_data()
    # the last two mileage records
    selected_mileage <- rlang::sym(input$selected_mileage_column)
    last_two_mileages <- tail(data[[selected_mileage]], n = 2)
    
    # Default message
    content <- "Good Condition" 
    # Default icon for good condition
    icon_type <- icon("smile") 
    
    # Calculate the difference if there are at least two records
    if(length(last_two_mileages) > 1) {
      mileage_difference <-  abs(diff(last_two_mileages))
      content <- formatC(mileage_difference, format = "f", big.mark = ',')
      icon_type <- icon("exchange-alt") # Icon for difference
    }
    
    # Display the valueBox with the appropriate content and icon
    valueBox(
      content,
      subtitle = "Mileages to Last Breakdown",
      icon = icon_type,
      color = "green" 
    )
  })
  
  
  
  
  # predicted risk box
  output$predicted_risk_box <- renderValueBox({
    req(predicted_risk_score())  
    
    # Get the risk score
    risk_score <- predicted_risk_score()  
    
    if (is.na(risk_score)) {
      value <- "NA"
      subtitle <- "Risk score not available"
    } else {
      # Round the risk score for display
      value <- round(risk_score, 2)
      subtitle <- "Predicted Risk Score"
    }
    valueBox(
      value = value,
      subtitle = subtitle,
      icon = icon("dashboard"),
      color = "purple"  
    )
  })
  
  
  output$date_driver <- renderTable({
    data <- filtered_data()
    
    # Dynamically reference the selected columns for date and driver
    selected_date <- rlang::sym(input$selected_date_column)
    selected_driver <- rlang::sym(input$selected_driver_column)
    
    # Use the selected columns to filter the data and create the table
    data %>%
      dplyr::select(!!selected_date, !!selected_driver) %>%
      unique()
  })
  
  
  
  
  
 
  
# Risk score calculation
predicted_risk_score <- reactive({
  req(input$bus_number) 
  sdata <- processed_data()
  
  sdata <- na.omit(sdata)
  
  # Rename 'Date' column to avoid conflict with base R function
  names(sdata)[names(sdata) == "Date"] <- "EventDate"
  
  # Fit the Cox proportional hazards model
  cox_model <- coxph(Surv(duration, censor) ~ bus + EventDate, data = sdata)
  
  # Current system date for prediction purposes
  current_date <- Sys.Date()
  
  # Unique buses for prediction
  unique_buses <- unique(sdata$bus)
  
  #prediction data for all unique buses
  prediction_data <- purrr::map_df(unique_buses, function(bus_num) {
    last_date <- max(sdata$EventDate[sdata$bus == bus_num], na.rm = TRUE)
    tibble(
      duration = as.numeric(difftime(current_date, last_date, units = "days")),
      censor = 0,
      bus = bus_num,
      EventDate = current_date
    )
  })
  
  # Predict the survival probabilities for all unique buses
  predicted_survival <- predict(cox_model, newdata = prediction_data, type = 'survival')
  
  # Calculate the hazard (probability of breakdown) for each unique bus
  predicted_hazard <- 1 - predicted_survival
  
  # dataframe with the bus numbers and their corresponding predicted hazards
  hazard_dataframe <- data.frame(bus = unique_buses, hazard = predicted_hazard)
  
  # the hazard for the input$bus_number
  specific_bus_hazard <- hazard_dataframe[hazard_dataframe$bus == input$bus_number, "hazard", drop = FALSE]
  
  if (nrow(specific_bus_hazard) > 0) {
    print(specific_bus_hazard)
  } else {
    # Display pop-up message alert
    shiny::showNotification("Bus Information not found", duration = 4000)
    NULL  # Return NULL to prevent further processing
  }
})




  

 
  
  # New prediction of Risk score calculation for top 10 buses
  predicted_risk_score2 <- reactive({
    req(input$bus_number) 
    sdata <- processed_data()
    
    if (is.null(sdata) || nrow(sdata) == 0) {
      shinyalert::shinyalert(
        title = "No Data Available",
        text = "Please go to the Regression Analysis Tab and Run it. Then come back to Bus Analytics Tab to use it.",
        type = "warning"
      )
      return(NULL)
    }
    
    # Drop rows with missing values
    sdata <- na.omit(sdata)
    
    # Rename 'Date' column to avoid conflict with base R function
    names(sdata)[names(sdata) == "Date"] <- "EventDate"
    
    # Fit the Cox proportional hazards model
    cox_model <- try(coxph(Surv(duration, censor) ~ bus + EventDate, data = sdata), silent = TRUE)
    
    # If there's an error, display a warning message and return NULL
    if (class(cox_model) == "try-error") {
      shinyalert::shinyalert(
        title = "Please wait...",
        text = "Wait a second to get the risk ssssscore.",
        type = "info",
        showCancelButton = FALSE,
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE
      )
      
      # Delay for 1 second
      invalidateLater(1000, session$sendCustomMessage(type = "closeAlert"))
      return(NULL)
    }
    
    # Current system date for prediction purposes
    current_date <- Sys.Date()
    
    # Unique buses for prediction
    unique_buses <- unique(sdata$bus)
    
    # Create prediction data for all unique buses
    prediction_data <- purrr::map_df(unique_buses, function(bus_num) {
      last_date <- max(sdata$EventDate[sdata$bus == bus_num], na.rm = TRUE)
      tibble(
        duration = as.numeric(difftime(current_date, last_date, units = "days")),
        censor = 0,
        bus = bus_num,
        EventDate = current_date
      )
    })
    
    # Predict the survival probabilities for all unique buses
    predicted_survival <- predict(cox_model, newdata = prediction_data, type = 'survival')
    
    # Calculate the hazard (probability of breakdown) for each unique bus
    predicted_hazard <- 1 - predicted_survival
    
    # Dataframe with the bus numbers and their corresponding predicted hazards
    hazard_dataframe <- data.frame(bus = unique_buses, hazard = predicted_hazard)
    
    # Sort the dataframe by hazard in descending order
    sorted_hazard_df <- hazard_dataframe[order(hazard_dataframe$hazard, decreasing = TRUE), ]
    
    # Top 10 buses with high probability
    top_10_buses <- head(sorted_hazard_df, 10)
    
    # Specific hazard for the input bus number
    specific_bus_hazard <- hazard_dataframe[hazard_dataframe$bus == input$bus_number, "hazard", drop = FALSE]
    
    if (nrow(specific_bus_hazard) > 0) {
      specific_bus_hazard_text <- paste("Hazard for bus", input$bus_number, ":", specific_bus_hazard)
    } else {
      specific_bus_hazard_text <- paste("No hazard available for bus", input$bus_number)
    }
    
    list(
      specific_bus_hazard = specific_bus_hazard_text,
      top_10_buses = top_10_buses
    )
  })
  
  
  
  output$top_10_buses <- renderTable({
    predicted_risk_score2()$top_10_buses
  })
  

  
  
}




# HTML content for documentation
documentationHTML <- '
<div class="documentation-container">
  <h2>App Documentation</h2>
  <p>This document provides an overview of the application features and usage:</p>
  
  
  <div id="Overview" class="collapse">
    <p>This documentation provides a comprehensive guide to understanding the features and functionalities of the Shiny app. 
    The app is designed to provide analytics and insights into bus management, bus breakdowns, including data input, regression 
    analysis, and bus analytics.</p>
  </div>
  <p><a data-toggle="collapse" href="#Overview" role="button" aria-expanded="false" aria-controls="Overview">
    <h4>Overview</h4>
  </a></p>
  
  <h3>Features</h3>
  
  <div id="Background-Toggle" class="collapse">
    <p>Allows users to toggle the background image on and off for better visibility and customization of the app interface.</p>
  </div>
  <p><a data-toggle="collapse" href="#Background-Toggle" role="button" aria-expanded="false" aria-controls="Background-Toggle">
    <h4>Background Toggle</h4>
  </a></p>
  
  
  <div id="UI-Switching" class="collapse">
    <p>* Enables users to switch between different user interfaces within the application.</p>
    <p>* Users can seamlessly transition between different sections of the app for data input, regression analysis, and bus analytics</p>
  </div>
  <p><a data-toggle="collapse" href="#UI-Switching" role="button" aria-expanded="false" aria-controls="UI-Switching">
    <h4>UI Switching</h4>
  </a></p>
  
  
  <div id="Dynamic-Content" class="collapse">
    <p>* Dynamically updates the UI based on user interactions. </p>
     <p>* Provides a responsive and interactive user experience by adjusting the content based on user input and actions.</p>
  </div>
  <p><a data-toggle="collapse" href="#Dynamic-Content" role="button" aria-expanded="false" aria-controls="Dynamic-Content">
    <h4>Dynamic Content</h4>
  </a></p>
  
  
  <div id="error" class="collapse">
    <p>* Error Messages are embedded in the app, for debudging of any issues you might encounter during usage. </p>
  </div>
  <p><a data-toggle="collapse" href="#error" role="button" aria-expanded="false" aria-controls="errort">
    <h4>Error Handling</h4>
  </a></p>
  
  
  <h3>Usage</h3>
  
  <div id="Background_Toggle" class="collapse">
    <p>Click on the "Background Switch" button to toggle the background image on or off, enhancing the visual appeal of the app. </p>
     
  </div>
  <p><a data-toggle="collapse" href="#Background_Toggle" role="button" aria-expanded="false" aria-controls="Background_Toggle">
    <h4>Dynamic Content</h4>
  </a></p>
  
  
  <div id="UI_Switching" class="collapse">
    <p>*	Click on the "App Page" button to switch between different user interfaces within the application.</p>
    <p>* Navigate between sections such as data input, regression analysis, and bus analytics to access different features of the app.</p>
  </div>
  <p><a data-toggle="collapse" href="#UI_Switching" role="button" aria-expanded="false" aria-controls="UI_Switching">
    <h4>UI Switching</h4>
  </a></p>
  
  
  <div id="Data_Input" class="collapse">
    <p>* Upload a CSV or TXT data file using the "Choose a CSV or TXT data file" button.</p>
    <p>* The app automatically detects the file format and processes the uploaded data for further analysis. </p>
    <p>*	Note: To ensure the effectiveness of the bus analytics section, especially to get the bus breakdown prediction score, 
    you must first run regression analysis in the regression session.</p>
  </div>
  <p><a data-toggle="collapse" href="#Data_Input" role="button" aria-expanded="false" aria-controls="Data_Input">
    <h4>Data Input</h4>
  </a></p>
  
  
    <div id="Data_Processing" class="collapse">
    <p>* The app processes uploaded data to prepare it for analysis.</p>
    <p>* We convert date columns to the Date format and calculates duration for survival analysis.</p>
    <p>* Data is sorted and processed to handle censoring issues (created binary indicator for censoring (all =1, no censored duration)) 
    and ensure accurate analysis results.</p>
    <p>* The app assumes date character format as "%m/%d/%Y" for data processing.</p>
    <p>* Censoring issues are addressed by creating binary indicators for censoring, considering the staggered entry or exit of buses.</p>
  </div>
  <p><a data-toggle="collapse" href="#Data_Processing" role="button" aria-expanded="false" aria-controls="Data_Processing">
    <h4>Data Processing</h4>
  </a></p>
  
  
  <div id="Regression_Analysis" class="collapse">
    <p>* Make sure you have uploaded the data in the data input section.</p>
    <p>* Select variables for regression analysis, including working date, group, numeric and optional categorical predictors.</p>
    <p>* Choose to either run linear or Cox regression analysis to analyze the relationship between variables and predict outcomes.</p>
    <p>*	View summary statistics and regression results to gain insights into the dataset and model performance.</p>
  </div>
  <p><a data-toggle="collapse" href="#Regression_Analysis" role="button" aria-expanded="false" aria-controls="Regression_Analysis">
    <h4>Regression Analysis</h4>
  </a></p>
  
  
   <div id="Bus_Analytics" class="collapse">
    <p>* Choose the bus variable.</p>
    <p>* View key metrics such as the number of times a bus requires repair, the number of unique drivers, and the list of routes, 
    millage to last breakdown, total millage of the bus, and the newly predicted risk score of bus breakdown.</p>
    <p>* And also get the top 10 buses with high the risk of breakdown for individual buses based on survival analysis.</p>
  </div>
  <p><a data-toggle="collapse" href="#Bus_Analytics" role="button" aria-expanded="false" aria-controls="Bus_Analytics">
    <h4>Bus Analytics</h4>
  </a></p>
  
  <div id="Error_Handling" class="collapse">
    <p><h4> No Data Available:</h4> Please go to the Regression Analysis Tab and Run it. Then come back to Bus Analytics Tab to use it.
</p>
    <p><h4> Bus Data not Found:</h4>Its means the bus number you entered is not avaliable in the dataset.
</p>
    <p><h4> Wait a second to get predicted risk score:</h4> Means the model is training the model and making new prediction. Wait a moment for the prediction.
</p>
  </div>
  <p><a data-toggle="collapse" href="#Error_Handling" role="button" aria-expanded="false" aria-controls="Error_Handling">
    <h4>Error Handling</h4>
  </a></p>
  
    <div id="Conclusion" class="collapse">
    <p>This documentation provides a detailed overview of the features and functionalities of the Shiny app for bus fleet management. Users can 
    leverage the app to gain valuable insights into bus performance, maintenance, and risk analysis. For further details and technical information, 
    please refer to the source code and comments within the app.</p>
  </div>
  <p><a data-toggle="collapse" href="#Conclusion" role="button" aria-expanded="false" aria-controls="Conclusion">
    <h4>Conclusion</h4>
  </a></p>
  
  
  <p>For more details, refer to the source code and comments within.</p>
</div>
'

# Run the app
shinyApp(ui = ui, server = server)
