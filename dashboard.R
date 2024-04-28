library(shiny)
library(dplyr)
library(shinydashboard) # Load the shinydashboard library

# Read the CSV file outside the server function to avoid reloading it on every reactive call
bus_data <- read.csv("C:/Users/USER/Documents/python lessons/eco/SORTA_Breakdowns.csv", stringsAsFactors = FALSE)

# Define UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Bus Analytics Dashboard"),
  dashboardSidebar(
    textInput("bus_number", "Enter Bus Number", value = "")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("bus_count"),
      valueBoxOutput("unique_drivers"),
      valueBoxOutput("list_routes"),
      valueBoxOutput("total_mileage")
    ),
    fluidRow(
      box(title = "Date and Corresponding Driver", width = 12, tableOutput("date_driver"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Filter the data based on bus number input
  filtered_data <- reactive({
    req(input$bus_number) # Require that a bus number is entered to proceed
    bus_data %>% filter(bus == input$bus_number)
  })
  
  # Display counts of times the bus number shows up in a valueBox
  output$bus_count <- renderValueBox({
    data <- filtered_data()
    count <- nrow(data)
    valueBox(
      formatC(count, format = "d", big.mark = ','),
      subtitle = "Bus Count",
      icon = icon("bus"),
      color = "yellow"
    )
  })
  
  # Display count of unique drivers in a valueBox
  output$unique_drivers <- renderValueBox({
    data <- filtered_data()
    unique_drivers <- length(unique(data$driver))
    valueBox(
      formatC(unique_drivers, format = "d", big.mark = ','),
      subtitle = "Unique Drivers",
      icon = icon("id-badge"),
      color = "green"
    )
  })
  
  # Display list of routes in a valueBox
  output$list_routes <- renderValueBox({
    data <- filtered_data()
    routes <- unique(data$route)
    valueBox(
      paste(routes, collapse = ", "),
      subtitle = "List of Routes",
      icon = icon("route"),
      color = "blue"
    )
  })
  
  # Display total sum of mileage in a valueBox
  output$total_mileage <- renderValueBox({
    data <- filtered_data()
    total_mileage <- sum(data$mileage, na.rm = TRUE)
    valueBox(
      formatC(total_mileage, format = "f", big.mark = ','),
      subtitle = "Total Mileage",
      icon = icon("tachometer-alt"),
      color = "red"
    )
  })
  
  # Display date and corresponding driver in a table
  output$date_driver <- renderTable({
    data <- filtered_data()
    data %>% select(wodate, driver) %>% unique()
  })
}

# Run the app
shinyApp(ui, server)
