# gb6u_design_a_real-t.R
# Real-time Data Pipeline Dashboard Project

# Load necessary libraries
library(shiny)
library(plotly)
library(dplyr)
library(RSQLite)

# Define the UI
ui <- fluidPage(
  titlePanel("Real-time Data Pipeline Dashboard"),
  
  # Dashboard header
  headerPanel(
    h1("Data Pipeline Dashboard"),
    h4("Real-time data visualization and analysis")
  ),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      h4("Data Source"),
      radioButtons("dataSource", label = "Select Data Source",
                   choices = c("Sensor 1", "Sensor 2", "Sensor 3")),
      h4("Data Range"),
      dateRangeInput("dateRange", label = "Select Date Range")
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Real-time Data", plotlyOutput("realtimePlot")),
        tabPanel(" Historical Data", plotlyOutput("historicalPlot")),
        tabPanel("Data Quality", tableOutput("dataQualityTable"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Connect to SQLite database
  db <- dbConnect(RSQLite::SQLite(), "data_pipeline.db")
  
  # Define reactive data source
  data <- reactive({
    switch(input$dataSource,
             "Sensor 1" = dbGetQuery(db, "SELECT * FROM sensor1_data"),
             "Sensor 2" = dbGetQuery(db, "SELECT * FROM sensor2_data"),
             "Sensor 3" = dbGetQuery(db, "SELECT * FROM sensor3_data"))
  })
  
  # Real-time data plot
  output$realtimePlot <- renderPlotly({
    plot_ly(data(), x = ~time, y = ~value, type = "scatter", mode = "lines")
  })
  
  # Historical data plot
  output$historicalPlot <- renderPlotly({
    data() %>% 
      filter(time >= input$dateRange[1] & time <= input$dateRange[2]) %>% 
      plot_ly(x = ~time, y = ~value, type = "scatter", mode = "lines")
  })
  
  # Data quality table
  output$dataQualityTable <- renderTable({
    data() %>% 
      group_by(sensor_id) %>% 
      summarise(mean_value = mean(value), sd_value = sd(value))
  })
}

# Run the application
shinyApp(ui = ui, server = server)