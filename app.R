# Create a Shiny app that retrieves and displays stock price data. 
# We will use the quantmod package to retrieve stock price data from Yahoo Finance.

# Load the required packages
library(shiny)
library(quantmod)
library(tibble)
library(tidyr)
library(ggplot2)
library(dplyr)

# The UI part of the app
# It should have an input and an output component
# Input should be a multi-select dropdown menu that allows the user to select multiple stock symbols
# The stock symbols allowed include: AAPL, GOOGL, MSFT, AMZN, FB, TSLA, NVDA, INTC, CSCO, and ADBE
# Output should comprise one table and one plot. 
# The table should display the stock price data for the selected stock symbols.
# The plot should be made with the ggplot2 package and should display the stock price data for the selected stock symbols.

ui <- fluidPage(
  titlePanel("Stock Price Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stock", "Select Stock Symbols", 
                  choices = c("AAPL", "GOOGL", "MSFT", "AMZN", "META", "TSLA", "NVDA", "INTC", "CSCO", "ADBE"), 
                  # AAPL should be selected by default
                  selected = "AAPL",
                  multiple = TRUE)
    ),
    mainPanel(
      tableOutput("stock_table"),
      plotOutput("stock_plot")
    )
  )
)

# The server part of the app
# It should have a function that retrieves stock price data from Yahoo Finance
# The function should return a data frame with the stock price data
# The server part should also have a function that creates a plot of the stock price data using ggplot2
# The server part should also have a function that renders the table and plot in the UI

server <- function(input, output) {
  
  # Function to retrieve stock price data
  # This function should have two arguments: 
  # stock_symbols (a character vector of stock symbols) 
  # start_date (a character string representing the start date)
  # This function should return a data frame with the stock price data, from the start date to the current date
  get_stock_data <- function(stock_symbols, start_date = "2020-01-01") {
    stock_data <- lapply(stock_symbols, function(symbol) {
      stock_data <- getSymbols(symbol, from = start_date, auto.assign = FALSE)
      stock_data <- tibble(date = index(stock_data),
                           close = coredata(Cl(stock_data))[, 1],
                           symbol = symbol)
    })
    do.call(rbind, stock_data)
  }
  
  # Create a reactive object to store the stock data
  stock_data <- reactive({
    get_stock_data(input$stock, start_date = "2024-05-01")
  })
  
  # Function to create a plot of the stock price data
  create_stock_plot <- function(data) {
    req(nrow(data) > 0)
    ggplot(data, aes(x = date, y = close, color = symbol)) +
      geom_line() +
      labs(title = "Stock Price Data",
           x = "Date",
           y = "Close Price",
           color = "Stock Symbol") +
      theme_minimal()
  }
  
  # Function to create a table of the stock price data
  # The input data is in long format,
  # We need to convert it to wide format for display in the table
  # Each stock symbol should have its own column
  create_stock_table <- function(data) {
    req(nrow(data) > 0)
    stock_data_wide <- data %>% 
      spread(symbol, close) %>%
      mutate(date = as.character(date))
    stock_data_wide
  }
  
  # Create a reactive value to store the table object
  stock_table_rv <- reactive({
    create_stock_table(stock_data())
  })
  
  # Create a reactive value to store the plot object
  stock_plot_rv <- reactive({
    create_stock_plot(stock_data())
  })
  
  # # Render the table and plot in the UI
  output$stock_table <- snapshotPreprocessOutput(renderTable({
    stock_table_rv()
  }), function(table_value){
    # Modify the snapshot value
    # The actual values in the table are not important for this test
    # So we want to remove them
    # Remove the actual values from the table
    table_value <- gsub("<td>.*</td>", "<td>VALUE</td>", table_value)
  })

  # output$stock_table <- renderTable({
  #   stock_table_rv()
  # })
  
  output$stock_plot <- renderPlot({
    stock_plot_rv()
  })
}

# Run the Shiny app
stock_app <- shinyApp(ui = ui, server = server)
runApp(stock_app, test.mode = TRUE)