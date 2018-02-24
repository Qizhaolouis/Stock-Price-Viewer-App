library(shiny)
library(quantmod)
library(dygraphs)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  headerPanel(
    h1("Quant Viewer", strong("by Qi Z"),
       style = "font-family: 'times', cursive;
       font-weight: 500; line-height: 1.1; 
       color: skyblue;")
    ),  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput(inputId = "obs",
                   label = "Number of days to view:",
                   value = 5),
      textInput("stock1", label="First Stock", value = "AMZN"),
      textInput("stock2", label="Second Stock", value = "AAPL"),
      dateRangeInput("dates", "Date range"),
      selectInput("price",label = "which price you want to see?",
                  choices=c("open","high","low","close","Volume","adjusted"),selected="open"),
      actionButton("go", "Let me see!")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      p("This is a lovely app for those who loves seeing",strong("stocks price")),
      # Output: HTML table with requested number of observations ----
      dygraphOutput("dygraph") ,
      tableOutput("view1"), 
      tableOutput("view2"),
      p("All the source from yahoo",a("https://finance.yahoo.com/"))
      
    ))
    )






# Define server logic required to draw a histogram
server <- function(input, output) {
  stock1 <- eventReactive(input$go, {
    input$stock1
  })
  stock2 <- eventReactive(input$go, {
    input$stock2
  })
  dates <- eventReactive(input$go, {
    input$dates
  })
  # Generate a summary of the dataset ----
  output$dygraph <- renderDygraph({
    dat <- dates()
    start <- dat[1] #2008-01-12
    end <- dat[2]
    num=which(c("open","high","low","close","Volume","adjusted")==input$price)
    getSymbols(stock1(), src = "yahoo", from = start, to = end)
    data1=eval(parse(text=stock1()))
    getSymbols(stock2(), src = "yahoo", from = start, to = end)
    data2=eval(parse(text=stock2()))
    data=cbind(data1[,num],data2[,num])
    dygraph(data, main = paste(input$stock1,"and",input$stock2,"stock price")) %>% 
      dyRangeSelector(dateWindow = c(start, end))
  })
  
  # Show the first "n" observations ----
  output$view1 <- renderTable({
    getSymbols(stock1(), src = "yahoo", from = dates()[1], to = dates()[2])
    data1=eval(parse(text=stock1()))
    tail(data1, n = input$obs)
  })
  output$view2 <- renderTable({
    getSymbols(stock2(), src = "yahoo", from = dates()[1], to = dates()[2])
    data2=eval(parse(text=stock2()))
    tail(data2, n = input$obs)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
