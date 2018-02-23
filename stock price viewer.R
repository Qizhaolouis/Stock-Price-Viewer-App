library(shiny)
library(quantmod)
library(dygraphs)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  tags$head(tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');"))),
  headerPanel(
    h1("Qi Zhao Quant", 
       style = "font-family: 'Lobster', cursive;
       font-weight: 500; line-height: 1.1; 
       color: #4d3a7d;")),  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput(inputId = "obs",
                   label = "Number of days to view:",
                   value = 5),
      textInput("stock1", label="First Stock", value = "AMZN"),
      textInput("stock2", label="Second Stock", value = "AAPL"),
      textInput("start", label="Start Date (yyyy-mm-dd):", value = "2017-12-01"),
      textInput("end", label= "End Date (yyyy-mm-dd):", value = "2018-03-22"),
      selectInput("price",label = "which price you want to see?",
                  choices=c("open","high","low","close"),selected="open"),
      actionButton("go", "Let me see!")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: HTML table with requested number of observations ----
      dygraphOutput("dygraph") ,
      tableOutput("view1"), 
      tableOutput("view2")
      
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
  start <- eventReactive(input$go, {
    input$start
    })
  end <- eventReactive(input$go, {
    input$end
    })
  # Generate a summary of the dataset ----
  output$dygraph <- renderDygraph({
    start <- start() #2008-01-12
    end <- end()
    num=which(c("open","high","low","close")==input$price)
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
      getSymbols(stock1(), src = "yahoo", from = start(), to = end())
      data1=eval(parse(text=stock1()))
      tail(data1, n = input$obs)
    })
    output$view2 <- renderTable({
      getSymbols(stock2(), src = "yahoo", from = start(), to = end())
      data2=eval(parse(text=stock2()))
      tail(data2, n = input$obs)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

