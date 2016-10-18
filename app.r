library(shinyTools)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    h2("Distribution"),
    numericInput("num1", "set lambda", 10, -10, 50),
    RProcessUI("process"),
    actionButton("trigger", "Start"),
    helpText("Poisson distribution is created in R batch script with a few seconds delay.")
  ),
  mainPanel(
    h3("Poisson Distribution"),
    plotOutput("plot")
  )
))

server <-function(input, output, session) {
  result <- callModule(RProcess, "process", trigger = reactive(input$trigger), object = reactive(input$num1), script = "./process.R")
  output$plot <- renderPlot(if(is.null(result()$result)) NULL else plot(1:100, result()$result))
}

shinyApp(ui, server)
