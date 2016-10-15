library(shinyTools)

check <- function(object, add){ if(object <= add$n) return(paste("Lambda must be greater", add$n, "idiot!"))}

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    h2("Distributions"),
    numericInput("num1", "set lambda", 10, -10, 50),
    RProcessStartUI("process1"),
    actionButton("trigger", "Start"),
    helpText("Poisson distribution is created in a first R batch script which takes a few seconds.
             Before this script finishes a second R batch script is started by the first one.
             This calculates a normal distribution with lambda as mean.")
  ),
  mainPanel(
    h3("Poisson Distribution"),
    RProcessFinishUI("process1"),
    plotOutput("plot1"),
    h3("Normal Distribution"),
    RProcessFinishUI("process2"),
    plotOutput("plot2")
  )
))

server <-function(input, output, session) {
  callModule(RProcessStart, "process1", trigger = reactive(input$trigger), object = reactive(input$num1),
             script = "./scripts/process1.R", logFile = "log/log1.log", pwd = "./tmp", sessionid = "asd",
             checkFun = "check", addArgs = list(n = 0))
  result1 <- callModule(RProcessFinish, "process1", pwd = "./tmp")
  result2 <- callModule(RProcessFinish, "process2", pwd = "./tmp")
  output$plot1 <- renderPlot(plot(1:100, result1()$result))
  output$plot2 <- renderPlot(plot(1:100, result2()$result))
}

shinyApp(ui, server)


