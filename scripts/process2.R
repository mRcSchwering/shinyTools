input <- shinyTools::Init(commandArgs(TRUE))
Sys.sleep(4)
res <- shinyTools::Try(dnorm(1:100, input, 10))
shinyTools::Fin(res)









