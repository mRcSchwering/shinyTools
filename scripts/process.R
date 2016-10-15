# read input and info about process, report to app that it started
input <- shinyTools::Init(commandArgs(TRUE))
#input <- shinyTools::Init("tmp/process-ifof.status")

# Log sth
shinyTools::Log(c("Input is", input))
Sys.sleep(2)
# some quality check
if(input > 100) shinyTools::Abort("Input greater 100")

# status update to shiny session
info <- shinyTools::Up(0.8)

# do stuff
Sys.sleep(4)
res <- shinyTools::Try(dpois(1:100, input))

# finish return result to shiny session
shinyTools::Fin(res)









