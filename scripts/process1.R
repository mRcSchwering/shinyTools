input <- shinyTools::Init(commandArgs(TRUE))
Sys.sleep(4)
res <- shinyTools::Try(dpois(1:100, input))

shinyTools::Start("process2", "/home/marc/src/mRc_repo/shinyTools/scripts/process2.R",
      logFile = "/home/marc/src/mRc_repo/shinyTools/log/log2.log")

shinyTools::Fin(res)





