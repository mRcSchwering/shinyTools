#' RProcessFinishUI
#' UI part of a start module for R batch processes. The UI is only an error message which appears only if an error happened.
#'
#' This is a process starter for R batch processes which are started from the shiny session, but should run in the background.
#' This is useful for things that take a while to compute. The app stays responsive while the batch process is running.
#' An R object can be passed through to a R batch script which will be started with 'Rscript'.
#' This object and information about the R batch script are saved as an *.rds and an *.status file.
#' These files are used for communication between both processes and they are written into the directory defined as 'pwd'.
#' The 2 files are prefixed with the module id.
#'
#'
#' @family RProcess module functions
#'
#' @param id        chr id of this object for shiny session
#'
#' @return chr HTML for creating ui elements.
#'
#' @examples
#'
#' @export
#'
RProcessFinishUI <- function(id) {
  ns <- NS(id)
  return(uiOutput(ns("errorMessage_fin")))
}



#' RProcessFinish
#'
#' This is a process starter for R batch processes which are started from the shiny session, but should run in the background.
#' This is useful for things that take a while to compute. The app stays responsive while the batch process is running.
#'
#' An R object can be passed through to a R batch script which will be started with 'Rscript'.
#' This object and information about the R batch script are saved as an *.rds and an *.status file.
#' These files are used for communication between both processes and they are written into the directory defined as 'pwd'.
#' The process is started with a reactive 'trigger'.
#'
#' With argument checkFun a function name can be defined which will be used as quality control for 'object'.
#' This function is run before the R batch script is started.
#' First argument of this function is the 'object' handed over to RProcessStart.
#' The function must either return NULL or a chr value. NULL means the input is valid.
#' Thereby the module will start the R batch script.
#' If the input should not be valid, the function must return a character value.
#' This chr value will be rendered as a error message for the user, and the modul will not start the R batch script.
#'
#' Additional argumets can be handed over to checkFun via the list addArgs.
#'
#' @family RProcess module functions
#'
#' @param input      argument used by shiny session
#' @param output     argument used by shiny session
#' @param session    argument used by shiny session
#' @param trigger    reactive which starts the process (e.g. action button input)
#' @param object     reactive object that will be handed over to R batch script as input
#' @param script     chr path to R batch script (name of file included)
#' @param logFile    chr or NULL (NULL) path of (desired) logging file (name included)
#' @param sessionid  chr or NULL (NULL) id for the session. Log entries will be prefixed with it.
#' @param pwd        chr (getwd()) path of R batch process working directory.
#'                   That's where intermediate files will appear as well.
#' @param checkFun   chr or NULL (NULL) if not NULL name of a function which can be used as a quality check for object
#'                   right before it is handed ober to the R batch script
#' @param addArgs    list or NULL (NULL) if not NULL list of additional arguments which will be passed to checkFun
#'
#' @return NULL
#'
#' @examples
#'
#' @export
#'
RProcessFinish <- function( input, output, session, pwd = getwd(), millis = 1000 )
{
  # some constants
  id <- session$ns("")
  pwd <- normalizePath(pwd)

  # errors
  error <- reactiveValues(text = NULL)

  # read status file
  status <- reactiveFileReader(millis, session, file.path(pwd, paste0(id, "ifof.status")),
                               function(x) if(file.exists(x)) ReadInfo(x) else NULL)

  # read results file
  result <- eventReactive(status(), {
    error$text <- NULL
    out <- list(finished = NULL, result = NULL, error = NULL, progress = NULL)
    if( !is.null(status()) ){
      out$progress <- status()$progress
      if( status()$progress == "1" ){
        out$finished <- Sys.time()
        if( is.null(status()$status) ){
          out$result <- readRDS(file.path(pwd, paste0(id, "ifof.rds")))
        } else {
          out$error <- status()$status
          error$text <- status()$status
        }
      }
    }
    out
  })

  # show error message
  output$errorMessage_fin <- renderUI(HTML(paste0("<div style='color:red'>", error$text, "</div>")))

  return(result)
}

