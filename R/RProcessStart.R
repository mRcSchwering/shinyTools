#' RProcessStartUI
#'
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
RProcessStartUI <- function(id) {
  ns <- NS(id)
  return(uiOutput(ns("errorMessage_start")))
}



#' RProcessStart
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
RProcessStart <- function(input, output, session, trigger, object, script,
                          logFile = NULL, sessionid = NULL, pwd = getwd(),
                          checkFun = NULL, addArgs = NULL) {

  # checks
  if(!is.null(logFile) && class(logFile) != "character") stop("Argument logFile must be NULL or chr with path of desired log file")
  if(length(sessionid) > 1) stop("Argument sessionid must be NULL or length 1")
  if(!is.null(checkFun) && class(checkFun) != "character") stop("Argument checkFun must be a chr or NULL")
  if(!is.null(addArgs) && class(addArgs) != "list") stop("Argument addArgs must be a list or NULL")

  # some constants
  command <- "Rscript"
  id <- session$ns("ifof")
  script <- normalizePath(script)
  pwd <- normalizePath(pwd)

  # write logFile if necessary
  if(!is.null(logFile)){
    if(!file.exists(logFile)) file.create(logFile)
    logFile <- normalizePath(logFile)
  }

  # errors
  error <- reactiveValues(text = NULL)

  # process starter
  observeEvent(trigger(), {
    pid <- paste0(format(Sys.time(), "%Y-%m-%d_%H:%M_"), paste(sample(0:9, 4, replace = TRUE), collapse = ""))

    # check function
    if(!is.null(checkFun)){
      error$text <- do.call(checkFun, args = list(object(), addArgs))
      if(!is.null(error$text)) return(NULL)
    }

    # write ifof files
    saveRDS(object(), file.path(pwd, paste0(id, ".rds")))
    info <- c(paste0("progress;", 0), paste0("pid;", pid), paste0("sessionid;", sessionid),
              "status", paste0("pwd;", pwd), paste0("ifof;", file.path(pwd, paste0(id, ".rds"))),
              paste0(command, ";", script), paste0("logFile;", logFile))
    write(info, file.path(pwd, paste0(id, ".status")))

    system2(command, args = c(script, file.path(pwd, paste0(id, ".status"))), wait = FALSE, stdout = NULL, stderr = NULL)
  })

  # show error message
  output$errorMessage_start <- renderUI(HTML(paste0("<div style='color:red'>", error$text, "</div>")))

  return(NULL)
}
