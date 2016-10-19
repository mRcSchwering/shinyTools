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
#'  This module starts and then observes a custom R script.
#'  The communication between the shiny session and the R script is done via a \code{*.status} file.
#'  Input and output are handed via a \code{*.rds} file.
#'
#'  To ensure that the communication between the shiny session and the R script is working properly, use \code{\link{Rscript_Init}}
#'  to start the script and \code{\link{Rscript_Fin}} to finish it.
#'  These functions belong to the Rscript communication function which should be used in the R script for communication with the shiny session.
#'  For examples see the vignette on \emph{RProcess Module Functions}
#'
#' @family RProcess module functions
#'
#' @seealso For further information see the documentation of Rscript communication such as \code{\link{Rscript_Init}}.
#'          Examples can be found in the vignette \emph{RProcess Module Functions}.
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
#'  This module starts a custom R script.
#'  Together with \code{\link{RProcessFinish}} this module can start and observe a custom R script as background process.
#'  Both functions are combined in a convenience function \code{\link{RProcess}}.
#'
#'  If this function is used together with \code{\link{RProcessFinish}}, they both need to be called with the same \code{id}
#'  and the same \code{pwd} (working directory).
#'  Otherwise the communication bewteen R script and shiny session will not work.
#'
#'  The communication between the shiny session and the R script is done via a \code{*.status} file.
#'  Input and output are handed over via a \code{*.rds} file.
#'  The working directory of the process can be defined with \code{pwd}.
#'  This is where the files are read and written as well.
#'
#'  There is the option to let the R script write a log.
#'  The desired log file can be given with \code{logFile}.
#'  If several users use the app it might be useful to include a unique id with \code{sessionid}.
#'  Every log entry will be appended with it.
#'
#' With \code{checkFun} a function name can be defined which will be used as quality control for \code{object}.
#' This function is run before the R batch script is started.
#' First argument of this function is the \code{object}.
#' The function must either return \code{NULL} or a \code{chr} value. \code{NULL} means the input is valid.
#' Thereby the module will start the R script.
#' If the input should not be valid, the function must return a character value.
#' This \code{chr} will be rendered as a error message for the user, and the modul will not start the R script.
#' Additional arguments to this \code{checkFun} can be handed over with \code{addArgs}.
#'
#'  To ensure that the communication between the shiny session and the R script is working properly, use \code{\link{Rscript_Init}}
#'  to start the script and \code{\link{Rscript_Fin}} to finish it.
#'  These functions belong to the Rscript communication function which should be used in the R script for communication with the shiny session.
#'  For examples see the vignette on \emph{RProcess Module Functions}
#'
#' @family RProcess module functions
#'
#' @seealso For further information see the documentation of Rscript communication such as \code{\link{Rscript_Init}}.
#'          Examples can be found in the vignette \emph{RProcess Module Functions}.
#'
#' @param input      argument used by shiny session
#' @param output     argument used by shiny session
#' @param session    argument used by shiny session
#' @param trigger    reactive which starts the process (e.g. action button input)
#' @param object     reactive object that will be handed over to R batch script as input
#' @param script     \code{chr} path to R batch script (name of file included)
#' @param logFile    \code{chr} or \code{NULL} (\code{NULL}) path of logging file (name included). Will be created if it doesnt exist.
#' @param sessionid  \code{chr} or \code{NULL} (\code{NULL}) id for the session. Log entries will be prefixed with it.
#' @param pwd        \code{chr} (\code{getwd()}) path of R batch process working directory.
#'                   That's where intermediate files will be written as well.
#' @param checkFun   \code{chr} or \code{NULL} (\code{NULL}) if not \code{NULL} name of a function which can be used as a quality check for object
#'                   right before it is handed ober to the R batch script
#' @param addArgs    \code{list} or \code{NULL} (\code{NULL}) if not NULL list of additional arguments which will be passed to checkFun
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
