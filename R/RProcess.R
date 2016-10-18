#' RProcessUI
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
RProcessUI <- function(id) {
  ns <- NS(id)

  out <- tagList(
    RProcessStartUI(ns("inner")),
    RProcessFinishUI(ns("inner"))
  )

  return(out)
}



#' RProcess
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
RProcess <- function( input, output, session, trigger, object,
                      script, logFile = NULL, sessionid = NULL, pwd = getwd(),
                      checkFun = NULL, addArgs = NULL, millis = 1000 )
{
  callModule( RProcessStart, "inner", trigger = reactive(trigger()), object = reactive(object()),
              script = script, logFile = logFile, sessionid = sessionid, pwd = pwd,
              checkFun = checkFun, addArgs = addArgs )
  result <- callModule( RProcessFinish, "inner", pwd = pwd, millis = millis )
  return(result)
}
