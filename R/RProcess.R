#' RProcessUI
#'
#' UI part of a module which starts and observes a custom R script as background process.
#' The UI of this module is a red text field which pops up as error message if something went wrong when starting the script or during script execution.
#'
#' For more details see the documentation of the corresponding \code{\link{RProcess}} function.
#'
#' @family RProcess module functions
#'
#' @seealso Seome examples on how to use RProcess module functions are given in the vignette \emph{RProcess Module Functions}
#'          (\code{vignette("RProcess_functions", package = "shinyTools")})
#'
#' @seealso In the R script communications functions such as \code{\link{Rscript_Init}} and \code{\link{Rscript_Fin}} should be used
#'          to ensure correct communication between the R script and the shiny session.
#'
#' @param id        \code{chr} id of this object for shiny session
#'
#' @return \code{chr} HTML for creating ui elements.
#'
#' @export
#'
RProcessUI <- function(id)
{
  ns <- NS(id)

  out <- tagList(
    RProcessStartUI(ns("inner")),
    RProcessFinishUI(ns("inner"))
  )

  return(out)
}



#' RProcess
#'
#'  This module starts and then observes a custom R script as a background process.
#'  The app stays responsive while the R script is running.
#'  It is a convenience function which combines \code{\link{RProcessStart}} and \code{\link{RProcessFinish}}
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
#'  For examples see the vignette on \emph{RProcess Module Functions} (\code{vignette("RProcess_functions", package = "shinyTools")}).
#'
#' @family RProcess module functions
#'
#' @seealso Seome examples on how to use RProcess module functions are given in the vignette \emph{RProcess Module Functions}
#'          (\code{vignette("RProcess_functions", package = "shinyTools")})
#'
#' @seealso In the R script communications functions such as \code{\link{Rscript_Init}} and \code{\link{Rscript_Fin}} should be used
#'          to ensure correct communication between the R script and the shiny session.
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
#' @param millis     \code{int} (1000) of the time interval in milli seconds in which to read the \code{*.status} file for possible changes
#'
#' @return \emph{list} with elements 4 elements
#' \itemize{
#'   \item \emph{finished} indicating the time at which the R script has finished (\code{NULL} if not finished yet)
#'   \item \emph{result} the actual output of the R script (\code{NULL} if not finished yet)
#'   \item \emph{error} if the R script finished with an error, a relevant message is given here as \code{chr} (\code{NULL} if no error occured)
#'   \item \emph{progress} \code{num} indicating progress of R script (1 if finished, [0,1[ if running, \code{NULL} otherwise)
#' }
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
