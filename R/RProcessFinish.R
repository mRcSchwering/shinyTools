#' RProcessFinishUI
#'
#' This is the UI part of a module to observe R scripts which are running as background processes from the shiny session.
#' It consists of only a red text field which will be visible as an error message if something during script execution went wrong.
#'
#' For more details see the documentation of the corresponding \code{\link{RProcessFinish}} function.
#'
#' @family RProcess module functions
#'
#' @seealso Seome examples on how to use RProcess module functions are given in the vignette \emph{RProcess Module Functions}
#'          (\code{vignette("RProcess_functions", package = "shinyTools")})
#'
#' @seealso In the R script communications functions such as \code{\link{Rscript_Init}} and \code{\link{Rscript_Fin}} should be used
#'          to ensure correct communication between the R script and the shiny session.
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
#'  This module observes a custom R script.
#'  Together with \code{\link{RProcessStart}} this module can start and observe a custom R script as background process.
#'  Both functions are combined in a convenience function \code{\link{RProcess}}.
#'
#'  If this function is used together with \code{\link{RProcessStart}}, they both need to be called with the same \code{id}
#'  and the same \code{pwd} (working directory).
#'
#'  The communication between the shiny session and the R script is done via a \code{*.status} file.
#'  Input and output are handed over via a \code{*.rds} file.
#'  The working directory of the process can be defined with \code{pwd}.
#'  This is where the files are read and written as well.
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
#' @param pwd        chr (getwd()) path of R batch process working directory.
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

