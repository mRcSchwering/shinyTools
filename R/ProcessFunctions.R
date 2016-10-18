#' ReadInfo
#'
#' @keywords internal
#'
#' Read content of a \code{*.status} file and return it as list of class ProcessInfo.
#' This function is used by different Rscript communication functions.
#'
#' The *.status file is used to communicate with the shiny session.
#' Each line in the file is translated into a list element with several entries seperated by ";".
#' The first entry is used as the element name, the following ones are its values.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param statusFile  chr path to relevant *.status file
#'
#' @return list of vectors of class ProcessInfo.
#'         This list contains information about the current process and is used by other Rscript communication functions.
#'
#' @export
#'
ReadInfo <- function( statusFile )
{
  info <- strsplit(readLines(statusFile), ";")
  names(info) <- sapply(info, function(x) x[1])
  info <- sapply(info, function(x) x[-1])
  info$statusFile <- statusFile
  info <- lapply(info, function(x) if(length(x) < 1) NULL else x)
  class(info) <- "ProcessInfo"
  info
}




#' Rscript_Log
#'
#' This function is used to write log messages for a R script run by a RProcess module such as \code{\link{RProcess}}.
#'
#' If a log file exists, it is appended by the lines defined in with msg. Each element of msg is a line.
#' Information about path of the log file, the session id and the process id is taken from the \code{ProcessInfo} object (see arguments).
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match, the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#'
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param msg   chr arr with log messages. Each element is a line.
#' @param info  ProcessInfo object or NULL (NULL).
#'              This is a list of relevant information about the process.
#'              It can be created with \code{\link{RProcessInit}}.
#'              If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return TRUE if a log was written, FALSE if not
#'
#' @export
#'
Rscript_Log <- function( msg, info = NULL )
{
  # get ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # create lines for log msg
  log <- c("", msg)
  if(!is.null(info$sessionid)) log <- paste(info$sessionid, log, sep = " : ")

  # write log after confirming pid
  pid <- ReadInfo(info$statusFile)$pid
  if(pid != info$pid){
    quit("no", 0)
  } else {
    if(!is.null(info$logFile)){
      write(log, info$logFile, append = TRUE)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}



#' Rscript_Init
#'
#' This function is used to read a \code{*.status} file and load the input for a R script which was started by a RProcess module
#' such as \code{\link{RProcess}}.
#' This should be the first command of the R script.
#'
#' The function returns the object handed over by the RProcess module it was started with.
#' It also writes a list \code{info} of class \emph{ProcessInfo} into the global environment which is used by all other Rscript communication functions.
#' This list contains information needed to correctly communicate with the shiny session.
#'
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' Information about path of the log file, the session id and the process id is taken from the \emph{ProcessInfo} object.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param args  chr of path to \code{*.status} file (\code{commandArgs(TRUE)}).
#'              If the R script was started using RProcess module, this is the first trailing argument of Rscript (default).
#' @param log   chr arr used as initial log entry for this process (\code{paste("Starting at", Sys.time())}). Each element is a line.
#'
#' @return input object as handed over from the RProcess module
#'         As a side effect an object \code{info} of class \emph{ProcessInfo} is written into the global environment.
#'         This is a list which contains information about the current process.
#'         It is used by all Rscript communication functions.
#'
#' @export
#'
Rscript_Init <- function( args = commandArgs(TRUE), log = paste("Starting at", Sys.time()) )
{
  if(length(args) < 1) stop("Argument args is empty")

  # reading info
  info <- ReadInfo(args[1])
  info$statusFile <- normalizePath(info$statusFile)

  # logging
  Rscript_Log( c("", paste(rep("#", 40), collapse = ""), log, "", "ProcessInfo:", "",
         sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";")))),
       info = info )

  # read input
  input <- readRDS(info$ifof)

  # update progress
  info$progress <- 0.1
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)
  info <<- info

  # set wd
  if(!is.null(info$pwd)) setwd(info$pwd)

  input
}




#' Rscript_Up
#'
#' This function is used to in a R script run by a RProcess module such as \code{\link{RProcess}} to communicate status updates to its shiny session.
#'
#' A progess update might be used by a RProcess module in the shiny session to provide a progress bar.
#' In addition, a log entry about the progress update is written in case a log file was defined.
#' This information is taken from the \emph{ProcessInfo} object.
#'
#' Information about path of the log file, the session id and the process id is taken from the \emph{ProcessInfo} object.
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param progress  \code{num > 0} and \code{< 1} indicating progress
#' @param log       \code{chr arr} used as initial log entry for this process (\code{paste("Progress update to", progress)}). Each element is a line.
#' @param info      \emph{ProcessInfo} object or \code{NULL} (\code{NULL}).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{Rscript_Init}}.
#'                  If NULL the global environment is searched for an object of class \emph{ProcessInfo}.
#'
#' @return list of class \emph{ProcessInfo}.
#'         This list contains information about the current process and is used by other functions of this module.
#'
#' @export
#'
Rscript_Up <- function( progress, log = paste("Progress update to", progress), info = NULL )
{
  if(progress >= 1 || progress <= 0) stop("Progress must be greater 0 and smaller 1")

  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # write log if pid valid
  Rscript_Log(log, info = info)

  # update progress
  info$progress <- progress
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)

  info
}




#' Rscript_Try
#'
#' This function is used to recover an error in a R script run by a RProcess module such as \code{\link{RProcess}}.
#' In general, if the R script runs into an error its shiny session will never know about it.
#' This functions, reports this error to the shiny session and writes a log if desired.
#'
#' For correct communication with the shiny session an \emph{ProcessInfo} object given (argument \code{info}).
#' If a log file exists, it is appended with the error message.
#' If a session id exists the log is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param expr      R expression to be evaluated
#' @param ignore    \code{bool} (\code{FALSE}) whether the script should continue after an error occured
#' @param info      \emph{ProcessInfo} object or \code{NULL} (\code{NULL}).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{Rscript_Init}}.
#'                  If NULL the global environment is searched for an object of class \emph{ProcessInfo}.
#'
#' @return \code{NULL} (if error occured) or result of evaluated expr
#'
#' @export
#'
Rscript_Try <- function( expr, ignore = FALSE, info = NULL )
{
  # eval expr
  expr <- substitute(expr)
  res <- try(eval(expr))

  # get process info
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # action
  # in case of error write log and update status
  if(class(res)[1] == "try-error"){
    res <- gsub("\n", "", res[1])
    Rscript_Log(c("Expression:", deparse(expr), "returned error:", res), info = info)
    if(!ignore){
      info$progress <- 1
      info$status <- res
      Rscript_Log(paste("Quitting at", Sys.time()), info = info)
      write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)
      quit("no", 1)
    } else {
      Rscript_Log("Error is ignored", info = info)
    }
    out <- NULL

  } else out <- res
  out
}





#' Rscript_Fin
#'
#' This function is used finish a R script run by a RProcess module such as \code{\link{RProcess}}.
#' It should be the last command of the script since it writes an output file, tells the shiny session about it,
#' and ends the R session with a call to \code{quit}.
#'
#'
#' The output will overwrite the input file used by the same R script.
#' By setting progress to 1 in the \code{*.status} file, the shiny session knows that this R script has finished.
#' It will then read the output file.
#'
#' For correct communication with the shiny session an \emph{ProcessInfo} object given (argument \code{info}).
#' If a log file exists, it is appended with the message given in argument \code{log}. Each element is a line.
#' If a session id exists the log is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param output    object which will be handed over back to shiny session
#' @param log       \code{chr} arr used as log entry for this process (\code{paste("Process finishes at", Sys.time())}). Each element is a line.
#' @param info      \emph{ProcessInfo} object or \code{NULL} (\code{NULL}).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{Rscript_Init}}.
#'                  If \code{NULL} the global environment is searched for an object of class \emph{ProcessInfo}.
#'
#' @return NULL
#'
#' @export
#'
Rscript_Fin <- function( output, log = paste("Process finishes at", Sys.time()), info = NULL )
{
  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # write log if pid valid
  Rscript_Log(log, info = info)

  # write output
  saveRDS(output, info$ifof)

  # write status with finishing signal
  info$progress <- 1
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)

  quit("no", 0)
}





#' Rscript_Abort
#'
#' This function is used to terminate a R script run by a RProcess module such as \code{\link{RProcess}}.
#' It will send a error message to the shiny session and end the R session with \code{quit}.
#' This way it can be used to make quality checks before handing data back to the shiny session.
#'
#' As in \code{\link{Rscript_Fin}} progress in the \code{*.status} file is set to 1, signalling the shiny session that the script has finished.
#' However, the status parameter in this file is filled with an error message.
#' The shiny session will not read the output of the R script but instead render the error message.
#'
#' For correct communication with the shiny session an \emph{ProcessInfo} object given (argument \code{info}).
#' If a log file exists, it is appended with the message given in argument \code{log}. Each element is a line.
#' If a session id exists the log is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param status    \code{chr} of status, which will be rendered as error message in the shiny session
#' @param log       \code{chr} arr used as log entry if a log file exists (\code{c("Process aborts:", status)}). Each element is 1 line.
#' @param info      \emph{ProcessInfo} object or \code{NULL} (\code{NULL}).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{Rscript_Init}}.
#'                  If \code{NULL} the global environment is searched for an object of class \emph{ProcessInfo}.
#'
#' @return NULL
#'
#' @export
#'
Rscript_Abort <- function( status, log = c("Process aborts:", status), info = NULL )
{
  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # write log if pid valid
  Rscript_Log(log, info = info)

  # write status with finishing signal
  info$progress <- 1
  info$status <- status
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)

  quit("no", 0)
}






#' Rscript_Start
#'
#' This function is used to start another R script from a R script started with a RProcess module such as \code{\link{RProcess}}.
#'
#' For correct communication with the shiny session an \emph{ProcessInfo} object given (argument \code{info}).
#' If a log file exists, it is appended with the message given in argument \code{log}. Each element is a line.
#' If a session id exists the log is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
#' If they do not match the R session ends with a call to \code{quit}.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family Rscript communication functions
#' @seealso These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
#'          For some examples on how they work together see the vignette \emph{RProcess Module Functions}.
#'
#' @param id        \code{chr} of process id. This id will be used by \code{RProcessFinish} to watch results of this process.
#'                   It defines how \code{.*status} and \code{.*rds} files for this process are named.
#' @param script    \code{chr} full path of R batch script which is to be started
#' @param log       \code{chr} arr used as log entry in case a log file exists. Each element is a line.
#' @param logFile   \code{chr} or \code{NULL} (\code{NULL}) full path of log file is needed. This log file will be used by the process started
#' @param info      \emph{ProcessInfo} object or \code{NULL} (\code{NULL}).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{Rscript_Init}}.
#'                  If \code{NULL} the global environment is searched for an object of class \emph{ProcessInfo}.
#'
#' @return TRUE
#'
#' @export
#'
Rscript_Start <- function( id, script,
                           log = c(paste0("Start process with id ", id, ":"), script),
                           logFile = NULL, info = NULL )
{
  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # make new info object
  info2 <- info
  info2$progress <- 0
  info2$ifof <- file.path(info2$pwd, paste0(id, "-ifof.rds"))
  info2$Rscript <- script
  info2$logFile <- logFile
  info2$statusFile <- file.path(info2$pwd, paste0(id, "-ifof.status"))

  # write log if pid valid
  Rscript_Log(log, info = info)

  # write status and input
  saveRDS(input, info2$ifof)
  write(sapply(1:length(info2), function(x) paste0(names(info2)[x], ";", paste(info2[[x]], collapse = ";"))), info2$statusFile)

  # start process
  system2("Rscript", args = c(info2$Rscript, info2$statusFile), wait = FALSE, stdout = NULL, stderr = NULL)

  TRUE
}
