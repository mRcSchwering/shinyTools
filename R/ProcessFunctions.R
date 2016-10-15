#' ReadInfo
#'
#' Read content of a *.status file and return it as list of class ProcessInfo.
#' For creating the ProcessInfo object initially in a R script it is better to use \code{\link{RProcessInit}}.
#' It uses this function but also takes care of some other stuff.
#'
#' The *.status file is used to communicate with the shiny session.
#' Each line in the file is translated into a list element with several entries seperated by ";".
#' The first entry is used as the element name, the following ones are its values.
#'
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' @family RProcess module functions
#'
#' @param statusFile  chr path to relevant *.status file
#'
#' @return list of class ProcessInfo.
#'         This list contains information about the current process and is used by other functions of this module.
#'
#' @examples
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



#' Log
#'
#' This is a convenience function for writing a log message.
#' This should be used in a R script started by shiny with the RProcess module.
#'
#' If a log file exists, it is appended by the lines defined in with msg. Each element of msg is a line.
#' Information about path of the log file, the session id and the process id is taken from the ProcessInfo object (see arguments).
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' @family RProcess module functions
#'
#' @param msg   chr arr with log messages. Each element is a line.
#' @param info  ProcessInfo object or NULL (NULL).
#'              This is a list of relevant information about the process.
#'              It can be created with \code{\link{RProcessInit}}.
#'              If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return TRUE if a log was written, FALSE if not
#'
#' @examples
#'
#' @export
#'
Log <- function( msg, info = NULL )
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



#' Init
#'
#' If a R script was started by shiny using the RProcess module, this should be the first command of the script.
#' This function reads the *.status file to retrieve information about the process and load the input.
#' As a side effect an object 'info' of class 'ProcessInfo' is written into the global envir.
#' This is a list which contains information about the current process. It is used by other functions of this module.
#'
#' A *.status file is used for communication between the shiny session and the R batch process (started with Rscript).
#' Information provided by RProcess module is handed over to the R batch process via this file.
#' Conversely the R batch process writes on this file to hand over status updates to the shiny session.
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' Information about path of the log file, the session id and the process id is taken from the ProcessInfo object.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#'
#' @family RProcess module functions
#'
#' @param args  chr of path to *.status file (commandArgs(TRUE)).
#'              If the R script was started using RProcess module, this is the first trailing argument of Rscript (default).
#' @param log   chr arr used as initial log entry for this process. Each element is a line.
#'
#' @return input object as defined in RProcess module
#'
#'
#' @examples
#'
#' @export
#'
Init <- function( args = commandArgs(TRUE), log = paste("Starting at", Sys.time()) )
{
  if(length(args) < 1) stop("Argument args is empty")

  # reading info
  info <- ReadInfo(args[1])
  info$statusFile <- normalizePath(info$statusFile)

  # logging
  Log( c("", paste(rep("#", 40), collapse = ""), log, "", "ProcessInfo:", "",
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


#' Up
#'
#' This is a convenience function used in a R batch script for sending a status update to the shiny session which started it.
#'
#' A progess update might be used by the RProcess module in the shiny session to provide a progress bar.
#' In addition a log entry about the progress update is written in case a log file was defined.
#' This information is taken from the ProcessInfo object.
#'
#' The communication happens via a *.status file.
#' Information provided by RProcess module is handed over to the R batch process via this file.
#' Conversely the R batch process writes on this file to hand over status updates to the shiny session.
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' Information about path of the log file, the session id and the process id is taken from the ProcessInfo object.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family RProcess module functions
#'
#' @param progress  num > 0 and < 1 indicating progress
#' @param log       chr arr used as initial log entry for this process. Each element is a line.
#' @param info      ProcessInfo object or NULL (NULL).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{RProcessInit}}.
#'                  If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return list of class ProcessInfo.
#'         This list contains information about the current process and is used by other functions of this module.
#'
#' @examples
#'
#' @export
#'
Up <- function( progress, log = paste("Progress update to", progress), info = NULL )
{
  if(progress >= 1 || progress <= 0) stop("Progress must be greater 0 and smaller 1")

  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # write log if pid valid
  Log(log, info = info)

  # update progress
  info$progress <- progress
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)

  info
}




#' Try
#'
#' A convenience function for wrapping an expression in try and recovering from an error.
#'
#' This is intended for use in an R batch script if it was started by a shiny session using the RProcess module.
#' If the batch script stops because of some error, the shiny session will never know about this.
#' By wrapping an error-prone expression in this function, a signal that the script failed is given to the shiny session (if ignore = FALSE).
#' If a log file exists, an entry is made.
#' This information is taken from the ProcessInfo object.
#'
#' The communication happens via a *.status file.
#' Information provided by RProcess module is handed over to the R batch process via this file.
#' Conversely the R batch process writes on this file to hand over status updates to the shiny session.
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' If a session id exists, the log message is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family RProcess module functions
#'
#' @param expr      R expression to be evaluated
#' @param ignore    bool (FALSE) whether the script should continue after an error occured
#' @param info      ProcessInfo object or NULL (NULL).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{RProcessInit}}.
#'                  If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return NULL (if error occured) or result of evaluated expr
#'
#' @examples
#'
#' @export
#'
Try <- function( expr, ignore = FALSE, info = NULL )
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
    Log(c("Expression:", deparse(expr), "returned error:", res), info = info)
    if(!ignore){
      info$progress <- 1
      info$status <- res
      Log(paste("Quitting at", Sys.time()), info = info)
      write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)
      quit("no", 1)
    } else {
      Log("Error is ignored", info = info)
    }
    out <- NULL

  } else out <- res
  out
}





#' Fin
#'
#' This is a convenience function for a R batch script started by a shiny session with the RProcess module.
#' It should be the last command of a script since it writes an output file and send a termination singal.
#'
#' The output will overwrite the input file used by the same R script.
#' By setting the progress to 1, the shiny session knows that this R script has finished.
#' It will then read the output file. The script quits itself.
#'
#' The communication happens via a *.status file.
#' Information provided by RProcess module is handed over to the R batch process via this file.
#' Conversely the R batch process writes on this file to hand over status updates to the shiny session.
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' Information about path of the log file, the session id and the process id is taken from the ProcessInfo object.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family RProcess module functions
#'
#' @param output    object which will be handed over back to shiny session
#' @param log       chr arr used as initial log entry for this process. Each element is a line.
#' @param info      ProcessInfo object or NULL (NULL).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{RProcessInit}}.
#'                  If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return NULL
#'
#' @examples
#'
#' @export
#'
Fin <- function( output, log = paste("Process finishes at", Sys.time()), info = NULL )
{
  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # write log if pid valid
  Log(log, info = info)

  # write output
  saveRDS(output, info$ifof)

  # write status with finishing signal
  info$progress <- 1
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)

  quit("no", 0)
}





#' Abort
#'
#' This is a convenience function for a R batch script started by a shiny session with the RProcess module.
#' It should be the last command of a script since it writes an output file and send a termination singal.
#'
#' The output will overwrite the input file used by the same R script.
#' By setting the progress to 1, the shiny session knows that this R script has finished.
#' It will then read the output file. The script quits itself.
#'
#' The communication happens via a *.status file.
#' Information provided by RProcess module is handed over to the R batch process via this file.
#' Conversely the R batch process writes on this file to hand over status updates to the shiny session.
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' Information about path of the log file, the session id and the process id is taken from the ProcessInfo object.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family RProcess module functions
#'
#' @param output    object which will be handed over back to shiny session
#' @param log       chr arr used as initial log entry for this process. Each element is a line.
#' @param info      ProcessInfo object or NULL (NULL).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{RProcessInit}}.
#'                  If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return NULL
#'
#' @examples
#'
#' @export
#'
Abort <- function( status, log = c("Process aborts:", status), info = NULL )
{
  # look for ProcessInfo object
  if(is.null(info)) info <- get(
    Filter(function(x) class(get(x, envir = globalenv()))[1] == "ProcessInfo", ls(envir = globalenv()))[1],
    envir = globalenv())
  if(length(info) < 1) stop("Argument info empty or no ProcessInfo object found.")

  # write log if pid valid
  Log(log, info = info)

  # write status with finishing signal
  info$progress <- 1
  info$status <- status
  write(sapply(1:length(info), function(x) paste0(names(info)[x], ";", paste(info[[x]], collapse = ";"))), info$statusFile)

  quit("no", 0)
}






#' Start
#'
#' This is a convenience function for a R batch script started by a shiny session with the RProcess module.
#' It should be the last command of a script since it writes an output file and send a termination singal.
#'
#' The output will overwrite the input file used by the same R script.
#' By setting the progress to 1, the shiny session knows that this R script has finished.
#' It will then read the output file. The script quits itself.
#'
#' The communication happens via a *.status file.
#' Information provided by RProcess module is handed over to the R batch process via this file.
#' Conversely the R batch process writes on this file to hand over status updates to the shiny session.
#' By using the functions \code{\link{Init}}, \code{\link{Up}}, \code{\link{Log}}, \code{\link{Try}},
#' \code{\link{Fin}} it is ensured that the communication bewteen shiny session and R batch script work and
#' that no files are falsly overwritten.
#'
#' If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
#' Information about path of the log file, the session id and the process id is taken from the ProcessInfo object.
#' If a session id exists, every line is prefixed with it.
#' If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the *.status file.
#' If they do not match, quit() is run.
#' This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
#'
#' @family RProcess module functions
#'
#' @param id        chr of process id. This id will be used by RProcessFinish to watch results of this process
#' @param script    chr full path of R batch script which is to be started
#' @param log       chr arr used as log entry in case logFile is not NULL. Each element is a line.
#' @param logFile   chr or NULL (NULL) full path of logFile is needed. This log file will be used by the process started
#' @param info      ProcessInfo object or NULL (NULL).
#'                  This is a list of relevant information about the process.
#'                  It can be created with \code{\link{RProcessInit}}.
#'                  If NULL the global environment is searched for an object of class ProcessInfo.
#'
#' @return TRUE
#'
#' @examples
#'
#' @export
#'
Start <- function( id, script,
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
  info2$Rscript <- script #"/home/marc/src/mRc_repo/shinyTools/scripts/process2.R" # muss absolut path oder von pwd aus
  info2$logFile <- logFile #"/home/marc/src/mRc_repo/shinyTools/log/log2.log" # muss absolut path oder von pwd aus
  info2$statusFile <- file.path(info2$pwd, paste0(id, "-ifof.status"))

  # write log if pid valid
  Log(log, info = info)

  # write status and input
  saveRDS(input, info2$ifof)
  write(sapply(1:length(info2), function(x) paste0(names(info2)[x], ";", paste(info2[[x]], collapse = ";"))), info2$statusFile)

  # start process
  system2("Rscript", args = c(info2$Rscript, info2$statusFile), wait = FALSE, stdout = NULL, stderr = NULL)

  TRUE
}
