% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProcessFunctions.R
\name{Rscript_Up}
\alias{Rscript_Up}
\title{Rscript_Up}
\usage{
Rscript_Up(progress, log = paste("Progress update to", progress),
  info = NULL)
}
\arguments{
\item{progress}{\code{num > 0} and \code{< 1} indicating progress}

\item{log}{\code{chr arr} used as initial log entry for this process (\code{paste("Progress update to", progress)}). Each element is a line.}

\item{info}{\emph{ProcessInfo} object or \code{NULL} (\code{NULL}).
This is a list of relevant information about the process.
It can be created with \code{\link{Rscript_Init}}.
If NULL the global environment is searched for an object of class \emph{ProcessInfo}.}
}
\value{
list of class \emph{ProcessInfo}.
        This list contains information about the current process and is used by other functions of this module.
}
\description{
This function is used to in a R script run by a RProcess module such as \code{\link{RProcess}} to communicate status updates to its shiny session.
}
\details{
A progess update might be used by a RProcess module in the shiny session to provide a progress bar.
In addition, a log entry about the progress update is written in case a log file was defined.
This information is taken from the \emph{ProcessInfo} object.

Information about path of the log file, the session id and the process id is taken from the \emph{ProcessInfo} object.
If a log file exists, it is appended by the lines defined with log. Each element of log is a line.
If a session id exists, every line is prefixed with it.
If a process id exists, before writing anything the loaded process id is compared with the current one -- read from the \code{*.status} file.
If they do not match the R session ends with a call to \code{quit}.
This ensures that, in case a newer version of the same process is running, the current one will quit and not overwrite anything.
}
\seealso{
These Rscript communication functions are used in R scripts started by a RProcess module such as \code{\link{RProcess}}.
         For some examples on how they work together see the vignette \emph{RProcess Module Functions}.

Other Rscript communication functions: \code{\link{ReadInfo}},
  \code{\link{Rscript_Abort}}, \code{\link{Rscript_Fin}},
  \code{\link{Rscript_Init}}, \code{\link{Rscript_Log}},
  \code{\link{Rscript_Start}}, \code{\link{Rscript_Try}}
}

