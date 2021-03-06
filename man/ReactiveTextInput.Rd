% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReactiveTextInput.R
\name{ReactiveTextInput}
\alias{ReactiveTextInput}
\title{ReactiveTextInput}
\usage{
ReactiveTextInput(input, output, session, n, prefix = "Input",
  values = NULL, dummies = NULL, checkFun = NULL, addArgs = NULL)
}
\arguments{
\item{input}{argument used by shiny session}

\item{output}{argument used by shiny session}

\item{session}{argument used by shiny session}

\item{n}{reactive int which controls the number of text input fields}

\item{prefix}{chr or NULL ("Input") used as prefix for creating a label for each text input field. NULL for no labels}

\item{values}{chr arr or NULL (NULL) for default values for each text input field. This is an actual input.}

\item{dummies}{chr arr or NULL (NULL) for dummy text inside each text input field.
This text is visible in each field but not an actual input.}

\item{checkFun}{chr or NULL (NULL) if not NULL name of a function which can be used as a quality check for textInputs}

\item{addArgs}{list or NULL (NULL) if not NULL list of additional arguments which will be passed to checkFun}
}
\value{
chr arr of user provided text input fields or NULL
}
\description{
This is a reactive series of text input fields with a functionality for validating the provided text inputs.
The number of text input fields in reactive.
}
\details{
Arguments dummies and values can be NULL or a chr arr of >= 1.
As n -- the number of text input fields -- increases dummies and values are being iterated (see example n >= 3).

With argument checkFun a function name can be defined which will be used as quality control for the text inputs.
This function must take a chr arr as first argument. These are the user provided text inputs.
The function must either return NULL or a chr value. NULL means the input is valid.
Thereby the module returns this chr arr.
If the user input should not be valid, the function must return a character value.
This chr value will be rendered as a error message for the user, and the modul returns NULL.

Additional argumets can be handed over to checkFun via the list addArgs.
}
\examples{
library(shinyTools)

# some function as example
check <- function(text, add){ if(any(grepl(add$pat, text))) return(paste("Don't use letter", add$pat, "in any entry."))}

# little app with module
ui <- fluidPage(sidebarLayout(
  sidebarPanel(h2("ReactiveTextInputUI"),
               numericInput("nGroups", "n Groups", 2, -2, 10),
               ReactiveTextInputUI("id1", "Groups")
  ),
  mainPanel(h2("Output of ReactiveTextInput"), verbatimTextOutput("display"))
))


server <-function(input, output, session) {
  display <- callModule(ReactiveTextInput, "id1", n = reactive(input$nGroups), prefix = "Group",
                        values = c("Untreated", "Treated"), checkFun = "check", addArgs = list(pat = "X"))
  output$display <- renderPrint(display())
}

shinyApp(ui, server)

}
\seealso{
Other ReactiveTextInput module functions: \code{\link{ReactiveTextInputUI}}
}

