#' TextInputUI
#'
#' This is a series of textInputs with a functionality for validating the provided text inputs.
#'
#' The length of labels argument defines how many input fields there are.
#' So, argument n of the TextInput (server) function must be equal to length(labels).
#'
#' @family TextInput module functions
#'
#' @param id        chr id of this object for shiny session
#' @param labels    chr arr for textInput labels. length defines how many input fields there are
#' @param value     chr arr of length 1 or length(labels) defining default string inside all or each input fields.
#'                  NULL for no default value text. This is an actual input if it is not changed.
#' @param dummy     chr arr of length 1 or length(labels) defining dummy text inside all or each input fields.
#'                  NULL for no dummy text. This appears inside text field but is not an actual input.
#' @param help      chr or NULL (NULL) for help text placed underneath input fields.
#' @param horiz     bool (FALSE) whether ui elements should be placed horizontally, not vertically
#'
#' @return chr HTML for creating ui elements.
#'
#' @examples
#' library(shinyTools)
#'
#' # some function as example
#' check <- function(text, add){ if(any(grepl(add$pat, text))) return(paste("Don't use letter", add$pat, "in any entry."))}
#'
#' # little app with module
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel(h2("TextInputUI"),
#'                TextInputUI("id1", c("positive Ctrls", "non-targeting Ctrls"), c("positive", "non-targeting"),
#'                            help = "use HGNC symbols", horiz = FALSE)
#'   ),
#'   mainPanel(h2("Output of TextInput"), verbatimTextOutput("display"))
#' ))
#'
#' server <-function(input, output, session) {
#'   display <- callModule(TextInput, "id1", n = 2, checkFun = "check", addArgs = list(pat = "X"))
#'   output$display <- renderPrint(display())
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
#'
TextInputUI <- function(id, labels, value = NULL, dummy = "Insert text here", help = "Some message about input.", horiz = FALSE) {
  ns <- NS(id)

  # checks
  if(length(dummy) > 1 && length(dummy) != length(labels)) stop("labels has length ", length(labels), " but dummy has length ", length(dummy))
  if(length(value) > 1 && length(value) != length(labels)) stop("labels has length ", length(labels), " but value has length ", length(value))

  # build ui
  n <- length(labels)
  out <- tagList()
  if(length(dummy) < 2) dummy <- rep(dummy, length(labels))
  if(length(value) < 2) value <- rep(value, length(labels))
  for(i in 1:n) out[[i]] <- textInput(ns(paste0("text_", i)), labels[i], value[i], placeholder =  dummy[i])

  if(horiz) out <- div(out, class = "shiny-flow-layout")
  out <- tagList(out, uiOutput(ns("errorMessage")))
  if(!is.null(help)) out <- tagList(out, helpText(help))
  if(horiz) out <- div(out, align = "center")

  return(out)
}



#' TextInput
#'
#' This is a series of textInputs fields with a functionality for validating the provided text inputs.
#'
#' With argument checkFun a function name can be defined which will be used as quality control for the text inputs.
#' This function must take a chr arr as first argument. These are the user provided text inputs.
#' The function must either return NULL or a chr value. NULL means the input is valid.
#' Thereby the module returns this chr arr.
#' If the user input should not be valid, the function must return a character value.
#' This chr value will be rendered as a error message for the user, and the modul returns NULL.
#'
#' Additional argumets can be handed over to checkFun via the list addArgs.
#'
#' @family TextInput module functions
#'
#' @param input      argument used by shiny session
#' @param output     argument used by shiny session
#' @param session    argument used by shiny session
#' @param n          int number of textInput fields. n must be length(labels) from FileUploadUI.
#' @param checkFun   chr or NULL (NULL) if not NULL name of a function which can be used as a quality check for textInputs
#' @param addArgs    list or NULL (NULL) if not NULL list of additional arguments which will be passed to checkFun
#'
#' @return chr arr of user provided text input fields or NULL
#'
#' @examples
#' library(shinyTools)
#'
#' # some function as example
#' check <- function(text, add){ if(any(grepl(add$pat, text))) return(paste("Don't use letter", add$pat, "in any entry."))}
#'
#' # little app with module
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel(h2("TextInputUI"),
#'                TextInputUI("id1", c("positive Ctrls", "non-targeting Ctrls"), c("positive", "non-targeting"),
#'                            help = "use HGNC symbols", horiz = FALSE)
#'   ),
#'   mainPanel(h2("Output of TextInput"), verbatimTextOutput("display"))
#' ))
#'
#' server <-function(input, output, session) {
#'   display <- callModule(TextInput, "id1", n = 2, checkFun = "check", addArgs = list(pat = "X"))
#'   output$display <- renderPrint(display())
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
#'
TextInput <- function(input, output, session, n, checkFun = NULL, addArgs = NULL) {

  # checks
  if(!is.null(checkFun) && class(checkFun) != "character") stop("Argument checkFun must be a chr or NULL")
  if(!is.null(addArgs) && class(addArgs) != "list") stop("Argument addArgs must be a list or NULL")

  # errors
  error <- reactiveValues(text = NULL)

  # output
  value <- reactive({
    out <- character(n)
    for(i in 1:n) out[i] <- input[[paste0("text_", i)]]
    if(is.null(checkFun)) return(out)
    error$text <- do.call(checkFun, args = list(out, addArgs))
    if(is.null(error$text)) out else NULL
  })

  # show error message
  output$errorMessage <- renderUI(HTML(paste0("<div style='color:red'>", error$text, "</div>")))

  return(value)
}
