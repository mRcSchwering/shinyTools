#' ReactiveTextInputUI
#'
#' This is a reactive series of text input fields with a functionality for validating the provided text inputs.
#' The number of text input fields in reactive.
#'
#'
#' @family ReactiveTextInput module functions
#'
#' @param id        chr id of this object for shiny session
#' @param title     chr or NULL for h3 title above text input fields
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
#'   sidebarPanel(h2("ReactiveTextInputUI"),
#'                numericInput("nGroups", "n Groups", 2, -2, 10),
#'                ReactiveTextInputUI("id1", "Groups")
#'   ),
#'   mainPanel(h2("Output of ReactiveTextInput"), verbatimTextOutput("display"))
#' ))
#'
#'
#' server <-function(input, output, session) {
#'   display <- callModule(ReactiveTextInput, "id1", n = reactive(input$nGroups), prefix = "Group",
#'                         values = c("Untreated", "Treated"), checkFun = "check", addArgs = list(pat = "X"))
#'   output$display <- renderPrint(display())
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
#'
ReactiveTextInputUI <- function(id, title) {
  ns <- NS(id)
  out <- tagList(h3(title), uiOutput(ns("textFields")), uiOutput(ns("errorMessage")))
  return(out)
}



#' ReactiveTextInput
#'
#' This is a reactive series of text input fields with a functionality for validating the provided text inputs.
#' The number of text input fields in reactive.
#'
#' Arguments dummies and values can be NULL or a chr arr of >= 1.
#' As n -- the number of text input fields -- increases dummies and values are being iterated (see example n >= 3).
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
#' @family ReactiveTextInput module functions
#'
#' @param input      argument used by shiny session
#' @param output     argument used by shiny session
#' @param session    argument used by shiny session
#' @param n          reactive int which controls the number of text input fields
#' @param prefix     chr or NULL ("Input") used as prefix for creating a label for each text input field. NULL for no labels
#' @param values     chr arr or NULL (NULL) for default values for each text input field. This is an actual input.
#' @param dummies    chr arr or NULL (NULL) for dummy text inside each text input field.
#'                   This text is visible in each field but not an actual input.
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
#'   sidebarPanel(h2("ReactiveTextInputUI"),
#'                numericInput("nGroups", "n Groups", 2, -2, 10),
#'                ReactiveTextInputUI("id1", "Groups")
#'   ),
#'   mainPanel(h2("Output of ReactiveTextInput"), verbatimTextOutput("display"))
#' ))
#'
#'
#' server <-function(input, output, session) {
#'   display <- callModule(ReactiveTextInput, "id1", n = reactive(input$nGroups), prefix = "Group",
#'                         values = c("Untreated", "Treated"), checkFun = "check", addArgs = list(pat = "X"))
#'   output$display <- renderPrint(display())
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
#'
ReactiveTextInput <- function(input, output, session, n, prefix = "Input", values = NULL, dummies = NULL,
                              checkFun = NULL, addArgs = NULL) {

  # checks
  if(length(prefix) > 1) stop("Argument prefix must be chr length 1 or NULL.")
  if(!is.null(checkFun) && class(checkFun) != "character") stop("Argument checkFun must be a chr or NULL")
  if(!is.null(addArgs) && class(addArgs) != "list") stop("Argument addArgs must be a list or NULL")

  # create input fields ui
  output$textFields <- renderUI({
    if( n() < 1 ) return(NULL)
    ns <- session$ns
    labs <- if(is.null(prefix)) NULL else paste(prefix, 1:n())
    tagList(lapply(1:n(), function(x) textInput(ns(paste0("text_", x)), labs[x], values[(x - 1)%%length(values) + 1],
                                      placeholder = dummies[(x - 1)%%length(dummies) + 1])))
  })

  # errors
  error <- reactiveValues(text = NULL)

  # output
  value <- reactive({
    out <- sapply(1:n(), function(x) input[[paste0("text_", x)]])
    if(class(out) == "list") return(NULL)
    if(is.null(checkFun)) return(out)
    error$text <- do.call(checkFun, args = list(out, addArgs))
    if(is.null(error$text)) out else NULL
  })

  # show error message
  output$errorMessage <- renderUI(HTML(paste0("<div style='color:red'>", error$text, "</div>")))

  return(value)
}
