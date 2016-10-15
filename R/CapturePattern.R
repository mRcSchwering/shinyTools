#' CapturePatternUI
#'
#' The CapturePattern Module extracts captures from chr strings based on a regular expression and displays them.
#'
#' The UI part is a text output of several lines.
#'
#' @family CapturePattern module functions
#'
#' @param id        chr id of this object for shiny session
#' @param label     chr or NULL a lable/ title for the file upload
#'
#' @return chr HTML for creating ui elements.
#'
#' @examples
#' library(shinyTools)
#'
#' # some generic strings with regular expressions
#' regexes <- list("^(.+?)_.*$", "^(.+?)_(.*?)_(.*?)\\.(.*)$", "^.*(SG.+?)1.*_(.+?)_.*$")
#' lib <- paste0("ENSG00000139", apply(expand.grid(0:9, 0:9, 0:9, "_8_", sample(20000:60000, 5), ".", sample(20000:60000, 5)),
#'                                     1, function(x) paste0(x, collapse = "")))
#' # little app with module
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel( width = 4, h2("Pattern as input"), p("Not part of Module UI"),
#'                 selectizeInput("regex", "Regex", choices = regexes, options = list(create = TRUE)),
#'                 helpText("Write your own Regex to try out.")
#'   ),
#'   mainPanel( width = 8, h2("CapturePatternUI"),
#'              CapturePatternUI("cap", "Patterns Captured")
#'   )
#' ))
#'
#' server <-function(input, output, session) {
#'   callModule(CapturePattern, "cap", pat = reactive(input$regex), lines = reactive(lib))
#' }
#'
#' shinyApp(ui, server)
#'
#' @import shiny
#'
#' @export
#'
CapturePatternUI <- function(id, label) {
  ns <- NS(id)
  out <- tagList(h3(label), br(), uiOutput(ns("captures")))
  return(out)
}



#' CapturePattern
#'
#' The CapturePattern Module extracts captures from chr strings based on a regular expression and displays them.
#'
#' This is a module which helps in writing a regular expression to extract information from an array of chr strings.
#' The module looks for capture groups in the regular expression and uses them on each string.
#' Strings are displayed as text output with each capture group being labelled by a color.
#'
#' Regular expression (pat) and the strings (lines) are both reactive, so changing either will lead to an update.
#'
#' The number of lines displayed can be specified (default = 5).
#' So it is possible to --say-- scan a big  file of strings and use them as lines argument in this module.
#'
#' The colors can be defined. If more captures than the defined colors are found, these captures are not labelled.
#' So if length(cols) == 2 but 4 capture groups are found, only the first 2 apear colored.
#'
#' Nested capture groups do not work.
#'
#' @family CapturePattern module functions
#'
#' @param input      argument used by shiny session
#' @param output     argument used by shiny session
#' @param session    argument used by shiny session
#' @param pat        reactive chr of regular expression
#' @param lines      reactive chr arr of strings in which the regular expression should capture substrings
#' @param n          int(= 5) number of lines to be displayed
#' @param cols       chr arr or NULL(NULL) defining colors to label capture groups in the order they appear in each string
#'                   If NULL 4 default colors are used (ggl colors).
#'
#' @return NULL
#'
#' @examples
#' library(shinyTools)
#'
#' # some generic strings with regular expressions
#' regexes <- list("^(.+?)_.*$", "^(.+?)_(.*?)_(.*?)\\.(.*)$", "^.*(SG.+?)1.*_(.+?)_.*$")
#' lib <- paste0("ENSG00000139", apply(expand.grid(0:9, 0:9, 0:9, "_8_", sample(20000:60000, 5), ".", sample(20000:60000, 5)),
#'                                     1, function(x) paste0(x, collapse = "")))
#' # little app with module
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel( width = 4, h2("Pattern as input"), p("Not part of Module UI"),
#'                 selectizeInput("regex", "Regex", choices = regexes, options = list(create = TRUE)),
#'                 helpText("Write your own Regex to try out.")
#'   ),
#'   mainPanel( width = 8, h2("CapturePatternUI"),
#'              CapturePatternUI("cap", "Patterns Captured")
#'   )
#' ))
#'
#' server <-function(input, output, session) {
#'   callModule(CapturePattern, "cap", pat = reactive(input$regex), lines = reactive(lib))
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
#'
CapturePattern <- function(input, output, session, pat, lines, n = 5, cols = NULL) {

  if(is.null(cols)) cols <- c("#db4437", "#4285f4", "#0f9d58", "#f4b400")

  output$captures <- renderUI({
    x <- lines()

    if(length(x) < n) return(HTML(paste("<div style='color:red;'>Fewer than", n, "lines detected.</div>")))
    x <- x[grepl(pat(), x)]
    if(length(x) < n) return(HTML(paste("<div style='color:red;'>Fewer than", n, "lines match the pattern.</div>")))
    x <- x[sample(1:length(x), n)]
    if(length(pat()) < 1 || pat() == "") return(HTML(paste0("<tt>", paste(x, collapse = "<br/>"), "</tt>")))

    txt <- character()
    for( i in 1:n ){
      m <- GetCaptures(x[i], pat())
      m <- apply(rbind(m[1, ], apply(m, 2, function(s) substr(x, s[2], s[3]))), 2, function(ss){
        if(as.integer(ss[1]) != 0) paste0('<font color="', cols[as.integer(ss[1])], '">', ss[2], "</font>") else ss[2]})
      txt <- c(txt, paste(m, collapse = ""))
    }
    HTML(paste0("<tt>", paste( txt, collapse = "<br/>"), "</tt>"))
  })

}



