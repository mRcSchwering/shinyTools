#' FileUploadUI
#'
#' This file upload handler provides some functionalities for file uploading and possible renaming of uploaded files.
#'
#' These are ui elements of the file upload handler, an error message and some text.
#' In case rename is not NULL, several text input forms for renaming files are created after file upload.
#'
#' @family FileUpload module functions
#'
#' @param id        chr id of this object for shiny session
#' @param label     chr or NULL a lable/ title for the file upload
#' @param help      chr or NULL a help Text
#' @param rename    chr or NULL (NULL). If not NULL uploaded files can be renamed after upload.
#'                  Value of rename will be used as a label above the renaming ui.
#' @param multiple  bool (FALSE) whether more than 1 file can be uploaded
#' @param horiz     bool (FALSE) whether ui elements should be placed horizontally, not vertically
#'
#' @return chr HTML for creating ui elements.
#'
#' @examples
#' library(shinyTools)
#'
#' # check functions as example
#' check1 <- function(df, add){ if(any(grepl(add$type, df$type))) return(paste("Don't upload", add$type, "files."))}
#' check2 <- function(names, add){ if(any(grepl(add$pat, names))) return(paste("Don't use", add$pat, "in a file name."))}
#'
#' # little app with module
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel( width = 4, h2("FileUploadUI"),
#'                 FileUploadUI("uploadID", "Upload File", rename = "Rename File", multiple = TRUE, horiz = TRUE)
#'   ),
#'   mainPanel( width = 8, h2("Return value of FileUpload"),
#'              verbatimTextOutput("content1")
#'   )
#' ))
#' server <-function(input, output, session) {
#'   info <- callModule(FileUpload, "uploadID", rename = TRUE, checkNames = "check2", checkFiles = "check1",
#'                      addArgs = list(pat = "a", type = "text"))
#'   output$content1 <- renderPrint(info())
#' }
#' shinyApp(ui, server)
#'
#' @export
#'
FileUploadUI <- function(id, label, help = "Select files for upload.", rename = NULL, multiple = FALSE, horiz = FALSE) {
  ns <- NS(id)

  # checks
  if(!is.logical(c(multiple, horiz))) stop("Arguments multiple and horiz need to be logical")

  # file Input
  inputUI <- tagList(
    uiOutput(ns("errorMessage_files")),
    fileInput(ns("file"), label, multiple = multiple)
  )

  # force reupload every time
  tags$script(paste0('$( "#', ns("file"), '" ).on( "click", function() { this.value = null; });'))

  # rename UI
  if(!is.null(rename)){
    renameUI <- tagList(
      strong(rename),
      uiOutput(ns("errorMessage_rename")),
      uiOutput(ns("rename"))
    )
    if(!horiz) inputUI <- tagList(inputUI, br(), renameUI)
    if(horiz) inputUI <- tagList(fluidRow(column(6, inputUI), column(6, renameUI)))
  }

  # add help Text
  if(!is.null(help)) out <- tagList(inputUI, helpText(help))

  return(out)
}



#' FileUpload
#'
#' This file upload handler provides some functionalities for file uploading and possible renaming of uploaded files.
#'
#' On server side there is an additional option to include a function call which can be used to do a quality check
#' with uploaded files.
#' With argument checkFiles a function name can be defined.
#' This function must take a data.frame as first argument, which will contain information about uploaded files.
#' It has columns name, datapath, size, type.
#' If the uploaded files should be valid, the function must return NULL.
#' If they should not be valid a chr string must be returned.
#' This chr string will be shown in red in the ui as user feedback.
#'
#' checkNames works like checkFiles, just that it is a quality check for the user defined names, if rename = TRUE.
#' It must take a chr vector as first argument which are the names defined by the user.
#'
#' Additional argumets can be handed over to both functions via the list addArgs.
#'
#' @family FileUpload module functions
#'
#' @param input      argument used by shiny session
#' @param output     argument used by shiny session
#' @param session    argument used by shiny session
#' @param rename     bool (FALSE) whether interactive renaming of uploaded files is enabled
#' @param checkFiles chr or NULL (NULL) if not NULL name of a function which can be used as a quality check for uploaded files
#' @param checkNames chr or NULL (NULL) if not NULL name of a function which can be used as a quality check user defined names
#' @param addArgs    list or NULL (NULL) if not NULL list of additional arguments which will be passed to checkFiles or checkNames
#'
#' @return data.frame of information about uploaded files (and user defined generic names)
#'
#' @examples
#' library(shinyTools)
#'
#' # check functions as example
#' check1 <- function(df, add){ if(any(grepl(add$type, df$type))) return(paste("Don't upload", add$type, "files."))}
#' check2 <- function(names, add){ if(any(grepl(add$pat, names))) return(paste("Don't use", add$pat, "in a file name."))}
#'
#' # little app with module
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel( width = 4, h2("FileUploadUI"),
#'                 FileUploadUI("uploadID", "Upload File", rename = "Rename File", multiple = TRUE, horiz = TRUE)
#'   ),
#'   mainPanel( width = 8, h2("Return value of FileUpload"),
#'              verbatimTextOutput("content1")
#'   )
#' ))
#' server <-function(input, output, session) {
#'   info <- callModule(FileUpload, "uploadID", rename = TRUE, checkNames = "check2", checkFiles = "check1",
#'                      addArgs = list(pat = "a", type = "text"))
#'   output$content1 <- renderPrint(info())
#' }
#' shinyApp(ui, server)
#'
#' @export
#'
FileUpload <- function(input, output, session, rename = FALSE, checkFiles = NULL, checkNames = NULL, addArgs = NULL) {

  # checks
  if(class(rename) != "logical") stop("Argument rename must be TRUE or FALSE")
  if(!is.null(checkFiles) && class(checkFiles) != "character") stop("Argument checkFiles must be a chr or NULL")
  if(!is.null(checkNames) && class(checkNames) != "character") stop("Argument checkNames must be a chr or NULL")
  if(!is.null(addArgs) && class(addArgs) != "list") stop("Argument addArgs must be a list or NULL")

  # error messages
  error <- reactiveValues(uploaded = NULL, renamed = NULL)

  # uploaded files
  uploaded <- eventReactive(input$file, {
    df <- input$file
    if( is.null(df) || is.na(df) || length(df) == 0 || df == "" ){
      NULL
    } else if( is.null(checkFiles) ){
      df
    } else {
      error$uploaded <- do.call(checkFiles, args = list(df, addArgs))
      if(is.null(error$uploaded)) df else NULL
    }
  })

  # UI for renaming
  output$rename <- renderUI({
    if( is.null(uploaded()) || !rename ) return(NULL)
    df <- uploaded()
    ns <- session$ns
    out <- tagList()
    for(i in 1:nrow(df)) out[[i]] <- textInput(ns(paste0("rename_", i)), df$name[i])
    out
  })

  # check file names
  renamed <- reactive({
    if( is.null(uploaded())) return(NULL)
    df <- uploaded()
    if( !rename ) return(df)
    genNames <- character(0)
    for(i in 1:nrow(df)) genNames <- c(genNames, input[[paste0("rename_", i)]])
    if( nrow(df) != length(genNames) ) return(NULL)
    if( is.null(checkNames) ){
      return(cbind(data.frame(genNames = genNames), df))
    } else {
      error$renamed <- do.call(checkNames, args = list(genNames, addArgs))
      if(is.null(error$renamed)) cbind(data.frame(genName = genNames), df) else NULL
    }
  })

  # show error messages
  output$errorMessage_files <- renderUI(HTML(paste0("<div style='color:red'>", error$uploaded, "</div>")))
  output$errorMessage_rename <- renderUI(HTML(paste0("<div style='color:red'>", error$renamed, "</div>")))

  return(renamed)
}
