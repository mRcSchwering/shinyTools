
# multiple Pages ----------------------------------------------------------
# inspired by daattali


modalTitle <- "Test Modal"
nPages <- 4

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyBS::bsModal(id = "modal", title = modalTitle, 
                   trigger = "show_modal", size = "large", 
                   shinyjs::hidden(
                     tagList(
                       div(class = "page", id = "page1", 
                           h3("New Accounts"), 
                           p("These accounts are not in the database yet.
                             Please check if you want to enter them like this."),
                           p(strong("Some Accounts")),
                           p("With a click on ", strong("Next"), 
                             " the new accounts are entered into the database.
                             If you want to abort, just close this window. 
                             Nothing has been entered yet.")
                           ),
                       div(class = "page", id = "page2", 
                           h3("Predictions"), 
                           p("The learning machine predicted the following types
                             for each prediction.
                             Please check if they are correct.
                             If not correct them."),
                           p(strong("Predictions")),
                           p("With a click on ", strong("Next"), 
                             " these transactions are not entered into the database yet.
                             If you want to abort you can do that by closing this window.
                             So far, only the new accounts from the previous page were entered.")
                           ),
                       div(class = "page", id = "page3", 
                           h3("Duplicated"),
                           p("These transactions were already found in the database.
                             They are assumed to be duplicates. 
                             If not check them and they will be entered anyway."),
                           p(strong("Duplicates")),
                           p("With a click on ", strong("Next"),
                             " these transactions are entered into the database.
                             If you want to abort you can do that by closing this window.
                             You can also go to the previous window and correct the ", em("types"),
                             ". So far, only the new accounts from the previous page were entered.")
                           ),
                       div(class = "page", id = "page4", 
                           h3("Finish"), 
                           p("Transactions were successfully entered into the database.")  
                       )
                           )
                           ),
                   actionButton("btnPrev", "< Previous"),
                   actionButton("btnNext", "Next >")
                       ),
  actionButton("show_modal", "Show Modal")
  )

server <- function( input, output, session )
{
  status <- reactiveValues(page = 1)
  
  observeEvent(status$page, {
    shinyjs::toggleState(id = "btnPrev", condition = status$page == 3)
    shinyjs::toggleState(id = "btnNext", condition = status$page < nPages)
    shinyjs::hide(selector = ".page")
    shinyjs::show(sprintf("page%s", status$page))
  })
  
  observeEvent(input$btnPrev, status$page <- status$page + -1)
  observeEvent(input$btnNext, status$page <- status$page + 1)
}

shinyApp(ui, server)

