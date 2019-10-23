#' Launches a shiny gadget for browsing nops_eval.zip of written
#' exams created with R/exams.
#'
#' @param file character. Path to nops_eval.zip or unzipped directory.
#' @param width integer. Width of dialogViewer
#' @param height integer. Height of dialogViewer
#'
#' @return data.frame that will be used to overwrite Data.txt
#' @export
#'
#' @import shiny
#' @import miniUI
browse_nops_eval <- function(file, width = 1000, height = 800){

  temp_dir <- tempfile()

  ui <- miniPage(
    uiOutput("titleBar"),
    miniContentPanel(
      htmlOutput("nopsReport")
    ),
    miniButtonBlock(
      actionButton("previous_page", "Previous"),
      actionButton("next_page", "Next")
    )
  )

  server <- function(input, output, session) {

    getPage <- function(idx) includeHTML(html_files[[idx]])
    updatePosition <- function(a, b) {
      renderUI(gadgetTitleBar(paste(a, "/", b)))
    }

    gracefullyStop <- function() {
      message("Gracefully stopping ...")
      unlink(temp_dir, recursive = TRUE)
      stopApp()
    }

    dir.create(temp_dir)
    zip::unzip(file, exdir = temp_dir)

    html_files <- list.files(temp_dir, pattern = "\\.html?", full.names = TRUE,
                             recursive = TRUE)
    maxIndex <- length(html_files)
    currentIndex <- 1
    output$nopsReport <- renderUI({getPage(currentIndex)})
    output$titleBar <- updatePosition(currentIndex, maxIndex)

    observeEvent(input$done, gracefullyStop())
    observeEvent(input$cancel, gracefullyStop())
    observeEvent(input$previous_page, {
      if (currentIndex > 1) {
        currentIndex <<- currentIndex - 1
        output$nopsReport <- renderUI({getPage(currentIndex)})
        output$titleBar <- updatePosition(currentIndex, maxIndex)
      }
    })
    observeEvent(input$next_page, {
      if (currentIndex < maxIndex){
        currentIndex <<- currentIndex + 1
        output$nopsReport <- renderUI({getPage(currentIndex)})
        output$titleBar <- updatePosition(currentIndex, maxIndex)
      }
    })
  }

  tryCatch(runGadget(ui, server,
                     viewer = dialogViewer(file, width = width, height = height)))

}


