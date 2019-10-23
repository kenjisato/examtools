#'
#' A shiny gadget to check and fix Daten.txt
#'
#' @param dir character. Path to nops_scan (uncompressed directory)
#' @param file character. Typically "Daten.txt"
#' @param width integer. Width of the dialog viewer.
#' @param height integer. Width of the dialog viewer.
#' @param n integer. Number of problems.
#'
#' @importFrom utils read.delim
#' @import shiny
#' @import miniUI
check_daten <- function(dir, file = "Daten.txt", width = 800, height = 600, n = 45) {
  daten <- read.delim(file.path(dir, file),
                      sep = " ", header = FALSE, colClasses = "character"
  )
  image.path <- file.path(dir, daten[[1]])

  COLNUM <- c(exam = 2, type = 4, registration = 6, info.end = 6)
  checked_from_01 <- function(ans) {
    letters[1:5][as.logical(as.integer(strsplit(ans, split = "")[[1]]))]
  }
  checked_to_01 <- function(selected) {
    paste(as.numeric(!is.na(match(letters[1:5], selected))), collapse = "")
  }

  ui <- miniPage(
    tags$style(
      type = "text/css",
      HTML("div[id^='prb.']>*{float: left; margin-left: 25px; height: 15px;} div[id^='prb.'] {height: 15px;}")
    ),
    gadgetTitleBar(tools::file_path_as_absolute(dir)),
    miniTabstripPanel(
      miniTabPanel("Exam Sheet",
                   icon = icon("pencil-square"),
                   miniButtonBlock(
                     actionButton("previous_page", "Previous"),
                     actionButton("next_page", "Next"),
                     border = NULL
                   ),
                   miniContentPanel(
                     fillRow(
                       miniContentPanel(
                         textInput("registration", "registration", ""),
                         textInput("type", "type", ""),
                         textInput("exam", "exam", ""),
                         h5("Answers"),
                         fluidRow(div(id = "checkbox-container", uiOutput(outputId = "chkbox_ui")))
                       ),
                       fillCol(
                         flex = c(NA, 1),
                         h4(textOutput("imageName")),
                         miniContentPanel(plotOutput("png", height = "100%"))
                       )
                     )
                   )
      ),
      miniTabPanel("Data",
                   icon = icon("table"),
                   miniContentPanel(
                     DT::DTOutput("Daten")
                   )
      )
    )
  )

  server <- function(input, output, session) {

    # checkbox generation
    output$chkbox_ui <- renderUI({
      chkbox_list <- list()
      for (i in 1:n) {
        chkbox_list[[i]] <- checkboxGroupInput(
          inputId = paste0("prb.", i),
          label = formatC(i, width = 2, flag = "0"),
          choices = letters[1:5],
          inline = TRUE
        )
      }
      tagList(chkbox_list)
    })

    refresh <- function(i) {

      showImage <- function(i) {
        output$png <- renderPlot({
          img <- png::readPNG(image.path[[i]])
          grid::grid.raster(img)
        })
        output$imageName <- renderText(daten[[i, 1]])
      }

      showData <- function(i) {
        for (j in seq_len(n)) {
          updateTextInput(session, "registration", value = daten[i, COLNUM[["registration"]]])
          updateTextInput(session, "type", value = daten[i, COLNUM[["type"]]])
          updateTextInput(session, "exam", value = daten[i, COLNUM[["exam"]]])
          updateCheckboxGroupInput(session, paste0("prb.", j),
                                   selected = checked_from_01(daten[i, COLNUM[["info.end"]] + j])
          )
        }
      }

      showData(i)
      showImage(i)
    }

    output$Daten <- DT::renderDT(
      daten,
      server = FALSE,
      selection = list(mode = "single")
    )

    # Initialization
    currentIndex <- 1
    maxIndex <- nrow(daten)
    refresh(currentIndex)

    # Event handlers
    observeEvent(input$done, stopApp(daten))

    for (id in c("registration", "exam", "type")) {
      local({
        my_id <- id
        observeEvent(input[[my_id]], {
          daten[currentIndex, COLNUM[[my_id]]] <<- input[[my_id]]
        })
      })
    }

    for (j in seq_len(n)) {
      local({
        my_j <- j
        observeEvent(input[[paste0("prb.", my_j)]], {
          daten[currentIndex, COLNUM[["info.end"]] + my_j] <<-
            checked_to_01(input[[paste0("prb.", my_j)]])
        }, ignoreNULL = FALSE)
      })
    }

    observeEvent(input$previous_page, {
      if (currentIndex > 1) {
        currentIndex <<- currentIndex - 1
        refresh(currentIndex)
      }
    })

    observeEvent(input$next_page, {
      if (currentIndex < maxIndex) {
        currentIndex <<- currentIndex + 1
        refresh(currentIndex)
      }
    })

    observeEvent(input$Daten_rows_selected, {
      currentIndex <<- input$Daten_rows_selected
      output$selectedRow <- renderPrint(currentIndex)
      refresh(currentIndex)
    })
  }

  runGadget(ui, server,
            viewer = dialogViewer(file, width = width, height = height)
  )
}