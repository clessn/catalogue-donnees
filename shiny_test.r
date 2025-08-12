library(shiny)
library(jsonlite)

ui <- fluidPage(
  titlePanel("JSON Codebook Viewer and Editor"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload JSON Codebook", accept = ".json"),
      downloadButton("downloadBtn", "Download Edited JSON")
    ),
    mainPanel(
      tableOutput("codebookTable")
    )
  )
)

server <- function(input, output, session) {
  
  codebookData <- reactiveVal(data.frame())
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    json_data <- fromJSON(input$fileUpload$datapath[1])
    codebookData(json_data)
  })
  
  output$codebookTable <- renderTable({
    codebookData()
  }, editable = TRUE)
  
  observeEvent(input$codebookTable_cell_edit, {
    info <- input$codebookTable_cell_edit
    str(info)
    modified_data <- codebookData()
    modified_data[info$row, info$col] <- info$value
    codebookData(modified_data)
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste0("edited_", input$fileUpload$name)
    },
    content = function(file) {
      write_json(codebookData(), file)
    }
  )
  
}

shinyApp(ui, server)