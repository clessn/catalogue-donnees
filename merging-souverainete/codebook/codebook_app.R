# Load Required Libraries
library(shiny)
library(shinyWidgets)
library(jsonlite)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body::after {
        content: '';
        display: block;
        position: fixed;
        top: 0;
        right: 0;
        bottom: 0;
        left: 0;
        z-index: -1;
        background-image: url('https://upload.wikimedia.org/wikipedia/commons/7/74/Drapeau_des_Patriotes_%28avec_Patriote%29.jpg');
        background-size: cover;
        opacity: 1;
      }
      .container-fluid, .shiny-output-error {
        background-color: rgba(255, 255, 255, 0.9) !important;
      }
    "))
  ),
  titlePanel("Codebook souveraineté, ON VA L'AVOIR NOTRE PAYS!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fileSelect", "Sélectionnez votre variable", "")
    ),
    mainPanel(
      uiOutput("dynamicUI")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  codebookData <- reactiveVal(list())
  
  source_list <- c("ces1965", 
                   "ces1968", 
                   "ces1974", 
                   "ces1979",        
                   "ces1984", 
                   "ces1988",
                   "ces1993", 
                   "ces1997", 
                   "ces2000", 
                   "ces2004", 
                   "ces2006", 
                   "ces2008", 
                   "ces2011", 
                   "ces2015", 
                   "ces2019", 
                   "ces2021", 
                   "datagotchi_pilot1_2021", 
                   "omnibusjanuary", 
                   "omnibusfebruary", 
                   "omnibusmarch", 
                   "omnibusapril", 
                   "omnibusmay", 
                   "omnibusjune", 
                   "datagotchi_pilot2_2022", 
                   "sondage_nationalisme_2022", 
                   "quorum_mcq", 
                   "pes_2022", 
                   "pco_survey")

  observe({
    json_files <- list.files(path = getwd(), pattern = "*.json", full.names = FALSE)
    updateSelectInput(session, "fileSelect", choices = json_files)
  })
  
  observeEvent(input$fileSelect, {
    req(input$fileSelect)
    selected_file <- file.path(getwd(), input$fileSelect)
    json_data <- fromJSON(selected_file)
    codebookData(json_data)
  })
  
  output$dynamicUI <- renderUI({
    json_data <- codebookData()
    
    # Initialize UI elements list
    ui_elements <- list()
    
    # Add description as title
    ui_elements[[length(ui_elements) + 1]] <- HTML(paste("<h2>", json_data$description, "</h2>"))
    
    # Loop through each source_id and generate UI elements
    for (id in names(json_data$source_id)) {
      # Use a local function to capture the current value of id
      makeUIElement <- function(id) {
        source_data <- json_data$source_id[[id]]
        
        # Add source_id as title, question_label as subtitle, and class as subsubtitle
        ui_elements[[length(ui_elements) + 1]] <- HTML(paste("<h3>", id,
                                                             "</h3><h4>", source_data$question_label,
                                                             "</h4><h4>", source_data$raw_variable,
                                                             "</h4><h5>Class: ", source_data$class, "</h5>"))
        
        # Create a table for choices
        choices_df <- as.data.frame(source_data$choices)
        table_id <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", id))
        ui_elements[[length(ui_elements) + 1]] <- tableOutput(table_id)
        
        # Render the table
        output[[table_id]] <- renderTable({
          choices_df
        })
        
        return(ui_elements)
      }
      ui_elements <- makeUIElement(id)
    }
    
    # Return the list of UI elements
    do.call(tagList, ui_elements)
  })
}

# Run the Application
shinyApp(ui, server)
