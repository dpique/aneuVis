library(shiny)
#library(shinyjs) 
library(readxl)
library(tidyverse)
library(here)
library(janitor)
source("scripts/helper_scripts.R")

#can calculate a chromosomal instability index



ui <- fluidPage(
  useShinyjs(),
  fileInput('inFile', 'Choose file', multiple = TRUE),
  actionButton('reset', 'Reset'),
  tableOutput('tbl')
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL)
  
  observe({
    req(input$inFile)
    rv$data <- read_xlsx(input$inFile$datapath)
  })
  
  observeEvent(input$reset, {
    rv$data <- NULL
    reset('inFile')
  })
  
  output$tbl <- renderTable({
    rv$data
  })
}

shinyApp(ui, server)
