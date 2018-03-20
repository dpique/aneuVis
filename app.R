
library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
source("scripts/helper_scripts.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("aneuvis v.0.2"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      fileInput(inputId = "files", label = "Upload", multiple = TRUE, accept = c(".xlsx"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      dataTableOutput("tbl_out")
      #plotOutput("plot_out")
      #verbatimTextOutput("plot_out")
      #uiOutput("plots")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  aneuDat_r <- reactive({
    validate(need(input$files != "", "aneupl prop..."))
    
    if (is.null(input$files)) {
      return(NULL)
    } else {
      
      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      path_list <- as.list(input$files$datapath)
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(input$files$datapath, read_xlsx)
      
      aneuDat <- map2(.x = input$files$name, .y= tbl_list, .f = ~data.frame(class=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        clean_names() %>%
        mutate(ploidy =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
        mutate_at(.vars = vars(starts_with("Chr")), 
                  .funs = ~ifelse(. == 0, 1, 
                                  ifelse(. <= maxChr, ., maxChrPlus1)))
      return(aneuDat)
    }
  })
  
  output$tbl_out <- renderDataTable({
    #aneuDat_r()
    maxChr = 8
    maxChrPlus1 = maxChr + 1
    letter_ids <- names(table(aneuDat_r()$class))
    matr_list_all <- lapply(letter_ids, function(x)
      return_chr_prop_matr(aneuDat_r(),x, maxPair = maxChrPlus1)
    )
    names(matr_list_all) = letter_ids
    #r#eturn(data.frame(matr_list_all[[1]]))
     matr_list_all3 <- map_df(.x=matr_list_all,.f = I, .id="src")
    # create_perc_matr3(matr_list_all2, title = "", minChr = 1, 
                       ##                        maxChr = maxChrPlus1, xlab = "", ylab="")
                       
    return(matr_list_all3)
  })
  
##  output$plot_out <- renderPrint({ #renderPlot({
##      maxChr = 8
##      maxChrPlus1 = maxChr + 1
##      
##      letter_ids <- names(table(aneuDat_r()$class))
##      #return(letter_ids)
##      matr_list_all <- lapply(letter_ids, function(x)
##        return_chr_prop_matr(aneuDat_r(),x, maxPair = maxChrPlus1)
##      )
##      names(matr_list_all) = letter_ids
##      #matr_list_all2 <- do.call(rbind,matr_list_all) %>%
##      #  map_df(.f = I, .id="src")
##      #matr_list_all_test3 <- map_df(.x = matr_list_all_test, .f = I, .id="src")
##      plt <- create_perc_matr3(matr_list_all2, title = "", minChr = 1, 
##                        maxChr = maxChrPlus1, xlab = "", ylab="")
##      return(letter_ids)
##    })
}
#shiny solution to the reset button
#could also use shinyjs for this (likely)
#https://groups.google.com/forum/#!topic/shiny-discuss/HbTa4v612FA

# Create Shiny app ----
shinyApp(ui, server)

