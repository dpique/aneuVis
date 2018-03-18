library(shiny)
library(readxl)
library(data.table)

source("scripts/helper_scripts.R")

ui <- fluidPage(
  titlePanel("aneuvis v.0.1"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      
      fileInput("file1", "Choose Excel File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           "application/vnd.ms-excel",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      tags$hr(),
      textInput(inputId = "titletxt", label = "Insert main title here", value = "title"),
      textInput(inputId = "xlab", label = "x-axis title:", value = "Chr_column_1"),
      textInput(inputId = "ylab", label = "y-axis title:", value = "Chr_column_2"),
      
      sliderInput("minChr", "Smallest Chr #:",
                  min = 0, max = 10,
                  value = 1),
      sliderInput("maxChr", "Largest Chr. #:",
                  min = 2, max = 100,
                  value = 9)
    ), 
    
    mainPanel = mainPanel(
      h3(textOutput("caption")),
      plotOutput("gridPlot"))
  )
)


server <- function(input, output) {
  mycsvs<-reactive({
    rbindlist(lapply(input$csvs$datapath, fread),
              use.names = TRUE, fill = TRUE)
  })
  #print(head(mycsvs))
  output$count <- renderText(c(nrow(mycsvs()), ncol(mycsvs())))
}

shinyApp(ui, server)


##########

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
      fileInput(inputId = "files", label = "Upload", multiple = TRUE, accept = c(".xlsx")),
      textOutput(outputId = "name_out"),
      verbatimTextOutput(outputId = "ploidyTbl")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      
      dataTableOutput("tbl_out"),
      uiOutput("plots"),
      textOutput("tbl_out2")
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
  
  lst2 <- reactive({
      #path_list <- as.list(input$files$datapath)
      name_list <- paste0(input$files$name, collapse = ", ")
      
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      #tbl_list <- lapply(input$files$datapath, read_xlsx)
      #df <- do.call(rbind, tbl_list)
      return(name_list)
    })
  
  output$tbl_out <- renderDataTable({
    aneuDat_r()
  })
  output$tbl_out2 <- renderPrint({
    letter_ids = names(table(aneuDat_r()[,1]))
    return(list(letter_ids, colnames(aneuDat_r())))
  })
  output$name_out <- renderText({
    lst2()
  })
  output$ploidyTbl <- renderPrint({
    table(aneuDat_r()$ploidy, aneuDat_r()$class) #returns html
  })
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(unique(aneuDat_r()$class)), function(i) {
    plotname <- paste("plot", i, sep="")
    plotOutput(plotname, height = 280, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  max_plots <- isolate(length(unique(aneuDat_r()$class)))
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        letter_ids <- names(table(aneuDat_r()$class))      
        
        matr_list_all <- lapply(letter_ids, function(x)
          return_chr_prop_matr(aneuDat_r(),x, maxPair = maxChrPlus1)
        )
        names(matr_list_all) = letter_ids
        gridPlots <- matr_list_all[my_i] %>% 
          map2(.y=names(.),.f=~create_perc_matr2(matr = .x, 
                                                 title = .y, minChr = 1, maxChr = maxChrPlus1, 
                                                 xlab = "", ylab=""))
        return(gridPlots)
      })
    })
  }
}
#shiny solution to the reset button
#could also use shinyjs for this (likely)
#https://groups.google.com/forum/#!topic/shiny-discuss/HbTa4v612FA

# Create Shiny app ----
shinyApp(ui, server)


