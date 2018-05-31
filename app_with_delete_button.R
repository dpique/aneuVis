library(shiny)
library(shinyjs)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
#library(shinycustomloader)
library(RColorBrewer)
require(openxlsx)


retFishDf <- function(fish_name, fish_datapath){
  path_list <- as.list(fish_name)#as.list(input$fish_files$name)
  tbl_list <- lapply(fish_datapath, read_excel) #lapply(input$fish_files$datapath, read_excel)
  
  f1 <- map2(.x = path_list, .y= tbl_list,
             .f = ~data.frame(category=.x, .y) %>% clean_names) %>% #) %>%
    #rename_at(vars(names(.)), ~ unlist(regmatches(., gregexpr("Y|X|[[:digit:]]+", .))))) %>%
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    clean_names() %>%
    mutate(smpl = paste0(1:n(), ";",category)) %>%
    mutate(category = as.character(category)) %>%
    gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
    mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr)))) %>%
    mutate(chr = factor(chr, levels=c(1:22, "X", "Y"))) %>%
    mutate(file_type = "fish") %>%
    mutate(category = tools::file_path_sans_ext(category)) %>%
    .[ , order(names(.))] 
  return(f1)
}
#path_list <- as.list(rv$fish_name)#as.list(input$fish_files$name)
#tbl_list <- lapply(rv$fish_datapath, read_excel) #lapply(input$fish_files$datapath, read_excel)

#mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr))))
#df
#tbl_list[[1]] %>% clean_names %>% rename_at(vars(names(.)), ~ unlist(regmatches(., gregexpr("Y|X|[[:digit:]]+", .))))# %>% 





ui <- fluidPage(
  useShinyjs(),
  fileInput(
    inputId = "fish_files", #files
    label = ".xlsx or .xls", #, see file structure guide below",
    multiple = TRUE,
    accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
  ),
  actionButton("submit_fish", "Submit and Go to Table Summary"),
  actionButton('reset_fish', 'Reset Input'),
  
  tableOutput('tbl')
)

resetFileUI <- function(id){
  ns <- NS(id)
  tagList(actionButton("submit_file", "Submit and Go to Table Summary"),
  actionButton(ns('reset_file'), 'Reset Input'))
}

resetFileUI <- function(id){
  ns <- NS(id)
  actionButton(ns('reset_file'), 'Reset Input')
}


server <- function(input, output, session) {
  
  rv <- reactiveValues(f1 = NULL)
  
  observe({
    req(input$fish_files)
    rv$f1 <- retFishDf(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
  })
  
  observeEvent(input$reset_fish, {
    rv$f1 <- NULL
    reset('fish_files')
  })
  
  output$tbl <- renderTable({
    rv$f1
  })
}

shinyApp(ui, server)