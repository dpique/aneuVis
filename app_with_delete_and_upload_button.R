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
#retSkyDf(sky_name = input$sky_file$name, sky_datapath = input$sky_file$datapath)
#sky_datapath = "/Users/dpique/Downloads/test_sky_data (1).xlsx"
retSkyDf <- function(sky_datapath){
  s1 <- read_excel(sky_datapath) %>% 
    clean_names() %>%
    filter(rowSums(is.na(.)) <= .50*ncol(.)) %>% #remove rows where > 50% of values are na
    filter(.[,1] != "Chr. No.") %>%
    set_names(nm=.[1,]) %>%
    .[-1,] %>%
    clean_names()
  
  category_s1 <- s1 %>% filter(.[,1] == "Category") %>% 
    gather(smpl, category, 2:ncol(.)) %>%
    select(-cell)
  
  s2 <- s1 %>% 
    filter(.[,1] != "Category") %>%
    mutate_at(vars(starts_with("x")), 
              .funs = funs(ifelse(str_detect(., ","), 
                                  str_split_fixed(., ",", n=2)[1,1], .))) %>%
    mutate_at(vars(starts_with("x")), .funs = as.numeric) %>%
    gather(key = smpl, value = num_chr, 2:ncol(.)) %>%
    rename(chr = "cell") %>%
    mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr)))) %>%
    left_join(category_s1, by="smpl") %>%
    mutate(file_type = "sky") %>%
    mutate(chr = factor(chr, levels=c(1:22, "X", "Y"))) %>%
    .[ , order(names(.))]
  return(s2)
}

retWgsDf <- function(wgs_datapath, wgs_key_datapath){
  path_list <- as.list(wgs_datapath)
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  tbl_list <- lapply(path_list, read_delim, delim="\t")
  
  g <- map2(.x = path_list, .y= tbl_list,
            .f = ~data.frame(category=.x, .y)) %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    .[,colSums(!is.na(.)) > 0] %>%
    select(-category)
  print(g)
  gK <- read_excel(path = wgs_key_datapath[1], sheet = 1) #%>%

  g2 <- g %>% 
    gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
    group_by(CHR, smpl) %>% 
    mutate(bin_size = END - START) %>%
    summarise(num_chr = round(weighted.mean(x = cp_nm, w = bin_size))) %>% 
    separate(CHR, c("chrRm", "chr"), sep=3) %>% 
    dplyr::select(-chrRm) %>% 
    mutate(chr = factor(chr, levels=c(1:22, "X", "Y")))%>% 
    left_join(gK, by=c("smpl" = "smpl_id")) %>%
    mutate(file_type = "sc-wgs") %>%
    .[ , order(names(.))] 
  return(g2)
}

#path_list <- as.list(rv$fish_name)#as.list(input$fish_files$name)
#tbl_list <- lapply(rv$fish_datapath, read_excel) #lapply(input$fish_files$datapath, read_excel)

#mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr))))
#df
#tbl_list[[1]] %>% clean_names %>% rename_at(vars(names(.)), ~ unlist(regmatches(., gregexpr("Y|X|[[:digit:]]+", .))))# %>% 

resetFileUI <- function(id){
  ns <- NS(id)
  tagList(actionButton("submit_file", "Submit and Go to Table Summary"),
          actionButton(ns('reset_file'), 'Reset Input'))
}

resetFileUI <- function(id){
  ns <- NS(id)
  actionButton(ns('reset_file'), 'Reset Input')
}


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
  
  fileInput(
    inputId = "sky_file",
    label = "Upload SKY File (.xls, .xlsx)", #names must match those in gnko
    multiple = FALSE,
    accept = c(".xlsx", ".xls")#c(".csv", ".txt", ".tsv")
  ), 
  actionButton("submit_sky", "Submit and Go to Table Summary"),
  actionButton('reset_sky', 'Reset Sky Input'),

  fileInput(
    inputId = "wgs_file",
    label = "Copy Number File (.txt)", #names must match those in gnko
    multiple = FALSE,
    accept = ".txt"), 
  fileInput(
    inputId = "wgs_key",
    label = "Copy Number Key (.xls or .xlsx)", #names must match those in gnko
    multiple = FALSE,
    accept = c(".xlsx", ".xls")#, ".csv", ".txt", ".tsv")
  ),
  actionButton("submit_wgs", "Submit and Go to Table Summary"),
  actionButton('reset_wgs', 'Reset WGS Input'),
  
  DT::dataTableOutput('sumryStatsTbl'),
  hr(),
  plotOutput("ternPlot")

)


server <- function(input, output, session) {
  
  rv <- reactiveValues(f1 = NULL, s1=NULL, w1=NULL)
   
  observe({ #Event(input$submit_fish, {
    req(input$fish_files)
    rv$f1 <- retFishDf(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
    #rv$s1 <- retSkyDf(sky_datapath = input$sky_file$datapath)
  })
  
  observeEvent(input$reset_fish, {
    rv$f1 <- NULL
    reset('fish_files')
  })
  
  observe({ #Event(input$submit_fish, {
    req(input$sky_file)
    rv$s1 <- retSkyDf(sky_datapath = input$sky_file$datapath)
  })
  
  observeEvent(input$reset_sky, {
    rv$s1 <- NULL
    reset('sky_file')
  })
  
  #observe({ #Event(input$submit_fish, {
  observeEvent(input$submit_wgs, {
    req(input$wgs_file, input$wgs_key)
    #req(input$wgs_key)
    rv$w1 <- retWgsDf(wgs_datapath = input$wgs_file$datapath, wgs_key_datapath = input$wgs_key$datapath)
  })
  
  observeEvent(input$reset_wgs, {
    rv$w1 <- NULL
    reset('wgs_file')
    reset('wgs_key')
  })
  
  
  stsTbl <- eventReactive({input$submit_fish | input$submit_sky | input$reset_sky | input$reset_fish |
      input$reset_wgs | input$submit_wgs}, {
        
        validate(
          need(!is.null(rv$w1) | !is.null(rv$f1) | !is.null(rv$s1), "Please upload at least 1 file!")
        )     
        
        numX = 2 #as.numeric(numbX()) #input$numberOfX)
        numY = 0 #as.numeric(numbY()) #input$numberOfY)
        list_to_pass <- list(rv$s1, rv$f1, rv$w1) %>% purrr::compact() #2018-05-05 issue here?
        aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score, numX=numX, numY=numY)
        
        print(aneupl_scores)
        heterog_scores = purrr::map_df(.x = list_to_pass, .f = calc_heterog_score)
        anca_scores_normalized = purrr::map_df(.x = list_to_pass, .f = calc_anca_score_normalized, numX=numX, numY=numY)
        anca_scores = purrr::map_df(.x = list_to_pass, .f = calc_anca_score, numX=numX, numY=numY)
        instab_idx = purrr::map_df(.x = list_to_pass, .f = calc_instab_idx)
        perc_ploidy <- purrr::map_df(.x = list_to_pass, .f = calc_perc_ploidy, numX=numX, numY=numY)
        sumStats <- purrr::reduce(list(aneupl_scores, heterog_scores, anca_scores_normalized, anca_scores, instab_idx, perc_ploidy), full_join, by=c("category", "file_type")) %>%
          select(category, file_type, n, everything())
        return(sumStats)
      })
  #}
  
    #observeEvent(input$submit_fish, {
      output$sumryStatsTbl <- DT::renderDataTable({
        
        #validate(
        #  need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$wgs_file), 'Please upload at least 1 file!')
        #) 
        DT::datatable(stsTbl(),       
                      filter = list(position = 'top', clear = FALSE),
                      options = list(
                        search = list(regex = TRUE, caseInsensitive = FALSE))) %>% DT::formatRound(c(4:11), 2)
      })
      
      output$ternPlot <- renderPlot({
        p <- ggtern() + 
          geom_point(data=stsTbl(), 
                     aes(x = aneuploid,y=diploid,z=polyploid,
                         color = category, shape= file_type),
                     #fill = paste0(file_type, ": ",category)),#, label=file_type), 
                     size = 3, alpha = 0.8) + #,  color = "black") +  #pch= 21, #, stroke = 1
          xlab("") + ylab("") +
          Tlab("Diploid") +
          Llab("Aneuploid") +
          Rlab("Polyploid") +
          guides(fill=guide_legend(title="Legend")) +
          limit_tern(1.03,1.03,1.03) 
        print(p)
        #return(p)
        #NULL
      })
    #})

}

shinyApp(ui, server)