library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)

source("scripts/helper_scripts.R")




max_plots <- 50 # *maximum* total number of plots

ui <- navbarPage(
  title = "aneuvis",
  theme = shinythemes::shinytheme("spacelab"), #shinythemes::themeSelector(), #, #shinyshinytheme("united"),
  tabPanel("Home",
           p("Aneuvis is a web tool for analyzing chromosomal number variation in single cells."),
           p("The three types of single-cell chromosomal data that can be uploaded into aneuvis are"),
             tags$ol(
               tags$li(tags$a(target = "_blank", 
                              href = "https://www.nature.com/scitable/topicpage/fluorescence-in-situ-hybridization-fish-327", 
                              "Fluorescence in situ hybridization (FISH)"), "- analyze chromosomal counts from 2 to 4 chromosomes."), 
               tags$li(tags$a(target = "_blank", 
                               href = "https://www.annualreviews.org/doi/abs/10.1146/annurev-genom-090413-025352", 
                               "Single cell whole genome sequencing (SC-WGS)"), "- analyze chromosome counts from single cell sequencing data."), 
               tags$li(tags$a(target = "_blank", 
                              href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254861/", 
                              "Spectral karyotyping (SKY)"), "- analyze chromosome counts and structural variation from all chromosomes.")
             ),
           p("The output from aneuvis is divided into 3 parts: Visualization, Table summary, and Comparative statistics"),
           p("Below we have a video tutorial for how to use aneuvis.")),
  tabPanel("Documentation",
           #sidebarLayout( #shinythemes::themeSelector(),
           #theme = shinythemes::shinytheme("spacelab"),
           #titlePanel("aneuvis v.0.4"),
           #tabPanel(
           radioButtons(
             inputId = "rb",
             label = "1. Select data type:",
             c(
               "2, 3, or 4-chromosome FISH" = "fish2",
               "Ginkgo: Single-cell CNV" = "ginkgo",
               "SKY" = "sky"
             )
           )
  ),
  navbarMenu("Upload Data",
             tabPanel("FISH",
                      h3("Upload fluorescence in situ hybridization (FISH) data"),
                      fileInput(
                                inputId = "fish_files", #files
                                label = ".xlsx or .xls", #, see file structure guide below",
                                multiple = TRUE,
                                accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
                              ),
                      actionButton("submit_fish", "Submit"),
                      hr(),
                      # p("The structure of an Excel file containing FISH data is shown below."),
                      h3("FISH file structure guide"),
                      
                      img(src="fish_layout_excel.png", width=300),
                      p("Multiple excel files, each with the same # of chromosomes, can be uploaded. Each file will be treated as a separate 'condition'."),
                      p("Download example 2-chromosome FISH data ", 
                        tags$a(target = "_blank", 
                               href = "https://docs.google.com/uc?export=download&id=1CKh6feR7AmndtAvoF4Y-EFxaigjiFmba", "here"), 
                        " (zip file)")
             ),
             
                        #p("Example FISH data is available for download here")),
                      #),
             tabPanel("SC-WGS",
                      h3("Upload single cell whole genome sequencing (sc-wgs) data"),
                            fileInput(
                              inputId = "gnk_file",
                              label = "Copy Number File (.txt)", #names must match those in gnko
                              multiple = FALSE,
                              accept = ".txt"), 
                      fileInput(
                              inputId = "gnk_key",
                              label = "Copy Number Key (.xls or .xlsx)", #names must match those in gnko
                              multiple = FALSE,
                              accept = c(".xlsx", ".xls")#, ".csv", ".txt", ".tsv")
                            ),
                      actionButton("submit_wgs", "Submit"),
                      hr(),
                      h3("Copy number and key file structure guide"),
                      
                      #p("The format of a copy number file is shown below."),
                      img(src="ginkgo_layout.png", width=400),
                      p("One Ginkgo copy number file can be uploaded at a time."),
                      p("Download example sc-WGS data (processed using Ginkgo)", 
                        tags$a(target = "_blank", 
                               href="http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441", "here"), 
                        "(save as '.txt' file)"),
                      p("Download example sc-WGS key", 
                        tags$a(target = "_blank",
                               href="https://docs.google.com/uc?export=download&id=1bhOEye8FR3Ut9w_hT-SMab2AaUEx9_5x", "here"))
                      #)
             ),
             tabPanel("SKY",
                      h3("Upload spectral karyotype (SKY) data"),
                      fileInput(
                        inputId = "sky_file",
                        label = "Upload SKY File (.xls, .xlsx)", #names must match those in gnko
                        multiple = FALSE,
                        accept = c(".xlsx", ".xls")#c(".csv", ".txt", ".tsv")
                      ), 
                      actionButton("submit_sky", "Submit"),
                      hr(),
                      p("The format of a sky file is shown below."),
                      img(src="sky_layout.png", width=400),
                      p("Multiple SKY copy number file can be uploaded at a time."),
                      p("Download example SKY data", 
                        tags$a(target = "_blank", 
                               href="http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441", "here")),
                      p("Access a list of ISCN chromosomal abberation symbols", 
                        tags$a(target = "_blank", 
                               href="https://cgap.nci.nih.gov/Chromosomes/ISCNSymbols", "here")))),
 

  tabPanel("Visualizations"),
  tabPanel("Table Summary",
                     p("The table below gives the weighted average copy number rounded to the nearest integer per chromosome per sample."),
                     p("A 'wide' table of this data is available for download, with chromosomes as columns and samples as rows."),
                     downloadButton("g2T.d", "Download"),
                     #tableOutput("g2T"),
          hr(),
           #DT::dataTableOutput("g2T"),
          tableOutput("f1TestTable"),
          tabsetPanel(
            id = 'dataset',
            tabPanel("sc-wgs summary", DT::dataTableOutput("g2T")),
            tabPanel("Stats Per Treatment", DT::dataTableOutput("sumryStatsTbl")),
            tabPanel("Stats Per Treatment & Chromosome", DT::dataTableOutput("sumryStatsTblPerChr"))
          )),
          
          #h2("Summary Stats per Treatment"),
          #tableOutput("sumryStatsTbl"),
          #h2("Summary Stats per Treatment and Chromosome"),
          #tableOutput("sumryStatsTblPerChr")),
  navbarMenu("Compare",
             tabPanel("Treatments"),
             tabPanel("Platform"))
)
#    conditionalPanel(
#      condition = "input.rb == 'fish2'",
#      fileInput(
#        inputId = "fish_files", #files
#        label = "2. Upload FISH data (.xlsx or .xls)",
#        multiple = TRUE,
#        accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
#      )),
#    
#    conditionalPanel(
#      condition = "input.rb == 'ginkgo'",
#      fileInput(
#        inputId = "gnk_file",
#        label = "2a. Upload Ginkgo Copy Number File (.txt)", #names must match those in gnko
#        multiple = FALSE,
#        accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
#      ), fileInput(
#        inputId = "gnk_key",
#        label = "2b. Upload Ginkgo Key (xls or xlsx)", #names must match those in gnko
#        multiple = FALSE,
#        accept = c(".xlsx", ".xls")#, ".csv", ".txt", ".tsv")
#      )),
#    conditionalPanel(
#      condition = "input.rb == 'sky'",
#      fileInput(
#        inputId = "sky_file", #files
#        label = "2. Upload Sky data (.xlsx or .xls)",
#        multiple = FALSE,
#        accept = c(".xlsx", ".xls")
#      )),
#    actionButton("submit", "Submit"),
#
#    p("Download example 2-chromosome FISH data ", 
#      tags$a(target = "_blank", 
#             href = "https://docs.google.com/uc?export=download&id=1CKh6feR7AmndtAvoF4Y-EFxaigjiFmba", "here"), 
#      " (zip file)"),
#    p("Download example Ginkgo data", 
#      tags$a(target = "_blank", 
#             href="http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441", "here"), 
#      "(save as '.txt' file)"),
#    p("Download example Ginkgo key", 
#      tags$a(target = "_blank",
#             href="https://docs.google.com/uc?export=download&id=1bhOEye8FR3Ut9w_hT-SMab2AaUEx9_5x", "here"))
#  )
#  )
  #mainPanel(),
  
  
##  mainPanel(
##    conditionalPanel(condition = "input.rb == 'fish2'",
##                     tabPanelFishMaster()),
##    conditionalPanel(condition = "input.rb == 'ginkgo'",
##                     tabsetPanel(
##                       tabPanel("Chromosomal Copy Number Heat Map",
##                                plotOutput("g4P")),
##                       tabPanel("Sample x Chromosome Summary Table", 
##                                p("The table below gives the weighted average copy number per chromosome per sample.
##                                  The table is also available for download."),
##                                downloadButton("g2T.d", "Download"),
##                                tableOutput("g2T")),
##                       tabPanel("Summary statistics",
##                        p("ginkgo_table:"),
##                        downloadButton("g10T.d", "Download"),
##                         tableOutput("gnk_score_tbl"))
##                      )),
##    conditionalPanel(condition = "input.rb == 'sky'",
##                     tabsetPanel(
##                       tabPanel("Plots",
##                                plotOutput("skyPlotChr")),
##                       tabPanel("Data Table",
##                                tableOutput("s1R.t")),
##                       tabPanel("Summary Statistics",
##                                tableOutput("s7T"))
##                     ))
##    )
#)



server <- shinyServer(function(input, output) {
  ###########
  #1. Read in raw ginkgo data
   gR <- eventReactive(input$submit_wgs, ignoreNULL = FALSE, { #var. named with capital R for Reactive
     #sprintf("hello3")
     #validate(need(input$gnk_file != "", "..."))
     #print("hello2")
    if (is.null(input$gnk_file)) {
      return(NULL)
    }
      
      path_list <- as.list(input$gnk_file$datapath)
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(path_list, read_delim, delim="\t")
      
      g <- map2(.x = path_list, .y= tbl_list,
                      .f = ~data.frame(category=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        .[,colSums(!is.na(.)) > 0] %>%
        select(-category)

      return(g)
    })

   #2. read in the ginkgo key 
   
   gKR <- reactive({#eventReactive(input$submit, { #g for ginkgo K for Key, R for reactive
     #validate(need(input$gnk_key != "", "..."))
     
     if (is.null(input$gnk_key)) {
       return(NULL)
     }
     gK <- read_xlsx(path = input$gnk_key$datapath[1], sheet = 1) #%>%
       #mutate(category = paste0(category, "__sc-wgs"))
     
     #print("head(gk):")
     #print(head(gK))
     return(gK)
   })
   
   
  g2R <- reactive({
    if (is.null(gR())) {
      return(NULL)
    }
    g2 <- gR() %>% 
      gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
      group_by(CHR, smpl) %>% 
      mutate(bin_size = END - START) %>%
      summarise(num_chr = round(weighted.mean(x = cp_nm, w = bin_size))) %>% 
      separate(CHR, c("chrRm", "chr"), sep=3) %>% 
      dplyr::select(-chrRm) %>% 
      filter(chr != "Y") %>% 
      mutate(chr = factor(chr, levels=c(1:22, "X")))%>% 
      left_join(gKR(), c("smpl" = "file_name")) %>%
      mutate(file_type = "sc-wgs") %>%
      .[ , order(names(.))] 
    return(g2)
  })
  
  g2.1R <- reactive({
    if (is.null(gR())) {
      return(NULL)
    }
    g2.t <- g2R() %>% #select(-avgRound) %>%
      spread(key = chr, value = num_chr)
    return(g2.t)
  })
  
  output$g2T.d <- downloadHandler(
    filename = function() {
      paste("ginkgo-chr-summary-",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(g2.1R(), file, row.names = FALSE)
    }
  )

  output$g2T <- DT::renderDataTable({
    if (is.null(g2R())) {
      return(NULL)
    }
    DT::datatable(g2R())
  })
  
  f1R <- eventReactive(input$submit_fish, ignoreNULL = FALSE, {
    #validate(need(input$fish_files != "", "..."))
    
    #if (is.null(input$fish_files)) {
    if (!is.data.frame(input$fish_files)) {
      return(NULL)
    }
    
    maxChr = 8
    maxChrPlus1 = maxChr + 1
    
    path_list <- as.list(input$fish_files$name)
    tbl_list <- lapply(input$fish_files$datapath, read_xlsx)
    
    f1 <-  map2(.x = path_list, .y= tbl_list,
                .f = ~data.frame(category=.x, .y)) %>% 
      do.call(rbind, .) %>% 
      as_tibble() %>% 
      clean_names() %>%
      mutate(smpl = paste0(1:n(), ";",category)) %>%
      mutate(category = as.character(category)) %>%
      gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
      mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr)))) %>%
      mutate(chr = factor(chr, levels=c(1:22, "X"))) %>%
      mutate(file_type = "fish") %>%
      .[ , order(names(.))] 
    
     return(f1)
  })
  
  f1TestTable_r <- reactive({
    if (is.null(f1R())) {
      return(NULL)
    }
  calc_aneupl_score(f1R())
    #print(calc_aneupl_score(f1R()))
    #print(calc_aneupl_score(g2R()))
  #list_to_pass <- list(g2R(), s2R(), f1R()) %>% compact()
   # print("length(list_to_pass):")
   # print(length(list_to_pass))
   # aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score)
   # head(aneupl_scores)
  })
  

  
  s1R <- eventReactive(input$submit_sky, ignoreNULL = FALSE, { #reactive({
    #validate(need(input$sky_file != "", "..."))
    print("class(input$sky_file)")
    print(class(input$sky_file))
    if (is.null(input$sky_file)) {
      return(NULL)
    }
    
    s1 <- read_xlsx(input$sky_file$datapath) %>% 
      clean_names() %>%
      filter(rowSums(is.na(.)) <= .50*ncol(.)) %>% #remove rows where > 50% of values are na
      filter(.[,1] != "Chr. No.") %>%
      set_names(nm=.[1,]) %>%
      .[-1,] %>%
      clean_names()
    
    #sample_name <- read_xlsx(input$sky_file$datapath) %>% names(.)[2]
    #print(sample_name)
    
    return(s1)
    
  })
  
  s2R <- reactive({
    if (is.null(s1R())) {
      return(NULL)
    }
    category_s1 <- s1R() %>% filter(.[,1] == "Source") %>% 
      gather(smpl, category, 2:ncol(.)) %>%
      select(-cell)
    
    s2 <- s1R() %>% 
      filter(.[,1] != "Source") %>%
      mutate_at(vars(starts_with("x")), 
                .funs = funs(ifelse(str_detect(., ","), 
                                    str_split_fixed(., ",", n=2)[1,1], .))) %>%
      mutate_at(vars(starts_with("x")), .funs = as.numeric) %>%
      gather(key = smpl, value = num_chr, 2:ncol(.)) %>%
      rename(chr = "cell") %>%
      mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr)))) %>%
      left_join(category_s1, by="smpl") %>%
      mutate(file_type = "sky") %>%
      mutate(chr = factor(chr, levels=c(1:22, "X"))) %>%
      .[ , order(names(.))]
    #print(head(s2))
    #print(head(calc_anca_score(s2)))
    #print(list(g2R(), s2, f1R()) %>% compact())
    return(s2)
  })
  
 # test <- reactive({
 #   print(g2R())
 # })
  
  output$f1TestTable <- renderTable({
   #tagList(head(s2R()), 
   #        head(g2R()))
   #        head(f1R()))
    #head(calc_anca_score(s2R()))
    #print(head(s2R()))
    #list_to_pass <- list(g2R(), s2R(), f1R()) %>% compact()
    #
    #tagList(
    #  print(length(list_to_pass)),
    #  head(calc_anca_score(s2R()))
    #)
  })
  

  output$sumryStatsTbl <- DT::renderDataTable({
    list_to_pass <- list(g2R(), s2R(), f1R()) %>% purrr::compact() #2018-05-05 issue here?
    #print("class(s2R())")
    #print(class(s2R()))
    #return(DT::datatable(bind_rows(f1R())))
    
    #return(DT::datatable(bind_rows(f1R(), s2R(), g2R())))
    #list_to_pass <- Filter(function(x) dim(x)[1] > 0, list(g2R(), s2R(), f1R()) )
    print("length(list_to_pass):")
    print(length(list_to_pass))
    print(lapply(list_to_pass, head))
    
    aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score)
    print(aneupl_scores)
    heterog_scores = purrr::map_df(.x = list_to_pass, .f = calc_heterog_score)
    anca_scores = purrr::map_df(.x = list_to_pass, .f = calc_anca_score)
    sumStats <- purrr::reduce(list(anca_scores, aneupl_scores,heterog_scores), full_join, by=c("category", "file_type")) 
    print("head(sumStats)")
    print(head(sumStats))
    return(DT::datatable(sumStats))
  })
  
  
  stsTblPerChr <- reactive({
    list_to_pass <- list(g2R(), s2R(), f1R()) %>% compact()
    aneupl_scores = purrr:::map_df(.x = list_to_pass, .f = calc_aneupl_score, retChr = TRUE)
    heterog_scores = purrr:::map_df(.x = list_to_pass, .f = calc_heterog_score, retChr = TRUE)
    anca_scores = purrr:::map_df(.x = list_to_pass, .f = calc_anca_score, retChr = TRUE)
    sumStats <- purrr::reduce(list(anca_scores, aneupl_scores,heterog_scores), full_join, by=c("category","file_type", "chr")) 
    print("head(sumStats), retChr=TRUE")
    print(head(sumStats))
    return(sumStats)
  })
  
  output$sumryStatsTblPerChr <- DT::renderDataTable({
    #list_to_pass <- list(g2R(), s2R(), f1R()) %>% compact()
    #aneupl_scores = purrr:::map_df(.x = list_to_pass, .f = calc_aneupl_score, retChr = TRUE)
    #heterog_scores = purrr:::map_df(.x = list_to_pass, .f = calc_heterog_score, retChr = TRUE)
    #anca_scores = purrr:::map_df(.x = list_to_pass, .f = calc_anca_score, retChr = TRUE)
    #sumStats <- purrr::reduce(list(anca_scores, aneupl_scores,heterog_scores), full_join, by=c("category","file_type", "chr")) 
    DT::datatable(stsTblPerChr(),       
                         filter = list(position = 'top', clear = FALSE),
                         options = list(
                           search = list(regex = TRUE, caseInsensitive = FALSE)))
  })
  
 #output$sumryStatsTblPerChr = downloadHandler('sumStatsTblPerChr-filt.csv', content = function(file) {
 #  s = input$sumryStatsTblPerChr_rows_all
 #  write.csv(stsTblPerChr()[s, , drop = FALSE], file)
 #})
  
  
  

})


shinyApp(ui, server)
#app <- shinyApp(ui, server)
#runApp(app, display.mode = "showcase")