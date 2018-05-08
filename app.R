library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
library(shinycustomloader)

#library(DT)
source("scripts/helper_scripts.R")




max_plots <- 50 # *maximum* total number of plots

ui <- navbarPage(
  title = "aneuvis 0.5",
  theme = shinythemes::shinytheme("spacelab"), #shinythemes::themeSelector(), #, #shinyshinytheme("united"),
  tabPanel("Home",  icon = icon("home"),
           h3("Aneuvis is a web tool for analyzing chromosomal number variation in single cells."),
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
           p("The output from aneuvis is divided into 2 parts: Table Summary and Visualization"),
           p("See the image below for an overview of Aneuvis"),
           h3("Do treatments A and B induce aneuploidy?"),
           img(src="aneuvis_layout.png", width=700),
           p("Watch the tutorial below to get started. (forthcoming)")),
  tabPanel("Documentation", icon=icon("book"),
           #sidebarLayout( #shinythemes::themeSelector(),
           #theme = shinythemes::shinytheme("spacelab"),
           #titlePanel("aneuvis v.0.4"),
           #tabPanel(
           p("Aneuvis is the product of a collaboration between the",tags$a(target = "_blank", 
                                                                          href = "http://www.einstein.yu.edu/faculty/9868/cristina-montagna/", 
                                                                          "Montagna"), "(aneuploidy and cytogenetics) and", tags$a(target = "_blank", 
                                                                                                                                   href = "http://www.einstein.yu.edu/faculty/12990/jessica-mar/", 
                                                                                                                                   "Mar"), "(computational biology) labs at Albert Einstein College of Medicine."),
           
           p("All source code is available on github."),
           p("Aneuvis was created using", tags$a(target = "_blank", 
                                                href = "http://shiny.rstudio.com/", 
                                                "Shiny"), "version 1.0.5 (R version 3.4.3) and is available under a GPLv3 license"),
           p("Please contact daniel.pique@med.einstein.yu.edu with any questions."),
           hr(),
           h3("FAQ")
           
  ),
  tabPanel("Upload Data", icon=icon("upload"),
           tabsetPanel(
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
                      hr(),
                      h3("Download example data"),
                        
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
                      img(src="ginkgo_layout.png", width=500),
                      p("One copy number file can be uploaded at a time."),
                      h4("Layout of the key"),
                      img(src="ginkgo_key.png", width=400),
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
                      h3("SKY file structure guide"),
                      img(src="sky_layout.png", width=550),
                      p("One SKY copy number file should be uploaded at a time."),
                      p("Download example SKY data", 
                        tags$a(target = "_blank", 
                               href="https://docs.google.com/uc?export=download&id=1VO2jwPt8bCgCP87-brYujAZ459OCaFmX", "here")),
                      p("Access a list of International System for Chromosome Nomenclature (ISCN) symbols", 
                        tags$a(target = "_blank", 
                               href="https://cgap.nci.nih.gov/Chromosomes/ISCNSymbols", "here"))))),
 
  tabPanel("Table Summary", icon = icon("table"),
                     #p("The table below gives the weighted average copy number rounded to the nearest integer per chromosome per sample."),
                     #p("A 'wide' table of this data is available for download, with chromosomes as columns and samples as rows."),
                     #downloadButton("g2T.d", "Download"),
                     #tableOutput("g2T"),
          #hr(),
           #DT::dataTableOutput("g2T"),
          #tableOutput("f1TestTable"),
          tabsetPanel(
            id = 'dataset',
            tabPanel("Stats Per Treatment", DT::dataTableOutput("sumryStatsTbl")),
            tabPanel("Stats Per Treatment & Chromosome", DT::dataTableOutput("sumryStatsTblPerChr")),
            tabPanel("sc-wgs summary", DT::dataTableOutput("g2T"),
                     p("A 'wide' table of this data is available for download, with chromosomes as columns and samples as rows. 
                      This table contains the weighted average copy number (by bin size) rounded to the nearest integer per chromosome per sample."),
                     downloadButton("g2T.d", "Download")
            )
          ), 
          hr(),
          p("Each row in this table represents
            a different file that was uploaded. The columns represent the following:",
            
            tags$ul(
              tags$li(
                "Columns labeled diploid, polyploid, and aneuploid represent the proportion of cells 
                in that state per treatment."
              ),
              tags$li(
                
                "The column labeled (n) represents the total number of cells or chromosomes analyzed within the file."
              ),
              tags$li(
                "The average number of copy alterations per group (anca_score) was calculated as in",
                tags$a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/pubmed/12775914", "Blegen et al 2003")
              ),
              tags$li(
                "The aneuploidy and heterogeneity scores were calculated as in",
                tags$a(
                  target = "_blank",
                  href = "https://www.ncbi.nlm.nih.gov/pubmed/27246460",
                  "Bakker et al 2016 (Suppl.Methods & Table S2)"
                )
              )
          ))),
  tabPanel("Visualization", icon = icon("bar-chart-o"), #icon = icon("heatmap"), #
           tabsetPanel(
             tabPanel("FISH", uiOutput("gridPlots")),
             tabPanel("sc-WGS", plotOutput("chrHeatG2")),
             tabPanel("SKY", plotOutput("chrHeatS2")),
             tabPanel("Scores by Group",
                      h3("Scatterplot of Aneuploidy and Heterogeneity Score by Group"),
                      plotOutput("aneuHeteroSctrPlt"),
                      hr(),
                      plotOutput("ternPlot"),
                      hr(),
                      p("Ternary plots are used to represent proportions of 3 groups that sum to 1"),
                      #p("T = Proportion of Diploid cells"),
                      #p("L = Proportion of Aneuploid cells"),
                      #p("R = Proportion of Polyploid cells"),
                      p("Position of each point represents the proportion of cells within each group.
                        For example, a point near 'Diploid' would mean that most of the cells within that group
                        are diploid.")),
             tabPanel("Scores by Group & Chromosome",
                      plotOutput("aneuHeteroSctrPltPerChr")
             ),
             tabPanel("Permutations",
                      h3("Are the groups that we see statistically significantly different from each other
                        in terms of the degree of numerical aneuploidy?"),
                      p("Methods: 250 random permutations of the category associated with each observed cell. 
                        The difference in ANCA scores between all possible pairs of categories is calculated after each permutation. 
                        A p-value is calculated by counting how many permuted ANCA scores are more extreme than
                        the observed ANCA score. The p-values is 2-sided, and there are three possible interpretations of the resulting p-value:
                        Significantly similar (blue color), not significantly different (p > 0.05, grey color), significantly different (red color).
                        If you're only looking for differences between groups, 
                        then you should only focus on whether the groups are significantly *different* or not (red color)"),
                      withLoader(tableOutput("permTableg2R"), type="html", loader="dnaspin"),
                      withLoader(plotOutput("permPlotg2R"), type="html", loader="dnaspin"),
                      #tableOutput("permTableg2R"),
                      #plotOutput("permPlotg2R"), 
                      hr(),
                      withLoader(tableOutput("permTables2R"), type="html", loader="dnaspin"),
                      withLoader(tableOutput("permPlots2R"), type="html", loader="dnaspin"),
                      #tableOutput("permTables2R"), plotOutput("permPlots2R"),
                      hr(),
                      #withLoader(tableOutput("permTablef1R"), type="html", loader="dnaspin"),
                      #withLoader(tableOutput("permPlotf1R"), type="html", loader="dnaspin"))
                      
                      tableOutput("permTablef1R"), plotOutput("permPlotf1R") ) 
           ))
          #h2("Summary Stats per Treatment"),
          #tableOutput("sumryStatsTbl"),
          #h2("Summary Stats per Treatment and Chromosome"),
          #tableOutput("sumryStatsTblPerChr")),
  #navbarMenu("Compare",
  #           tabPanel("Treatments", "This page is currently blank. Let me know if we should reorganize some of the 
  #                    plots from the Visualizations tab to this page. We can also delete this tab. "),
  #           tabPanel("Platform", "This page is currently blank. Let me know if we should reorganize some of the 
  #                    plots from the Visualizations tab to this page. We can also delete this tab."))
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
      left_join(gKR(), by=c("smpl" = "smpl_id")) %>%
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
    category_s1 <- s1R() %>% filter(.[,1] == "Category") %>% 
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
  

  stsTbl <- reactive({
    list_to_pass <- list(g2R(), s2R(), f1R()) %>% purrr::compact() #2018-05-05 issue here?
    aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score)
    print(aneupl_scores)
    heterog_scores = purrr::map_df(.x = list_to_pass, .f = calc_heterog_score)
    anca_scores = purrr::map_df(.x = list_to_pass, .f = calc_anca_score)
    perc_ploidy <- purrr::map_df(.x = list_to_pass, .f = calc_perc_ploidy)
    sumStats <- purrr::reduce(list(anca_scores, aneupl_scores,heterog_scores, perc_ploidy), full_join, by=c("category", "file_type")) 
    return(sumStats)
  })
  
  output$sumryStatsTbl <- DT::renderDataTable({
    
    validate(
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$gnk_file), 'Please upload at least 1 file!')
    ) 
    #DT::datatable() 
    DT::datatable(stsTbl(),       
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE))) %>% DT::formatRound(c(2, 4:5, 7:9), 2)
  })
  
  stsTblPerChr <- reactive({
    list_to_pass <- list(g2R(), s2R(), f1R()) %>% compact()
    aneupl_scores = purrr:::map_df(.x = list_to_pass, .f = calc_aneupl_score, retChr = TRUE)
    heterog_scores = purrr:::map_df(.x = list_to_pass, .f = calc_heterog_score, retChr = TRUE)
    anca_scores = purrr:::map_df(.x = list_to_pass, .f = calc_anca_score, retChr = TRUE)
    sumStats <- purrr::reduce(list(anca_scores, aneupl_scores,heterog_scores), full_join, by=c("category","file_type", "chr")) 
    return(sumStats)
  })
  
  output$sumryStatsTblPerChr <- DT::renderDataTable({
    validate(
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$gnk_file), 'Please upload at least 1 file!')
    ) 
    DT::datatable(stsTblPerChr(),       
                         filter = list(position = 'top', clear = FALSE),
                         options = list(
                           search = list(regex = TRUE, caseInsensitive = FALSE))) %>%
    DT::formatRound(c(3,5,6), 2)
  })
  
 #output$sumryStatsTblPerChr = downloadHandler('sumStatsTblPerChr-filt.csv', content = function(file) {
 #  s = input$sumryStatsTblPerChr_rows_all
 #  write.csv(stsTblPerChr()[s, , drop = FALSE], file)
 #})
  
  ### Adding ternary plots
  
  
  output$ternPlot <- renderPlot({
    p <- ggtern() + 
      geom_point(data=stsTbl(), 
                 aes(x = aneuploid,y=diploid,z=polyploid,
                     fill = paste0(file_type, ": ",category)),#, label=file_type), 
                 size = 3, alpha = 0.4, pch= 21, color = "black", stroke = 1) + 
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
  
 #difficult to make ternplot per chromosome 
 
  
  #### 2018-05-06 adding scatterplots for heterogeneity and aneuploidy scores
  output$aneuHeteroSctrPlt <- renderPlot({
    p <- ggplot(stsTbl(), aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
                              fill = category, shape=file_type)) + 
      geom_point(pch=21, size=4, stroke = 1, alpha=0.4) + theme_classic() +
      coord_fixed(ratio = 1)
      
    return(p)
    #NULL
  })
  
  output$aneuHeteroSctrPltPerChr <- renderPlot({
    p <- ggplot(stsTblPerChr(), aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
                              color = chr, shape=category)) + 
      geom_point(size=3) + theme_classic() +
      coord_fixed(ratio = 1)
      #scale_size_manual(values = scale_size_manual())
    
    return(p)
    #NULL
  })
  
  ##### 2018-05-06 adding heatmaps
  g4R <- reactive({
    validate(
      need(!is.null(input$sky_file), 'Please upload at least 1 sc-wgs file!')
    ) 
    
    g2_to_g4 <- g2R() %>% 
      spread(chr, num_chr) %>%
      group_by(category)  %>%
      unite(colPaste, -category, -smpl, remove = FALSE) %>%
      count(colPaste) %>%
      mutate(prop = n / sum(n)) %>%
      separate(colPaste, c(1:22, "X"), sep = "_") %>%
      ungroup() %>%
      mutate(category = paste(row_number(), category, sep="___")) %>%
      gather(key = chr, value=chr_freq, 2:(ncol(.)-1)) %>%
      mutate(chr= factor(chr, levels=c(1:22, "X", "n"))) %>%
      mutate(chr_freq = as.numeric(chr_freq)) %>%
      separate(category,into = c("row_numb", "categ"), sep = "___", remove = FALSE) %>%
      mutate(row_numb=as.numeric(row_numb)) %>%
      arrange(categ, row_numb) %>%
      mutate(category = factor(category,levels=unique(category))) 
      return(g2_to_g4)
  })
  
  output$chrHeatG2 <- renderPlot({
      g4.1 <- ggplot(filter(g4R(), chr %in% c(1:22,"X")), aes(x=chr, y=category, 
                                                              fill=factor(chr_freq, levels=sort(unique(chr_freq))))) + 
        geom_tile(color = "white", size = 1) + 
        scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
        theme_classic() + theme(axis.ticks = element_blank(),
                                axis.line = element_blank(),
                                axis.text.x = element_text(size= 8),
                                axis.text.y = element_text(vjust=0.3, hjust = 1)) +
        #coord_fixed(ratio = 1) + 
        xlab("Chromosome") + ylab("")+ 
        theme(legend.position="left", 
              plot.margin=grid::unit(c(0,0,0,0), "mm"),
              aspect.ratio=1)
      
      g4.2 <- ggplot(filter(g4R(), chr == "n"), aes(x=chr, y=category, fill=prop)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_gradient(low = "white", high = "black" ) +
        theme_classic() + theme(axis.ticks = element_blank(),
                                axis.line = element_blank(),
                                axis.text.x = element_text(size= 8),
                                axis.text.y = element_text(vjust=0.3, hjust = 1)) +
        coord_fixed(ratio = 1) + 
        xlab("n") + ylab("") + 
        scale_y_discrete(position = "right") + 
        theme(legend.position="right", 
              plot.margin=grid::unit(c(0,0,0,0), "mm"))
      return(gridExtra::grid.arrange(g4.1, g4.2, ncol=2, widths=c(4,1)))#,layout_matrix=))
    })
  
  ### do the same for sky plots
  s4R <- reactive({
    
    validate(
      need(!is.null(input$sky_file), 'Please upload at least 1 SKY file!')
    ) 
    
    s2_to_s4 <- s2R() %>% 
      spread(chr, num_chr) %>%
      group_by(category)  %>%
      unite(colPaste, -category, -smpl, remove = FALSE) %>%
      count(colPaste) %>%
      mutate(prop = n / sum(n)) %>%
      separate(colPaste, c(1:22, "X"), sep = "_") %>%
      ungroup() %>%
      mutate(category = paste(row_number(), category, sep="___")) %>%
      gather(key = chr, value=chr_freq, 2:(ncol(.)-1)) %>%
      mutate(chr= factor(chr, levels=c(1:22, "X", "n"))) %>%
      mutate(chr_freq = as.numeric(chr_freq)) %>%
      separate(category,into = c("row_numb", "categ"), sep = "___", remove = FALSE) %>%
      mutate(row_numb=as.numeric(row_numb)) %>%
      arrange(categ, row_numb) %>%
      mutate(category = factor(category,levels=unique(category))) 
    return(s2_to_s4)
  })
  
  output$chrHeatS2 <- renderPlot({
    g4.1 <- ggplot(filter(s4R(), chr %in% c(1:22,"X")), aes(x=chr, y=category, 
                                                            fill=factor(chr_freq, levels=sort(unique(chr_freq))))) + 
      geom_tile(color = "white", size = 1) + 
      scale_fill_brewer(type = "div",palette = "RdBu", drop=FALSE, direction = -1, name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      #coord_fixed(ratio = 1) + 
      xlab("Chromosome") + ylab("")+ 
      theme(legend.position="left", 
            plot.margin=grid::unit(c(0,0,0,0), "mm"),
            aspect.ratio=1)
    
    g4.2 <- ggplot(filter(s4R(), chr == "n"), aes(x=chr, y=category, fill=prop)) +
      geom_tile(color = "white", size = 1) + 
      scale_fill_gradient(low = "white", high = "black" ) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + 
      xlab("n") + ylab("") + 
      scale_y_discrete(position = "right") + 
      theme(legend.position="right", 
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
    return(gridExtra::grid.arrange(g4.1, g4.2, ncol=2, widths=c(4,1)))#,layout_matrix=))
  })
  
  
  #### adding permutation plots
  permsG2R <- reactive({
    validate(
      need(!is.null(input$gnk_file), 'Please upload at least 1 sc-wgs file.')
    ) 
    
    nPerms <- 250
    #list_to_pass <- list(g2R(), s2R(), f1R()) %>% purrr::compact() #2018-05-05 issue here?
    fxn <- calc_anca_score #calc_heterog_score#calc_aneupl_score
    g2r_perm_df = retPermPlotDf(input_df = g2R(), fxn, nPerms = nPerms)
    return(g2r_perm_df)
    #lapply(list_to_pass, function(x) retPermPlotDf(x, fxn)) #input_df <- g2R()
  })
  
  
  output$permTableg2R <- renderTable({
    permsG2R()
  })
  
  output$permPlotg2R <- renderPlot({
    colorRedBlue <- RColorBrewer::brewer.pal(n = 11, name = "RdBu")
    ggplot(permsG2R(), aes(x=V1, y=V2, fill=pval_cut)) + 
      geom_tile() + 
      scale_fill_manual(values = colorRedBlue[c(3:9)], drop=FALSE) +
      geom_tile(color = "white", size = 1) + #scale_fill_distiller(direction = 1) +
      geom_text(aes(label=round(pvalue, 3))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") + scale_x_discrete(position = "top") 
    })

  
  permsS2R <- reactive({
    validate(
      need(!is.null(input$sky_file), 'Please upload at least 1 SKY file.')
    ) 
    
    nPerms <- 250
    #list_to_pass <- list(g2R(), s2R(), f1R()) %>% purrr::compact() #2018-05-05 issue here?
    fxn <- calc_anca_score #calc_heterog_score#calc_aneupl_score
    s2r_perm_df = retPermPlotDf(input_df = s2R(), fxn, nPerms = nPerms)
    return(s2r_perm_df)
    #lapply(list_to_pass, function(x) retPermPlotDf(x, fxn)) #input_df <- g2R()
  })
  
  
  output$permTables2R <- renderTable({
    permsS2R()
  })
  
  output$permPlots2R <- renderPlot({
    colorRedBlue <- RColorBrewer::brewer.pal(n = 11, name = "RdBu")
    ggplot(permsS2R(), aes(x=V1, y=V2, fill=pval_cut)) + 
      geom_tile() + 
      scale_fill_manual(values = colorRedBlue[c(3:9)], drop=FALSE) +
      geom_tile(color = "white", size = 1) + #scale_fill_distiller(direction = 1) +
      geom_text(aes(label=round(pvalue, 3))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") + scale_x_discrete(position = "top") 
  })
  
  
  permsF1R <- reactive({
    validate(
      need(!is.null(input$fish_files), 'Please upload at least 1 FISH file.')
    ) 
    
    nPerms <- 250
    #list_to_pass <- list(g2R(), s2R(), f1R()) %>% purrr::compact() #2018-05-05 issue here?
    fxn <- calc_anca_score #calc_heterog_score#calc_aneupl_score
    f1r_perm_df = retPermPlotDf(input_df = f1R(), fxn, nPerms = nPerms)
    return(f1r_perm_df)
    #lapply(list_to_pass, function(x) retPermPlotDf(x, fxn)) #input_df <- g2R()
  })
  
  
  output$permTablef1R <- renderTable({
    permsF1R()
  })
  
  output$permPlotf1R <- renderPlot({
    colorRedBlue <- RColorBrewer::brewer.pal(n = 11, name = "RdBu")
    ggplot(permsF1R(), aes(x=V1, y=V2, fill=pval_cut)) + 
      geom_tile() + 
      scale_fill_manual(values = colorRedBlue[c(3:9)], drop=FALSE) +
      geom_tile(color = "white", size = 1) + #scale_fill_distiller(direction = 1) +
      geom_text(aes(label=round(pvalue, 3))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") + scale_x_discrete(position = "top") 
  })
  
  
  
  #########adding FISH bivariate plots 2018-05-05
  classes <- reactive({unique(f1R()$category)})
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  file_names <- reactive({input$fish_files$name})
  
  output$gridPlots <- renderUI({
    nchrs <- length(unique(f1R()$chr))#f1R() %>% ncol(.) - 2
    chr_pairs <- combn(1:nchrs, 2)
    
    cl_ln <- length(unique(f1R()$category))
    plot_output_list <- lapply(1:(cl_ln*ncol(chr_pairs)), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    #create a tagList of all plots
    do.call(tagList, plot_output_list)
  })
  
  
  all_combos_chr_pairs_and_classes <- reactive({
    nchrs <- length(unique(f1R()$chr)) #f1R() %>% ncol(.) - 2
    chr_pairs <- combn(1:nchrs, 2)
    classes <- unique(f1R()$category)
    expand.grid(1:ncol(chr_pairs),1:length(classes))
  })
  
  f4Plot <- reactive({
    validate(
      need(!is.null(input$fish_files), 'Please upload at least 1 FISH file!')
    ) 
    
    f4 <- f1R() %>% 
      select(-file_type) %>% 
      spread(chr, num_chr) %>% 
      select(-smpl,smpl) #move this column to the end

    print(head(f4))
    return(f4)
  })
  
  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        
        classes <- unique(f1R()$category)
        file_names <- input$fish_files$name #input$fish_files$name})
        
        maxChr <- 8
        maxChrPlus1 = maxChr + 1
        nchrs <-  length(unique(f1R()$chr))
        chr_pairs <- combn(1:nchrs, 2)
        
        #print("my_i")
        #print(my_i)
        #
        #print("head(f4Plot())")
        #print(head(f4Plot()))
        #print("chr_pairs[,all_combos_chr_pairs_and_classes()[my_i,1]]+1")
        #print(chr_pairs[,all_combos_chr_pairs_and_classes()[my_i,1]]+1)
        #print("all_combos_chr_pairs_and_classes()[my_i,1]")
        #print(all_combos_chr_pairs_and_classes()[my_i,1])
 
    
        f1R.t2 <- f4Plot() %>% select(c(1, chr_pairs[,all_combos_chr_pairs_and_classes()[my_i,1]]+1), ncol(.))
        print("head(f1R.t2)")
        print(head(f1R.t2))
        #  for (ea_class in 1:length(classes)){
        
        matr_plot <- return_chr_prop_matr2(f1R.t2,classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                          maxPair = maxChrPlus1)
        print("matr_plot")
        print(matr_plot)
        x_y_axis_lab <- colnames(matr_plot)[4:5]
        print("x_y_axis_lab")
        print(x_y_axis_lab)
        
        plt <- create_perc_matr2(matr_plot, title = classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                 minChr = 1, 
                                 maxChr = maxChrPlus1, xlab = x_y_axis_lab[1], ylab=x_y_axis_lab[2])
        return(plt)
        
      })
    })
  }
  
  
  
#  output$gridPlots2 <- renderPlot({
#    classes <- unique(f1R()$category)
#    file_names <- input$fish_files$name #input$fish_files$name})
#    
#    maxChr <- 8
#    maxChrPlus1 = maxChr + 1
#    nchrs <-3 # length(unique(f1R()$chr))# %>% ncol(.) - 3
#    chr_pairs <- combn(1:nchrs, 2)
#    f2 <- f1R() %>% spread(chr, num_chr)
#    plot_list <- list()
#    for(ea_chr_pair in 1:ncol(chr_pairs)){
#      f1R.t2 <- f2 %>% select(c(1, chr_pairs[,ea_chr_pair]+1))
#      
#      for (ea_class in 1:length(classes)){
#        matr_plot <- return_chr_prop_matr(f1R.t2,classes[ea_class], maxPair = maxChrPlus1)
#        plt <- create_perc_matr2(matr_plot, title = classes[ea_class], minChr = 1, 
#                                 maxChr = maxChrPlus1, xlab = "", ylab="")
#        #save plotted object
#        plot_list[[ea_class + (length(classes)*(ea_chr_pair - 1))]] <- plt
#        
#      }
#    }
#    #plot_list.arr <- grid.arrange(grobs = plot_list, ncol = ncol(chr_pairs)) ## display plot
#    plot_list.arr <- grid.arrange(grobs = plot_list, ncol =1) ## display plot
#    
#    return(plot_list.arr)
#  })
  

})


shinyApp(ui, server)
#app <- shinyApp(ui, server)
#runApp(app, display.mode = "showcase")