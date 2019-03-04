library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
library(RColorBrewer)
library(Cairo)

source("scripts/helper_scripts.R")

max_plots <- 75 # *maximum* total number of gridplots

ui <- tagList(shinyjs::useShinyjs(), 
              withMathJax(), 
              navbarPage(
                title = "aneuvis 1.0", # (Currently Under Construction!)",
                theme = shinythemes::shinytheme("spacelab"),
                id = "inTabset",
                tabPanel("Home",  icon = icon("home"),
                         h3("Aneuvis is a web tool for analyzing chromosomal number variation in single cells."),
                         p("The three types of single-cell chromosomal data that can be uploaded into aneuvis are"),
                         tags$ol(
                           tags$li(tags$a(target = "_blank", 
                                          href =  "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC346675/pdf/pnas00453-0160.pdf", 
                                          "Fluorescence in situ hybridization (FISH)"), "- analyze chromosomal counts from fluorescent DNA probes."), 
                           tags$li(tags$a(target = "_blank", 
                                          href = "https://www.annualreviews.org/doi/abs/10.1146/annurev-genom-090413-025352", 
                                          "Single cell whole genome sequencing (SC-WGS)"), "- analyze chromosome counts from single cell sequencing data."), 
                           tags$li(tags$a(target = "_blank", 
                                          href = "https://www.ncbi.nlm.nih.gov/pubmed/8662537", 
                                          "Spectral karyotyping (SKY)"), "- analyze chromosome counts and structural variation from all chromosomes.")
                         ),
                         p("The output from aneuvis is divided into 3 parts: Table Summary, Visualization, and Hypothesis Testing"),
                         p("See the image below for an overview of aneuvis"),
                         h3("Do treatments A and B induce aneuploidy?"),
                         img(src="aneuvis_layout.png", width=700)
                ),
                tabPanel("Documentation", icon=icon("book"), value = "FAQ", 
                         p("Aneuvis is the product of a collaboration between the",
                           tags$a(target = "_blank", 
                                  href = "http://www.einstein.yu.edu/faculty/9868/cristina-montagna/", 
                                  "Montagna"), "(aneuploidy and cytogenetics) and", 
                           tags$a(target = "_blank", 
                                  href = "https://aibn.uq.edu.au/profile/3649/jessica-mar", 
                                  "Mar"), "(computational biology) labs at Albert Einstein College of Medicine and University of Queensland, respectively."),
                         p("All source code is available on", tags$a(target = "_blank", 
                                                                     href = "https://github.com/dpique/aneuvis", "Github")),
                         p("Aneuvis was created using", tags$a(target = "_blank", 
                                                               href = "http://shiny.rstudio.com/", 
                                                               "Shiny"), "version 1.0.5 (R version 3.4.3) and is available under a GPLv3 license"),
                         p("Please contact daniel.pique@med.einstein.yu.edu with any questions."),
                         p("Last Updated:", format(Sys.Date(), "%B %d, %Y")),
                         hr(),
                         h3("FAQ"),
                         tags$ol(
                           tags$li(tags$b("Can I upload multiple files at once (e.g. both test and control data)?")),
                           tags$ul("Yes. Multiple files should be uploaded in a single step. First, ensure that all files to be uploaded are in the same folder on your computer. 
                                      Then, under the \"Upload Data\" tab, click \"Browse...\". Using your computer's file system navigator (e.g. Finder (Mac) or Windows Explorer), select all the desired files using \"Ctrl + click\" (on PC) or \"command + click \" (on Mac)."),
                           tags$li(tags$b("I am trying to upload multiple FISH files in excel and I get an error. What should I do?")),
                           tags$ul("Check the column names in the excel files to make sure they are the same between all files."),
                           tags$li(tags$b("When uploading Excel files, I get a message that says 'server disconnected - Reload'.")),
                           tags$ul("Ensure that the each Excel file only has a single tab. Excel files with multiple tabs/sheets may not be processed correctly by Aneuvis."),
                           tags$li(tags$b("I am trying to upload an excel file and I get an error that looks like this:")), 
                           img(src="error_1.png", width=200),
                           tags$ul("This could be an issue with the file encoding. Try opening the file and saving it (using 'Save As...') with the same file name (without modifying the file). Then, try re-uploading the file into aneuvis."),
                           tags$ul("Also, please ensure that the headers (i.e. first rows) of all the excel files are identical."),
                           
                           tags$li(tags$b("I see a message that says 'disconnect from the server'. Why is this?")),
                           tags$ul("There could be two reasons for this."),
                           tags$ul("1. You may have been disconnected. Aneuvis sessions that are idle for longer than 1 hour are automatically disconnected, which helps us lower the total cost of running aneuvis. If you need to intermittently return to aneuvis, we suggest downloading the desktop version (", tags$a(target = "_blank", 
                                                                                                                                                                                                                                                                                                                            href = "https://drive.google.com/uc?export=download&id=1_5Jl7QNRMuPEls8paJfO34Oz-EFvgXjM", "beta version"), ", currently only available for Mac) of aneuvis, which runs on your local computer and will not time-out."),
                           tags$ul("2. There is a browser-specific issue. Aneuvis has been tested and works on Chrome, Safari, and Firefox. Please use one of these three browsers for the most consistent results."),
                           
                           tags$li(tags$b("I see a message that says \"An error has occurred. Check your logs or contact the app author for clarification.\" after uploading my FISH data. What should I do?")),
                           tags$ul("There may be an issue with the layout of the uploaded Excel file(s). Try downloading the example FISH data (several Excel files). 
                                   You can directly modify these example Excel files, pasting in your own data into these files. Then, upload these data files to Aneuvis."),

                           tags$li(tags$b("I have an idea to improve aneuvis. How should I share this?")),
                           tags$ul("Email me (daniel.pique@med.einstein.yu.edu) with any suggestions. You can also open an issue on Github or submit a pull request.")
                         ),
                         hr(),
                         h3("Video tutorial of aneuvis"),
                         HTML(paste0('<iframe width="700" height="500" src="https://www.youtube.com/embed/', "SWwBYFNb2PA" ,'" frameborder="5" allowfullscreen></iframe>'))
                ),
                tabPanel("Upload Data", icon=icon("upload"), value = "uploadTab",
                         #tags$head(
                         #  tags$style(HTML('#linkToFAQ{background-color:orange}'))
                         #),
                         p("Getting an error when uploading data? ", #div(id="linkToFAQ", tags$a("Check out the FAQ."))),
                           actionButton("linkToFAQ", "Check out the FAQ.")),
                         #tags$style("#linkToFAQ{background-color:orange}"), #"color: #fff; background-color: #337ab7; border-color: #2e6da4")),#"color: #808080; background-color: #FFFFFF; border-color: #FFFFFF")),

                         tabsetPanel(
                           tabPanel("FISH",
                                    #h2("Under Construction (Feb 5 2019): the code used to produce FISH gridplot visualizations is being updated. Visualizations are currently not working as expected."),
                                    h3("Upload fluorescence in situ hybridization (FISH) data"),
                                    p("Note: All FISH files to be compared must be uploaded together; otherwise, files will overwrite each other if uploaded 1 by 1."),
                                    p("Please gather all excel files in 1 directory and upload all excel files in a single step."),
                                    p("FISH files containing between 2-4 chromosomes can be analyzed. Files with >4 chromosomes will be truncated to 4 chromosomes."),
                                    fileInput(
                                      'fish_files', 
                                      span(".xlsx or .xls",
                                           tags$a(
                                             target = "_blank", 
                                             "(example data)",
                                             href = "https://docs.google.com/uc?export=download&id=1ZO9jWicY-5WohvGbQi_WrQWcDaram5wZ"
                                           )
                                      ),
                                      accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"),
                                      multiple = TRUE 
                                    ),
                                    
                                    actionButton("submit_fish", "Submit and Go to Table Summary"),
                                    actionButton('reset_fish', 'Reset Input'),
                                    #Resetting input: https://gist.github.com/bborgesr/07406b30ade8a011e59971835bf6c6f7
                                    #textOutput("fish_summary"),
                                    textOutput("txt_warn_fish"),
                                    hr(),
                                    h3("FISH file structure guide"),
                                    img(src="fish_layout_excel.png", width=300),
                                    p("Multiple excel files, each with the same # of chromosomes, can be uploaded."),
                                    p("Each Excel file should have *one* tab."),
                                    p("All files *must* have same column names in row 1. Ex. Chr17 and Chr 17 are different"), 
                                    p("Each file will be treated as a separate 'condition'."),
                                    p("The name of each file (before the .xls or .xlsx extension) will be the 'category' ")
                           ),
                           tabPanel("SC-WGS",
                                    h3("Upload single cell whole genome sequencing (sc-wgs) data"),
                                    fileInput(
                                      inputId = "wgs_file",
                                      label = span("Copy Number File (.txt)",
                                           tags$a(
                                             target = "_blank", 
                                             "(example data)",
                                             href = "https://docs.google.com/uc?export=download&id=1VW35NIXSCu7OKaFTSFF_LacwjBqM9JWo"
                                           )
                                      ),
                                      multiple = FALSE,
                                      accept = ".txt"), 
                                    fileInput(
                                      inputId = "wgs_key",
                                      label = span("Key (.xls or .xlsx)",
                                                   tags$a(
                                                     target = "_blank", 
                                                     "(example key)",
                                                     href = "https://docs.google.com/uc?export=download&id=1Yon70xRNv693qSjANrADW1WHSHkZ-2Xj"
                                                   )
                                      ),
                                      multiple = FALSE,
                                      accept = c(".xlsx", ".xls")
                                    ),
                                    actionButton("submit_wgs", "Submit and Go to Table Summary"),
                                    actionButton('reset_wgs', 'Reset Input'),
                                    textOutput(outputId = "txt_warn"),
                                    hr(),
                                    h3("Copy number and key file structure guide"),
                                    img(src="ginkgo_layout.png", width=500),
                                    p("One copy number file can be uploaded at a time."),
                                    h4("Layout of the key"),
                                    img(src="ginkgo_key.png", width=400),
                                    p("Original data on Ginkgo website linked", 
                                      tags$a(target = "_blank", 
                                             href= "http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441",
                                             "here"))
                            ),
                           tabPanel("SKY",
                                    h3("Upload spectral karyotype (SKY) data"),
                                    fileInput(
                                      inputId = "sky_file",
                                      label = span("Upload SKY File (.xls, .xlsx)",
                                                   tags$a(
                                                     target = "_blank",
                                                     "(example data)",
                                                     href = "https://docs.google.com/uc?export=download&id=1hUP9yCWbDeh6Yf2IpR5LtaFs4iFK86tp"
                                                   )
                                              ),
                                      multiple = FALSE,
                                      accept = c(".xlsx", ".xls")
                                    ), 
                                    actionButton("submit_sky", "Submit and Go to Table Summary"),
                                    actionButton('reset_sky', 'Reset Input'),
                                    hr(),
                                    h3("SKY file structure guide"),
                                    img(src="sky_layout.png", width=800),
                                    p("One SKY copy number Excel file with a single tab should be uploaded at a time."),
                                    p("Access a list of International System for Chromosome Nomenclature (ISCN) symbols", 
                                      tags$a(target = "_blank", 
                                             href="https://cgap.nci.nih.gov/Chromosomes/ISCNSymbols", "here"))))),
                
                tabPanel("Table Summary", icon = icon("table"), value = "tableTab",
                         actionButton("returnToDataUpload", "Upload additional datasets"),
                         actionButton("goToVisualization", "Continue to visualization"),
                         selectInput("numberOfX", "Number of X Chromosomes Expected", 
                                     choices=c("1" = "1", "2" = "2"), selected = "2"),
                         selectInput("numberOfY", "Number of Y Chromosomes Expected", 
                                     choices=c("0" = "0", "1" = "1"), selected = "0"),
                         hr(),
                         tabsetPanel(
                           tabPanel("Stats By Group", 
                                    downloadButton("stats_report2", label="Download Stats Table by Group (.csv)"), 
                                    DT::dataTableOutput("sumryStatsTbl"), hr(), addStatSummary()),
                           tabPanel("Stats By Group & Chromosome", 
                                    downloadButton("stats_report3", label="Download Stats Table by Group and Chromosome (.csv)"),
                                    DT::dataTableOutput("sumryStatsTblPerChr"), hr(), addStatSummary()),
                           tabPanel("SC-WGS Chromosome-level Summary", 
                                    hr(), 
                                    p("A 'wide' table of single cell whole genome sequencing (sc-wgs) data is available for download, with chromosomes as columns and samples as rows. 
                                      This table contains the weighted average copy number (by bin size) rounded to the nearest integer per chromosome per sample."),
                                    downloadButton("g2T.d", "Download"),
                                    DT::dataTableOutput("g2T")),
                           tabPanel("Platform concordance",
                                    platformConcordanceUI(id = "concord"))
                           
                         )
                         ),
                tabPanel("Visualization", icon = icon("bar-chart-o"), value = "visTab",
                         downloadButton("report", label="Download visualizations (.pdf)", class = "butt"),
                         tabsetPanel(
                           tabPanel("Scores by Group",
                                    h3("Scatterplot of Aneuploidy and Heterogeneity Score by Group"),
                                    h5("Interactive: Click and drag over points to create a box with the cursor"),
                                    plotOutput("aneuHeteroSctrPlt", brush = "brush_aneuHeteroSctrPlt"),
                                    verbatimTextOutput("brush_info_aneuHeteroSctrPlt"),
                                    hr(),
                                    h3("Ternary Plot of Proportion of Diploid, Polyploid, and Aneuploid Cells by Group"),
                                    #h2("This visualization is currently under construction"),
                                    
                                    plotOutput("ternPlot"),
                                    hr(),
                                    p("Ternary plots are used to represent proportions of 3 groups that sum to 1"),
                                    p("Position of each point represents the proportion of cells within each group.
                                      For example, a point near 'Diploid' would mean that most of the cells within that group
                                      are diploid.")),
                           tabPanel("Scores by Group & Chromosome",
                                    h3("Scatterplot of Aneuploidy and Heterogeneity Score by Group and Chromosome"),
                                    h5("Interactive: Click and drag over points to create a box with the cursor"),
                                    plotOutput("aneuHeteroSctrPltPerChr", brush = "brush_aneuHeteroSctrPltPerChr"),
                                    verbatimTextOutput("brush_info_aneuHeteroSctrPltPerChr")
                           ),
                           tabPanel("FISH", 
                                    fluidRow(
                                      column(4,
                                             h4("Interpreting a gridplot"),
                                             img(src="expl_gridplot.png", width=250)
                                      ),
                                      column(8,
                                             p("Bivariate chromosome gridplots show the percentage of cells
                                               associated with the indicated number of chromosomes."),
                                             p("The diploid state (2 copies of each chromosome) is indicated in bold"),
                                             p("Deeper red colors are associated with an increased percentage"),
                                             p("The sum of the values in each grid equals 100%.")
                                             )
                                    ),
                                    uiOutput("gridPlots")),
                           tabPanel("SC-WGS",
                                    heatMapUI("scwgs_test")),
                           tabPanel("SKY",
                                    heatMapUI("sky_test"))
                                    )), 
                tabPanel("Hypothesis Testing", icon = icon("random"),
                         p("The FISH, SC-WGS, and SKY tabs test for statistically significant differences between experimental groups for the indicated data type."),
                         p("The 'Multi-platform' tab tests for statistically significant differences between experimental groups across all available data types."),
                         tabsetPanel(
                           tabPanel("FISH", 
                                    permPlotTblUI("fish", header = "FISH")),
                           tabPanel("SC-WGS",
                                    permPlotTblUI("sc-wgs", header = "Single Cell Whole Genome Sequencing")),
                           tabPanel("SKY", 
                                    permPlotTblUI("sky", header = "SKY")),
                           tabPanel("Multi-platform",
                                    permPlotTblMultiInputUI("multiple", header = "Multi-platform"))
                         )
                         )))

server <- shinyServer(function(input, output, session) {
  ###########
  #1. Read in raw data
  rv <- reactiveValues(f1=NULL, s1=NULL, w1=NULL, df_key=NULL, df_fish=NULL, upload_state = NULL,
                       count = 0)
  
  observeEvent(input$fish_files, {
    rv$upload_state <- 'uploaded'
    rv$count <- rv$count + 1
    print("rv$count - file1")
    print(rv$count)
  })
  
  observeEvent(input$reset_fish, {
    rv$upload_state <- 'reset'
    rv$count <- rv$count + 1
    print("rv$count - reset")
    print(rv$count)
    #shinyjs::reset('file1')
    
  })
  
  file_input <- reactive({
    if (is.null(rv$upload_state)) {
      return(NULL)
    } else if (rv$upload_state == 'uploaded') {
      return(input$fish_files)
    } else if (rv$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  ####
  
  df_test <- eventReactive({char_to_num(paste0(unlist(input$fish_files$name), collapse = "")) | input$reset_fish}, {
    df <- retFishDf_head3(fish_name = file_input()$name, fish_datapath = file_input()$datapath)
    print("df:")
    print(df)
    return(df)
  })
  
  msg <- eventReactive({char_to_num(paste0(unlist(input$fish_files$name), collapse = "")) | input$reset_fish}, {
    #df_test()
    #df <- retFishDf_head3(fish_name = file_input()$name, fish_datapath = file_input()$datapath)
    if(is.null(df_test())){
      #shinyjs::reset('file1')
      return("Plz upload data")
    } else if(allSame(df_test())){
      return("Data uploaded successfully!")
    } else {
      return("Please check column headers")
    }
    
  })
  
  output$txt_warn_fish <- renderText({msg()})
  
  ###
  
  observeEvent(input$submit_fish,{
    req(input$fish_files)
    rv$f1 <- retFishDf(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
  })
  
  #observe({
  #  req(input$fish_files)
  #  #validate(
  #  #  need(try(colnames(input$sky_file)), "Please select a data set")
  #  #)
  #  #rv$s1 <- retSkyDf(sky_datapath = input$sky_file$datapath)
  #  rv$df_fish <- retFishDf_head3(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
  #})
  # stop 2-28-2019
  #output$txt_warn_fish <- renderText({
  #  allSame <- function(x) length(unique(x)) == 1
  #  
  #  validate(
  #    need(!is.null(rv$df_fish), "Please upload data files!")
  #    #need(is.null(rv$df_fish) & !allSame(rv$df_fish), "Warning- please make sure column headers are same!")
  #    #need(!allSame(rv$df_fish), ""
  #    #need(!is.null(rv$df_fish), ""WARNING: Please make sure column")
  #    #return("WARNING: Please make sure column names are specified correctly (see below)!")
  #  )
  #  
  #  #rv$df_fish <- retFishDf_head3(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
  #  
  #  
  #  print("allSame(tbl_list_colnames)")
  #  print(allSame(rv$df_fish))
  #  allSameRes <- allSame(rv$df_fish)
  #  #print(is.null(allSameRes))
  #  print("allSameRes")
  #  print(allSameRes)
  #  
  #  print("rv$df_fish")
  #  print(rv$df_fish)
  #  
  #  #print()
  #  #if(allSame(rv$df_fish) == TRUE | is.null(allSame(rv$df_fish))){
  #    
  #  if(is.null(rv$df_fish)){
  #    return("Please upload data files!")
  #  } else if(allSame(rv$df_fish) == TRUE){ #| is.null(allSame(rv$df_fish))){
  #    return("") #all the same
  #  #} else if(is.null(rv$df_fish)){
  #   # return("Please upload data files!")
  #  } else {
  #    #rv$df_fish <- NULL
  #    return("WARNING: Please make sure column names are specified correctly (see below)!")
  #    #return("WARNING: Please make sure column names are specified correctly (see below)!")
  #  }
  #  #input$fish_files$name
  #  #rv$df_fish <- retFishDf_head(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
  #  #return(rv$df_fish)
  #  #do all the columns have the same name?
  #  #if(rv$df_fish){#input$fish_files$name) | is.null(rv$df_fish)){# is.null(colnames(rv$df_fish))){
  #  #  return("")
  #  #} else if (!rv$df_fish) {
  #  #  return("WARNING: Please make sure column names are specified correctly (see below)!")
  #  #}
  #})
  
  #observeEvent(input$reset_fish, {
  #  rv$f1 <- NULL
  #  rv$df_fish <- NULL
  #  shinyjs::reset('fish_files')
  #})
  
  observe({
    req(input$sky_file)
    #validate(
    #  need(try(colnames(input$sky_file)), "Please select a data set")
    #)
    rv$s1 <- retSkyDf(sky_datapath = input$sky_file$datapath)
  })
  
  observeEvent(input$reset_sky, {
    rv$s1 <- NULL
    shinyjs::reset('sky_file')
  })

  observeEvent(input$submit_wgs, {
  #observe({
    req(input$wgs_file, input$wgs_key)
    #validate(need(x, message = FALSE))
    #original
    rv$w1 <- retWgsDf(wgs_datapath = input$wgs_file$datapath, wgs_key_datapath = input$wgs_key$datapath)

  })
  
  
  #02-25-2019
  
  observe({
    req(input$wgs_key)
    #validate(
    #  need(try(colnames(input$sky_file)), "Please select a data set")
    #)
    #rv$s1 <- retSkyDf(sky_datapath = input$sky_file$datapath)
    #rv$df_fish <- retFishDf_head(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
    rv$df_key <- retWgsDf_head(wgs_key_datapath = input$wgs_key$datapath)
    
  })
  
  output$txt_warn <- renderText({

    validate(
      need(!is.null(rv$df_key), "Please upload key!")
    )
    
    #$df_key <- retWgsDf_head(wgs_key_datapath = input$wgs_key$datapath)

    if(is.null(colnames(rv$df_key))){
      return("")
    } else if(identical(colnames(rv$df_key), c("smpl_id", "category"))){
      return("")
    } else {
      return("WARNING: Please make sure column names are specified correctly (see below)!")
    }
  })
  
  observeEvent(input$reset_wgs, {
    rv$w1 <- NULL
    rv$df_key <- NULL 
    shinyjs::reset('wgs_file')
    shinyjs::reset('wgs_key')
  })
  
  
  g2.1R <- reactive({
    if (is.null(rv$w1)) {
      return(NULL)
    }
    g2.t <- rv$w1 %>% 
      spread(key = chr, value = num_chr)
    colnames(g2.t)[4:ncol(g2.t)] <- paste0("Chr. ", colnames(g2.t)[4:ncol(g2.t)])
    return(g2.t)
  })
  
  output$g2T.d <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), "-ginkgo-chr-summary", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(g2.1R(), file, row.names = FALSE)
    }
  )
  
  output$g2T <- DT::renderDataTable({
    if (is.null(rv$w1)) {
      return(NULL)
    }
    DT::datatable(rv$w1)
  })
  
  numbX <- reactive({
    input$numberOfX
  })
  numbY <- reactive({
    input$numberOfY
  })
  
  stsTbl <- eventReactive({input$submit_fish | input$submit_sky | input$reset_sky | input$reset_fish |
      input$reset_wgs | input$submit_wgs |  as.numeric(input$numberOfX) | as.numeric(input$numberOfY)}, {
        
        validate(
          need(!is.null(rv$w1) | !is.null(rv$f1) | !is.null(rv$s1), " ")
        )     
        
        numX = as.numeric(numbX())
        numY = as.numeric(numbY())
        list_to_pass <- list(rv$s1, rv$f1, rv$w1) %>% purrr::compact()
        #calculate the scores that reflect variability and degree of aneuploidy
        aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score, numX=numX, numY=numY)
        heterog_scores = purrr::map_df(.x = list_to_pass, .f = calc_heterog_score)
        anca_scores_normalized = purrr::map_df(.x = list_to_pass, .f = calc_anca_score_normalized, numX=numX, numY=numY)
        anca_scores = purrr::map_df(.x = list_to_pass, .f = calc_anca_score, numX=numX, numY=numY)
        instab_idx = purrr::map_df(.x = list_to_pass, .f = calc_instab_idx)
        perc_ploidy <- purrr::map_df(.x = list_to_pass, .f = calc_perc_ploidy, numX=numX, numY=numY)
        sumStats <- purrr::reduce(list(aneupl_scores, heterog_scores, anca_scores_normalized, anca_scores, instab_idx, perc_ploidy), full_join, by=c("category", "file_type")) %>%
          select(category, file_type, n, everything())
        return(sumStats)
      })
  
  output$sumryStatsTbl <- DT::renderDataTable({
    
    validate(
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$wgs_file), " ")
    ) 
    DT::datatable(stsTbl(),       
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE))) %>% DT::formatRound(c(4:11), 2)
  })
  
  stsTblPerChr <- eventReactive({input$submit_fish | input$submit_sky | input$reset_sky | input$reset_fish |
      input$reset_wgs | input$submit_wgs |  as.numeric(input$numberOfX) | as.numeric(input$numberOfY)}, {
        
        validate(
          need(!is.null(rv$w1) | !is.null(rv$f1) | !is.null(rv$s1), " ")
        )     
        
        numX = as.numeric(numbX())
        numY = as.numeric(numbY()) 
        
        list_to_pass <- list(rv$s1, rv$f1, rv$w1) %>% purrr::compact()
        aneupl_scores = purrr:::map_df(.x = list_to_pass, .f = calc_aneupl_score, retChr = TRUE, numX=numX, numY=numY)
        heterog_scores = purrr:::map_df(.x = list_to_pass, .f = calc_heterog_score, retChr = TRUE)
        anca_scores_normalized = purrr::map_df(.x = list_to_pass, .f = calc_anca_score_normalized, retChr = TRUE, numX=numX, numY=numY)
        sumStats <- purrr::reduce(list(aneupl_scores,heterog_scores, anca_scores_normalized), full_join, by=c("category","file_type", "chr")) %>%
          select(category, file_type, n, everything())
        return(sumStats)
      })
  
  output$sumryStatsTblPerChr <- DT::renderDataTable({
    validate(
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$wgs_file), " ") #'Please upload at least 1 file!')
    ) 
    DT::datatable(stsTblPerChr(),       
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE))) %>%
      DT::formatRound(5:7, 2)
  })

  
  output$stats_report2 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-aneuvis-stats-by-group.csv")
    },
      content = function(file) {
        write.csv(stsTbl(), file, row.names = FALSE)
      }
  )
  
  output$stats_report3 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-aneuvis-stats-by-group-and-chr.csv")
    },
    content = function(file) {
      write.csv(stsTblPerChr(), file, row.names = FALSE)
    }
  )
  

  ### Adding ternary plots
  ### issue with ternary plots - 2019-02-05
  
  output$ternPlot <- renderPlot({
    p <- ggtern::ggtern() + 
      geom_point(data=stsTbl(), 
                 aes(x = aneuploid,y=diploid,z=polyploid, color=category, shape=file_type),
                 size = 3, alpha = 0.8) + 
          xlab("") + ylab("") +
          Tlab("Diploid") +
          Llab("Aneuploid") +
          Rlab("Polyploid") +
          limit_tern(1.03,1.03,1.03) 
    #print(stsTbl())
    #p <- ggplot() + 
    #  geom_point(data=stsTbl(), 
    #             aes(x = aneuploid,y=diploid),
    #             size = 3, alpha = 0.8)
    
    print(p)
  })
  
  #output$ternPlot <- renderPlot({
  #  p <- ggtern() + 
  #    geom_point(data=stsTbl(), 
  #               aes(x = aneuploid,y=diploid,z=polyploid,
  #                   color = category, shape= file_type),
  #               size = 3, alpha = 0.8) + 
  #    xlab("") + ylab("") +
  #    Tlab("Diploid") +
  #    Llab("Aneuploid") +
  #    Rlab("Polyploid") +
  #    guides(fill=guide_legend(title="Legend")) +
  #    limit_tern(1.03,1.03,1.03) 
  #  print(p)
  #})
  
  #### adding scatterplots for heterogeneity and aneuploidy scores
  output$aneuHeteroSctrPlt <- renderPlot({
    p2 <- ggplot(stsTbl(), aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
                               color = category, shape= file_type)) + 
      geom_point( size=4, alpha=0.8) + theme_classic() +
      coord_fixed(ratio = 1)
    return(p2)
  })
  
  output$brush_info_aneuHeteroSctrPlt <- renderPrint({
    brushedPoints(data.frame(stsTbl()), input$brush_aneuHeteroSctrPlt)
  })
  
  output$aneuHeteroSctrPltPerChr <- renderPlot({
    p <- ggplot(stsTblPerChr(), aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
                                    color = chr, shape=category)) + 
      geom_point(size=3) + theme_classic() +
      coord_fixed(ratio = 1)
    return(p)
  })
  
  output$brush_info_aneuHeteroSctrPltPerChr <- renderPrint({
    brushedPoints(data.frame(stsTblPerChr()), input$brush_aneuHeteroSctrPltPerChr)
  })
  
  ##### 2018-05-06 adding heatmaps
  g4R <- reactive({
    if (is.null(input$wgs_file)) {
      return(NULL)
    }
    return(two_to_four(rv$w1))
  })
  
  ### do the same for sky plots
  s4R <- reactive({
    if (is.null(input$sky_file)) {
      return(NULL)
    }
    return(two_to_four(rv$s1))
  })
  
  #2018-05-30
  callModule(heatMap, "sky_test", input_df = rv$s1, file_type = "sky", orig_input = reactive(input$sky_file))
  callModule(heatMap, "scwgs_test", input_df = rv$w1, file_type = "sc-WGS", orig_input = reactive(input$wgs_file))
  
  #### adding permutation plot modules - 2018-05-12 
  callModule(permPlotTbl, "fish",  
             input_df = reactive(rv$f1), 
             nPerms = reactive(input$Nperms))
  
  callModule(permPlotTbl, "sc-wgs", 
             input_df = reactive(rv$w1),
             nPerms = reactive(input$Nperms))
  
  callModule(permPlotTbl, "sky",
             input_df = reactive(rv$s1), 
             nPerms = reactive(input$Nperms))
  
  callModule(permPlotTblMultiInput, "multiple",
             sky_df = reactive(rv$s1), fish_df = reactive(rv$f1), 
             wgs_df = reactive(rv$w1), nPerms = reactive(input$Nperms))
  
  callModule(platformConcordance2, "concord", 
             sky_df = reactive(rv$s1), 
             fish_df = reactive(rv$f1), 
             wgs_df = reactive(rv$w1),
             numbX = numbX, numbY = numbY)
  
  ###### adding shinyjs buttons - redirection
  ### fish
  observeEvent(input$submit_fish, {
    updateTabsetPanel(session, "inTabset",
                      selected = "tableTab")
  })
  
  observeEvent(input$linkToFAQ, {
    updateTabsetPanel(session, "inTabset", # tableTab
                      selected = "FAQ")
  })
  
  observe({
    shinyjs::toggleState("submit_fish", !is.null(input$fish_files))
  })
  
  ### wgs
  observeEvent(input$submit_wgs, {
    updateTabsetPanel(session, "inTabset",
                      selected = "tableTab")
  })
  
  observe({
    shinyjs::toggleState("submit_wgs", !is.null(input$wgs_file) & !is.null(input$wgs_key))
  })
  
  ### sky
  observeEvent(input$submit_sky, {
    updateTabsetPanel(session, "inTabset",
                      selected = "tableTab")
  })
  
  observe({
    shinyjs::toggleState("submit_sky", !is.null(input$sky_file))
  })
  
  #additional buttons
  observeEvent(input$returnToDataUpload, {
    updateTabsetPanel(session, "inTabset",
                      selected = "uploadTab")
  })
  
  observeEvent(input$goToVisualization, {
    updateTabsetPanel(session, "inTabset",
                      selected = "visTab")
  })
  
  #########adding FISH bivariate plots 2018-05-05
  classes <- reactive({unique(rv$f1$category)})
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  file_names <- reactive({input$fish_files$name})
  
  output$gridPlots <- renderUI({
    nchrs <- length(unique(rv$f1$chr))
    chr_pairs <- combn(1:nchrs, 2)
    
    cl_ln <- length(unique(rv$f1$category))
    plot_output_list <- lapply(1:(cl_ln*ncol(chr_pairs)), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    #create a tagList of all plots
    do.call(tagList, plot_output_list)
  })
  
  
  all_combos_chr_pairs_and_classes <- reactive({
    nchrs <- length(unique(rv$f1$chr))
    chr_pairs <- combn(1:nchrs, 2)
    classes <- unique(rv$f1$category)
    expand.grid(1:ncol(chr_pairs),1:length(classes))
  })
  
  f4Plot <- reactive({
    validate(
      need(!is.null(input$fish_files), " ")
    )
    f4 <- rv$f1 %>% 
      select(-file_type) %>% 
      spread(chr, num_chr) %>% 
      select(-smpl,smpl)
    return(f4)
  })
  
  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      output[[plotname]] <- renderPlot({
        classes <- unique(rv$f1$category)
        file_names <- input$fish_files$name
        maxChr <- 8
        maxChrPlus1 = maxChr + 1
        nchrs <-  length(unique(rv$f1$chr))
        chr_pairs <- combn(1:nchrs, 2)
        
        f1R.t2 <- f4Plot() %>% select(c(1, chr_pairs[,all_combos_chr_pairs_and_classes()[my_i,1]]+1), ncol(.))
        matr_plot <- return_chr_prop_matr2(f1R.t2,classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                           maxPair = maxChrPlus1)
        print(matr_plot)
        x_y_axis_lab <- colnames(matr_plot)[4:5]
        print(x_y_axis_lab)
        
        #2019-02-05
        #plt <- create_perc_matr2.1(matr_plot, title = classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
        #                           minChr = 1, 
        #                           maxChr = maxChrPlus1, xlab = x_y_axis_lab[1], ylab=x_y_axis_lab[2])
        plt <- create_perc_matr2.simple(matr_plot, title = classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                   minChr = 1, 
                                   maxChr = maxChrPlus1, xlab = x_y_axis_lab[1], ylab=x_y_axis_lab[2])
        
        #create_perc_matr2.simple
        return(plt)
      })
    })
  }
  
  ### generate rmarkdown report
  output$report <- downloadHandler(
    filename = paste0(Sys.Date(),"-aneuvis-report.pdf"),
    content  = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report3.Rmd")
      file.copy("report3.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(fish_files = input$fish_files, 
                     sky_file = input$sky_file,
                     wgs_file = input$wgs_file,
                     wgs_key = input$wgs_key,
                     numbX = numbX(), numbY = numbY(),
                     stsTbl = stsTbl(),
                     stsTblPerChr=stsTblPerChr(),
                     g4 = g4R(),
                     s4 = s4R(),
                     f1 = rv$f1)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
})

if (interactive()) {
  s <- session_info()
  write_csv(x = plyr::ldply(s$platform), path = here("session_info", paste0(Sys.time(), "pltfrm.txt")))
  write_csv(x = s$packages, path = here("session_info", paste0(Sys.time(), "pkgs.txt")))
  si <- sessionInfo()
  paste0(c(si$basePkgs, s$packages$package), collapse = "\\|^") 
}

shinyApp(ui, server)



#What's necessary to run this package