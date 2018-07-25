#2018-05-31
#Updating the app to allow for reset buttons
#will be a major rewrite (including observe, observeEvent, and 
#eventReactive wrappers around existing objects)
#will first start with FISH

library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
#library(shinycustomloader)
library(RColorBrewer)
#require(openxlsx)
library(Cairo)
#library(Rtools)
#library(DT)
source("scripts/helper_scripts.R")

max_plots <- 50 # *maximum* total number of plots

ui <- tagList(shinyjs::useShinyjs(), 
              withMathJax(), 
              navbarPage(
                title = "aneuvis 0.7",
                
                theme = shinythemes::shinytheme("spacelab"),
                id = "inTabset",
                
                tabPanel("Home",  icon = icon("home"),
                         h3("Aneuvis is a web tool for analyzing chromosomal number variation in single cells."),
                         p("The three types of single-cell chromosomal data that can be uploaded into aneuvis are"),
                         tags$ol(
                           tags$li(tags$a(target = "_blank", 
                                          href =  "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC346675/pdf/pnas00453-0160.pdf", #"https://www.nature.com/scitable/topicpage/fluorescence-in-situ-hybridization-fish-327", 
                                          "Fluorescence in situ hybridization (FISH)"), "- analyze chromosomal counts from 2 to 4 (or more) chromosomes."), 
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
                tabPanel("Documentation", icon=icon("book"),
                         p("Aneuvis is the product of a collaboration between the",
                           tags$a(target = "_blank", 
                                  href = "http://www.einstein.yu.edu/faculty/9868/cristina-montagna/", 
                                  "Montagna"), "(aneuploidy and cytogenetics) and", 
                           tags$a(target = "_blank", 
                                  href = "http://www.einstein.yu.edu/faculty/12990/jessica-mar/", 
                                  "Mar"), "(computational biology) labs at Albert Einstein College of Medicine."),
                         p("All source code is available on", tags$a(target = "_blank", 
                                                                     href = "https://github.com/dpique/aneuVis", "Github")),
                         p("Aneuvis was created using", tags$a(target = "_blank", 
                                                               href = "http://shiny.rstudio.com/", 
                                                               "Shiny"), "version 1.0.5 (R version 3.4.3) and is available under a GPLv3 license"),
                         p("Please contact daniel.pique@med.einstein.yu.edu with any questions."),
                         hr(),
                         h3("FAQ"),
                         tags$ol(
                           tags$li(tags$b("I am trying to upload multiple FISH files in excel and I get an error. What should I do?")),
                           tags$ul("Try checking the column names in the excel files to make sure they are the same between all files."),
                           tags$li(tags$b("When uploading Excel files, I get a message that says 'server disconnected - Reload'.")),
                           tags$ul("Ensure that the each Excel file only has a single tab. Excel files with multiple tabs/sheets may not be processed correctly by Aneuvis."),
                           tags$li(tags$b("I am trying to upload an excel file and I get an error that looks like this:")), 
                           img(src="error_1.png", width=200),
                           tags$ul("This could be an issue with the file encoding. Try opening the file and saving it (using 'Save As...') with the same file name (without modifying the file). Then, try re-uploading the file into aneuvis."),
                           tags$li(tags$b("I have an idea to improve aneuvis. How should I share this?")),
                           tags$ul("Email me (daniel.pique@med.einstein.yu.edu) with any suggestions. You can also open an issue on Github or submit a pull request.")
                         ),
                         hr(),
                         h3("Video tutorial of aneuvis"),#"Watch the tutorial below to get started. (forthcoming)"),
                         #fluidPage(
                         #  tags$video(type = "video/mp4",src = "trial5_good.mp4", controls = "controls", width=700)
                         #)#,https://www.youtube.com/watch?v=SWwBYFNb2PA
                         HTML(paste0('<iframe width="700" height="500" src="https://www.youtube.com/embed/', "SWwBYFNb2PA" ,'" frameborder="5" allowfullscreen></iframe>'))
                         #https://www.youtube.com/watch?v=aircAruvnKk
                ),
                tabPanel("Upload Data", icon=icon("upload"), value = "uploadTab",
                         tabsetPanel(
                           tabPanel("FISH",
                                    h3("Upload fluorescence in situ hybridization (FISH) data"),
                                    p("Note: All FISH files to be compared must be uploaded together; otherwise, files will overwrite each other if uploaded 1 by 1."),
                                    fileInput(
                                      'fish_files', 
                                      span(".xlsx or .xls",
                                           tags$a(
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
                                    textOutput("fish_summary"),
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
                                             "(example data)",
                                             href = "https://docs.google.com/uc?export=download&id=1VW35NIXSCu7OKaFTSFF_LacwjBqM9JWo"
                                           )
                                      ),
                                      #label = "Copy Number File (.txt)", #names must match those in gnko
                                      multiple = FALSE,
                                      accept = ".txt"), 
                                    fileInput(
                                      inputId = "wgs_key",
                                      label = span("Sample Key (.xls or .xlsx)",
                                                   tags$a(
                                                     "(example key)",
                                                     href = "https://docs.google.com/uc?export=download&id=1Yon70xRNv693qSjANrADW1WHSHkZ-2Xj"
                                                   )
                                      ),
                                      multiple = FALSE,
                                      accept = c(".xlsx", ".xls")#, ".csv", ".txt", ".tsv")
                                    ),
                                    actionButton("submit_wgs", "Submit and Go to Table Summary"),
                                    actionButton('reset_wgs', 'Reset Input'),
                                    hr(),
                                    h3("Copy number and key file structure guide"),
                                    
                                    #p("The format of a copy number file is shown below."),
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
                                      #label = "Upload SKY File (.xls, .xlsx)", #names must match those in gnko
                                      label = span("Upload SKY File (.xls, .xlsx)",
                                                   tags$a(
                                                     "(example data)",
                                                     href = "https://docs.google.com/uc?export=download&id=1hUP9yCWbDeh6Yf2IpR5LtaFs4iFK86tp"
                                                   )
                                      ),
                                      
                                      multiple = FALSE,
                                      accept = c(".xlsx", ".xls")#c(".csv", ".txt", ".tsv")
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
                         #downloadButton("stats_report", label="Download statistics (.xlsx)"),
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
                tabPanel("Visualization", icon = icon("bar-chart-o"), value = "visTab",  #icon = icon("heatmap"), #
                         downloadButton("report", label="Download visualizations (.pdf)", class = "butt"),
                         #tags$head(tags$style(".butt{background-color:#add8e6; color:red}")), # background color and font color
                         
                         tabsetPanel(
                           tabPanel("Scores by Group",
                                    h3("Scatterplot of Aneuploidy and Heterogeneity Score by Group"),
                                    h5("Interactive: Click and drag over points to create a box with the cursor"),
                                    plotOutput("aneuHeteroSctrPlt", brush = "brush_aneuHeteroSctrPlt"),
                                    verbatimTextOutput("brush_info_aneuHeteroSctrPlt"),
                                    hr(),
                                    h3("Ternary Plot of Proportion of Diploid, Polyploid, and Aneuploid Cells by Group"),
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
                                    heatMapUI("sky_test")) #2018-05-30
                                    )), 
                tabPanel("Hypothesis Testing", icon = icon("random"),
                         #p("There are multiple tabs that can be used to test for differences across experimental groups or data types."),
                         p("The FISH, SC-WGS, and SKY tabs test for statistically significant differences between experimental groups for the indicated data type."),
                         p("The 'Multi-platform' tab tests for statistically significant differences between experimental groups across all available data types."),
                        # p("The Comparing Platform Concordance tab shows whether different platforms have the same trend in change of a given statistic between treatment groups."),
                        # h3("Are groups statistically significantly different from each other
                        #    in terms of the degree of numerical aneuploidy?"),
                        # p("How to use this page:"),
                        # p("Three steps: 1. Select the tab of the data type you would like to permute"),
                        # p("2. Select the # of desired permutations (default is 500). More perms will take longer."),
                        # p("3. Select the score to permute, then hit 'permute'. This may take a few minutes depending on 
                        #   the number of permutations."),
                        # p("Methods: Generate random permutations of the category associated with each observed cell. 
                        #   The difference in scores between all possible pairs of categories is calculated after each permutation. 
                        #   A p-value is calculated by counting how many permuted ANCA scores are more extreme than
                        #   the observed ANCA score."),
                        # p("The p-values is 1-sided, and tests the null hypothesis that there is no significant difference in scores
                        #   between a given pair of groups. there two possible interpretations of the resulting p-value:
                        #   not significantly different (p > 0.05, grey color) or significantly different (blue color)."),
                        # h4("Key for the table columns"),
                        # p("- Group 1 and Group 2 are the groups that are being compared"),
                        # p("- nperm_gr_thn_obs is the number of permutations greater than the observed normalized ANCA score"),
                        # p("- pvalue is the pvalue rounded to 2 decimal places,	pval_cut is the categorization of the pvalue into bins (for heatmap purposes)"),
                        # p("- perm_mean is the mean of the anca scores across all permuted samples"),	
                        # p("- perm_dist_2.5% and perm_dist_97.5% are the lower and upper 95% CI for the permuted ANCA scores"),
                        # p("- obs_val is the observed difference in ANCA score between the 2 groups"),
                        # p("- fold_change is the observed difference in ANCA scores divided by the mean permuted difference in ANCA scores. Analogous to fold enrichment above baseline noise."),
                         
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
  rv <- reactiveValues(f1 = NULL, s1=NULL, w1=NULL)
  
  observe({
    req(input$fish_files)
    rv$f1 <- retFishDf(fish_name = input$fish_files$name, fish_datapath = input$fish_files$datapath)
  })
  
  observeEvent(input$reset_fish, {
    rv$f1 <- NULL
    shinyjs::reset('fish_files')
  })
  
  observe({
    req(input$sky_file)
    rv$s1 <- retSkyDf(sky_datapath = input$sky_file$datapath)
  })
  
  observeEvent(input$reset_sky, {
    rv$s1 <- NULL
    shinyjs::reset('sky_file')
  })
  
  observeEvent(input$submit_wgs, {
    req(input$wgs_file, input$wgs_key)
    rv$w1 <- retWgsDf(wgs_datapath = input$wgs_file$datapath, wgs_key_datapath = input$wgs_key$datapath)
  })
  
  observeEvent(input$reset_wgs, {
    rv$w1 <- NULL
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
          need(!is.null(rv$w1) | !is.null(rv$f1) | !is.null(rv$s1), " ") #"Please upload at least 1 file!")
        )     
        
        numX = as.numeric(numbX())
        numY = as.numeric(numbY())
        list_to_pass <- list(rv$s1, rv$f1, rv$w1) %>% purrr::compact() #2018-05-05 issue here?
        aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score, numX=numX, numY=numY)
        
        #print(aneupl_scores)
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
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$wgs_file), " ") #'Please upload at least 1 file!')
    ) 
    DT::datatable(stsTbl(),       
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE))) %>% DT::formatRound(c(4:11), 2)
  })
  
  stsTblPerChr <- eventReactive({input$submit_fish | input$submit_sky | input$reset_sky | input$reset_fish |
      input$reset_wgs | input$submit_wgs |  as.numeric(input$numberOfX) | as.numeric(input$numberOfY)}, {
        
        validate(
          need(!is.null(rv$w1) | !is.null(rv$f1) | !is.null(rv$s1), " ")# "Please upload at least 1 file!")
        )     
        
        numX = as.numeric(numbX())
        numY = as.numeric(numbY()) 
        
        list_to_pass <- list(rv$s1, rv$f1, rv$w1) %>% purrr::compact() #2018-05-05 issue here?
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
  
  #2018-05-29
  #output$stats_report <- downloadHandler(
  #  filename =  paste0(Sys.Date(), "-aneuvis-stats.xlsx"), 
  #  content = function(file) {
  #    list_of_datasets <- list("Stats By Group" = stsTbl(), "Stats By Group and Chromosome" = stsTblPerChr())
  #    write.xlsx(list_of_datasets, file = file)
  #  }
  #)
  #2018-06-01
  #output$stats_report <- downloadHandler(
  #  filename = function() {
  #    paste0(Sys.Date(), "-aneuvis-stats.xlsx")
  #    },
  #  content = function(file) {
  #    list_of_datasets <- list("Stats By Group" = stsTbl(), "Stats By Group and Chromosome" = stsTblPerChr())
  #    write.xlsx(list_of_datasets, file = file)
  #  }
  #)
  
  
  
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
  
#  output$downloadFile <- downloadHandler(
#    filename = function() {
#      paste0(Sys.Date(), "-aneuvis-stats.xlsx")
#    },
#     content = function(file){
#       fname <- paste(file, "xlsx", sep = ".")
#       # Create and assign names to the blank workbook and worksheets
#       wb <- loadWorkbook(fname, create = TRUE)
#       createSheet(wb, name = "Stats By Group")
#       createSheet(wb, name = "Stats By Group and Chromosome")
#       # Write the reactive datasets to the appropriate worksheets
#       writeWorksheet(wb, stsTbl(), sheet = "Stats By Group")
#       writeWorksheet(wb, stsTblPerChr(), sheet = "Stats By Group and Chromosome")
#       # Save and prepare for download
#       saveWorkbook(wb)
#       file.rename(fname, file)
#     }
#})
   
  # Set up parameters to pass to Rmd document
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  
  ### Adding ternary plots
  output$ternPlot <- renderPlot({
    p <- ggtern() + 
      geom_point(data=stsTbl(), 
                 aes(x = aneuploid,y=diploid,z=polyploid,
                     color = category, shape= file_type),
                 size = 3, alpha = 0.8) + 
      xlab("") + ylab("") +
      Tlab("Diploid") +
      Llab("Aneuploid") +
      Rlab("Polyploid") +
      guides(fill=guide_legend(title="Legend")) +
      limit_tern(1.03,1.03,1.03) 
    print(p)
  })
  
  #### 2018-05-06 adding scatterplots for heterogeneity and aneuploidy scores
  output$aneuHeteroSctrPlt <- renderPlot({
    p2 <- ggplot(stsTbl(), aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
                               color = category, shape= file_type)) +  #paste0(file_type, ": ",category))) + #category, shape=file_type)) + 
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
  
  #if(FALSE){
  
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
  callModule(permPlotTbl, "fish", #file_input = reactive(input$fish_files), 
             input_df = reactive(rv$f1), nPerms = reactive(input$Nperms))
  
  callModule(permPlotTbl, "sc-wgs", #file_input = reactive(input$wgs_file), 
             input_df = reactive(rv$w1), nPerms = reactive(input$Nperms))
  
  callModule(permPlotTbl, "sky", #file_input = reactive(input$sky_file),
             input_df = reactive(rv$s1), nPerms = reactive(input$Nperms))
  
  callModule(permPlotTblMultiInput, "multiple", #file_input = reactive(input$sky_file),
             sky_df = reactive(rv$s1), fish_df = reactive(rv$f1), 
             wgs_df = reactive(rv$w1), nPerms = reactive(input$Nperms))
  
  #callModule(platformConcordance, "concord", stsTbl = stsTbl)#reactive(stsTbl()))
  callModule(platformConcordance2, "concord", 
             sky_df = reactive(rv$s1), 
             fish_df = reactive(rv$f1), 
             wgs_df = reactive(rv$w1),
             numbX = numbX, numbY = numbY)
  
  ###### adding shinyjs buttons - redirection 2018-05-10
  
  ### fish
  observeEvent(input$submit_fish, {
    updateTabsetPanel(session, "inTabset",
                      selected = "tableTab")
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
      need(!is.null(input$fish_files), " ") #'Please upload at least 1 FISH file!')
    )
    
    f4 <- rv$f1 %>% 
      select(-file_type) %>% 
      spread(chr, num_chr) %>% 
      select(-smpl,smpl) #move this column to the end
    
    #print(head(f4))
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
        
        x_y_axis_lab <- colnames(matr_plot)[4:5]
        
        plt <- create_perc_matr2.1(matr_plot, title = classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                   minChr = 1, 
                                   maxChr = maxChrPlus1, xlab = x_y_axis_lab[1], ylab=x_y_axis_lab[2])
        return(plt)
      })
    })
  }
  
  ### generate rmarkdown report
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
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


shinyApp(ui, server)