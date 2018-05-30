library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
#library(shinycustomloader)
library(RColorBrewer)
require(openxlsx)


#library(DT)
source("scripts/helper_scripts.R")

max_plots <- 50 # *maximum* total number of plots

ui <- tagList(shinyjs::useShinyjs(), 
              withMathJax(), 
              navbarPage(
                title = "aneuvis 0.6",
                
                theme = shinythemes::shinytheme("spacelab"),
                id = "inTabset",
                
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
                         p("The output from aneuvis is divided into 3 parts: Table Summary, Visualization, and Hypothesis Testing"),
                         p("See the image below for an overview of Aneuvis"),
                         h3("Do treatments A and B induce aneuploidy?"),
                         img(src="aneuvis_layout.png", width=700),
                         p("Watch the tutorial below to get started. (forthcoming)")),
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
                           tags$li(tags$b("I am trying to upload an excel file and I get an error that looks like this:")), 
                           img(src="error_1.png", width=200),
                           tags$ul("This could be an issue with the file encoding. Try opening the file and saving it (using 'Save As...') with the same file name (without modifying the file). Then, try re-uploading the file into aneuvis."),
                           tags$li(tags$b("I have an idea to improve aneuvis. How should I share this?")),
                           tags$ul("Email me (daniel.pique@med.einstein.yu.edu) with any suggestions. You can also open an issue on Github or submit a pull request.")
                         )
                         
                ),
                tabPanel("Upload Data", icon=icon("upload"), value = "uploadTab",
                         tabsetPanel(
                           tabPanel("FISH",
                                    h3("Upload fluorescence in situ hybridization (FISH) data"),
                                    p("Note: All FISH files to be compared must be uploaded together; otherwise, files will overwrite each other if uploaded 1 by 1."),
                                    fileInput(
                                      inputId = "fish_files", #files
                                      label = ".xlsx or .xls", #, see file structure guide below",
                                      multiple = TRUE,
                                      accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
                                    ),
                                    actionButton("submit_fish", "Submit and Go to Table Summary"),
                                    #actionButton('reset_fish', 'Reset Input'),
                                    #Resetting input: https://gist.github.com/bborgesr/07406b30ade8a011e59971835bf6c6f7
                                    textOutput("fish_summary"),
                                    hr(),
                                    h3("FISH file structure guide"),
                                    
                                    img(src="fish_layout_excel.png", width=300),
                                    p("Multiple excel files, each with the same # of chromosomes, can be uploaded."),
                                    p("All files *must* have same column names in row 1. Ex. Chr17 and Chr 17 are different"), 
                                    p("Each file will be treated as a separate 'condition'."),
                                    p("The name of each file (before the .xls or .xlsx extension) will be the 'category' "),
                                    hr(),
                                    h3("Download example data"),
                                    
                                    p("Download example 2-chromosome FISH data ", 
                                      tags$a(target = "_blank", 
                                             href = "https://docs.google.com/uc?export=download&id=1ZO9jWicY-5WohvGbQi_WrQWcDaram5wZ", "here"), 
                                      " (zip file)")
                           ),
                           
                           #p("Example FISH data is available for download here")),
                           #),
                           tabPanel("SC-WGS",
                                    h3("Upload single cell whole genome sequencing (sc-wgs) data"),
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
                                    hr(),
                                    h3("Copy number and key file structure guide"),
                                    
                                    #p("The format of a copy number file is shown below."),
                                    img(src="ginkgo_layout.png", width=500),
                                    p("One copy number file can be uploaded at a time."),
                                    h4("Layout of the key"),
                                    img(src="ginkgo_key.png", width=400),
                                    p("Download example sc-WGS data (processed using Ginkgo)", 
                                      tags$a(target = "_blank", 
                                             href= "https://docs.google.com/uc?export=download&id=1VW35NIXSCu7OKaFTSFF_LacwjBqM9JWo",
                                             "here"), 
                                      "(original data on Ginkgo website linked", 
                                      tags$a(target = "_blank", 
                                             href= "http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441",
                                             "here)")), 
                                    p("Download example sc-WGS key", 
                                      tags$a(target = "_blank",
                                             href="https://docs.google.com/uc?export=download&id=1Yon70xRNv693qSjANrADW1WHSHkZ-2Xj", "here"))
                           ),
                           tabPanel("SKY",
                                    h3("Upload spectral karyotype (SKY) data"),
                                    fileInput(
                                      inputId = "sky_file",
                                      label = "Upload SKY File (.xls, .xlsx)", #names must match those in gnko
                                      multiple = FALSE,
                                      accept = c(".xlsx", ".xls")#c(".csv", ".txt", ".tsv")
                                    ), 
                                    actionButton("submit_sky", "Submit and Go to Table Summary"),
                                    hr(),
                                    h3("SKY file structure guide"),
                                    img(src="sky_layout.png", width=800),
                                    p("One SKY copy number file should be uploaded at a time."),
                                    p("Download example SKY data", 
                                      tags$a(target = "_blank", 
                                             href="https://docs.google.com/uc?export=download&id=1hUP9yCWbDeh6Yf2IpR5LtaFs4iFK86tp", "here")),
                                    p("Access a list of International System for Chromosome Nomenclature (ISCN) symbols", 
                                      tags$a(target = "_blank", 
                                             href="https://cgap.nci.nih.gov/Chromosomes/ISCNSymbols", "here"))))),
                
                tabPanel("Table Summary", icon = icon("table"), value = "tableTab",
                         actionButton("returnToDataUpload", "Upload additional datasets"),
                         actionButton("goToVisualization", "Continue to visualization"),
                         downloadButton("stats_report", label="Download statistics (.xlsx)"),
                         selectInput("numberOfX", "Number of X Chromosomes Expected", 
                                     choices=c("1" = "1", "2" = "2"), selected = "2"),
                         selectInput("numberOfY", "Number of Y Chromosomes Expected", 
                                     choices=c("0" = "0", "1" = "1"), selected = "0"),
                         hr(),
                         tabsetPanel(
                           tabPanel("Stats By Group", DT::dataTableOutput("sumryStatsTbl")),
                           tabPanel("Stats By Group & Chromosome", DT::dataTableOutput("sumryStatsTblPerChr")),
                           tabPanel("SC-WGS Chromosome-level Summary", DT::dataTableOutput("g2T"),
                                    p("A 'wide' table of single cell whole genome sequencing (sc-wgs) data is available for download, with chromosomes as columns and samples as rows. 
                                      This table contains the weighted average copy number (by bin size) rounded to the nearest integer per chromosome per sample."),
                                    downloadButton("g2T.d", "Download")
                                    )
                         ), 
                         hr(),
                         p("Each row in this table represents
                           a different file that was uploaded. The columns represent the following:"),
                         
                         img(src="expl_summary_stat_vis.png", width=800),
                         p(
                           tags$ul(
                             tags$li(
                               p("Columns labeled diploid, polyploid, and aneuploid represent the proportion of cells 
                                 in that state per treatment (\\(P_D\\), \\(P_P\\), and \\(P_A\\), respectively).")
                               ),
                             tags$ul(
                               tags$li("Diploid: cells containing 2 copies of all the chromosomes analyzed"),
                               tags$li("Aneuploid: any cell that has at least one chromosome with copy number different than 2, as long as the copy number is not the same for all chromosomes analyzed. 
                                       Note: Cells with 1 copy of all chromosomes analyzed will be classified as aneuploid."),
                               tags$li("Polyploid: cells with matching chromosome copy numbers for all the chromosomes analyzed, as long as they are higher than 2.")
                               ),
                             #),
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
                             )),
                         p("A tabular and visual representation of the summary statistics is shown below"),
                         img(src="expl_summary_stat.png", width=900),
                         img(src="expl_summary_stat3.png", width=900)

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
                                    p("This heatmap represents the number of distinct chromosomal states per group. 
                                      Each column represents a chromosome, and each row represents a distinct chromosomal 
                                      state per group. The proportion of cells within each group that have the given chromosomal 
                                      state is shown on the rightmost plot (square black boxes). The darker the square, 
                                      the greater the proportion of cells within that group that are in that state."),
                                    p("Resize the width of your browser window to have the two plots move closer together"),
                                    
                                    plotOutput("chrHeatG2")),
                           tabPanel("SKY", 
                                    p("This heatmap represents the number of distinct chromosomal states per group. 
                                        Each column represents a chromosome, and each row represents a distinct chromosomal 
                                        state per group. The proportion of cells within each group that have the given 
                                        chromosomal state is shown on the rightmost plot (square black boxes). The darker 
                                        the square, the greater the proportion of cells within that group that are in that state."),
                                    p("Resize the width of your browser window to have the two plots move closer together"),
                                    plotOutput("chrHeatS2")),
                           tabPanel("Sky_test_module",
                                    heatMapUI("sky_test")),
                           tabPanel("scwgs_test_module",
                                  heatMapUI("scwgs_test"))
                           
                           #2018-05-30
                                    )),
                tabPanel("Hypothesis Testing", icon = icon("random"),
                         h3("Are groups statistically significantly different from each other
                            in terms of the degree of numerical aneuploidy?"),
                         p("How to use this page:"),
                         p("Three steps: 1. Select the tab of the data type you would like to permute"),
                         p("2. Select the # of desired permutations (default is 500). More perms will take longer."),
                         p("3. Select the score to permute, then hit 'permute'. This may take a few minutes depending on 
                           the number of permutations."),
                         p("Methods: Generate random permutations of the category associated with each observed cell. 
                           The difference in scores between all possible pairs of categories is calculated after each permutation. 
                           A p-value is calculated by counting how many permuted ANCA scores are more extreme than
                           the observed ANCA score."),
                         p("The p-values is 1-sided, and tests the null hypothesis that there is no significant difference in scores
                           between a given pair of groups. there two possible interpretations of the resulting p-value:
                           not significantly different (p > 0.05, grey color) or significantly different (blue color)."),
                         h4("Key for the table columns"),
                         p("- Group 1 and Group 2 are the groups that are being compared"),
                         p("- nperm_gr_thn_obs is the number of permutations greater than the observed normalized ANCA score"),
                         p("- pvalue is the pvalue rounded to 2 decimal places,	pval_cut is the categorization of the pvalue into bins (for heatmap purposes)"),
                         p("- perm_mean is the mean of the anca scores across all permuted samples"),	
                         p("- perm_dist_2.5% and perm_dist_97.5% are the lower and upper 95% CI for the permuted ANCA scores"),
                         p("- obs_val is the observed difference in ANCA score between the 2 groups"),
                         p("- fold_change is the observed difference in ANCA scores divided by the mean permuted difference in ANCA scores. Analogous to fold enrichment above baseline noise."),
                         
                         tabsetPanel(
                           tabPanel("FISH", 
                                    permPlotTblUI("fish", header = "FISH")),
                           tabPanel("SC-WGS",
                                    permPlotTblUI("sc-wgs", header = "Single Cell Whole Genome Sequencing")),
                           tabPanel("SKY", 
                                    permPlotTblUI("sky", header = "SKY")),
                           tabPanel("Multiplatform summary"),
                           tabPanel("Comparing data types")

                         )
                         )))


server <- shinyServer(function(input, output, session) {
  ###########
  #1. Read in raw ginkgo data
  gR <- eventReactive(input$submit_wgs, ignoreNULL = FALSE, { #var. named with capital R for Reactive
    #validate(need(input$wgs_file != "", "..."))
    if (is.null(input$wgs_file)) {
      return(NULL)
    }
    
    path_list <- as.list(input$wgs_file$datapath)
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
    #validate(need(input$wgs_key != "", "..."))
    
    if (is.null(input$wgs_key)) {
      return(NULL)
    }
    gK <- read_excel(path = input$wgs_key$datapath[1], sheet = 1) #%>%
    #mutate(category = paste0(category, "__sc-wgs"))
    
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
      #filter(chr != "Y") %>% 
      mutate(chr = factor(chr, levels=c(1:22, "X", "Y")))%>% 
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
      spread(key = chr, value = num_chr) #%>%
    colnames(g2.t)[4:ncol(g2.t)] <- paste0("Chr. ", colnames(g2.t)[4:ncol(g2.t)])
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
  
  
  values_fish <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$submit_fish, {
    values_fish$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset_fish, {
    values_fish$upload_state <- 'reset'
  })
  
  file_input_fish <- reactive({
    if (is.null(values_fish$upload_state)) {
      return(NULL)
    } else if (values_fish$upload_state == 'uploaded') {
      return(input$submit_fish)
    } else if (values_fish$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  
  #ff <- eventReactive({input$fish_files,
  #})
                
  output$fish_summary <- renderText({
    if(is.null(values_fish$upload_state)){
      return("No files uploaded. Submit a set of files.")# or resubmit previously uploaded files with 'Submit and Go to Table Summary'")
    } else if(!is.null(file_input_fish())){
      return(paste("Uploaded file(s):", paste0(input$fish_files$name, collapse="; ")))#  map_chr(.x = , .f = paste0, collapse="; ")))
    } else if(is.null(file_input_fish())){
      return("Files cleared. Submit a new set of files or resubmit previously uploaded files with 'Submit and Go to Table Summary'")
    }
  })
    

  #
  #file_input_fish()
  f1R <- eventReactive(input$submit_fish, ignoreNULL=FALSE, {#values_fish$upload_state, ignoreNULL=FALSE, {#file_input_fish(), ignoreNULL = FALSE, {
    #validate(need(input$fish_files != "", "..."))
    
    if (is.null(input$fish_files)) {
      #if (!is.data.frame(input$fish_files)) {
      return(NULL)
    }
    
    maxChr = 8
    maxChrPlus1 = maxChr + 1
    
    path_list <- as.list(input$fish_files$name)
    tbl_list <- lapply(input$fish_files$datapath, read_excel)
    
    #mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr))))
    #df
    #tbl_list[[1]] %>% clean_names %>% rename_at(vars(names(.)), ~ unlist(regmatches(., gregexpr("Y|X|[[:digit:]]+", .))))# %>% 
    
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
  })
  
  
  s1R <- eventReactive(input$submit_sky, ignoreNULL = FALSE, { #reactive({
    #validate(need(input$sky_file != "", "..."))
    #print("class(input$sky_file)")
    #print(class(input$sky_file))
    if (is.null(input$sky_file)) {
      return(NULL)
    }
    
    s1 <- read_excel(input$sky_file$datapath) %>% 
      clean_names() %>%
      filter(rowSums(is.na(.)) <= .50*ncol(.)) %>% #remove rows where > 50% of values are na
      filter(.[,1] != "Chr. No.") %>%
      set_names(nm=.[1,]) %>%
      .[-1,] %>%
      clean_names()
    
    #sample_name <- read_excel(input$sky_file$datapath) %>% names(.)[2]
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
  })
  
  numbX <- reactive({
    input$numberOfX
  })
  numbY <- reactive({
    input$numberOfY
  })
  
  stsTbl <- reactive({
    #list_to_pass2 <- map2(.x = c("sc-wgs", "sky", "fish"), .y =  list_to_pass, .f = ~mutate(.data = .y, file_type = .x))
    #list_to_pass <- list_to_pass2
    #sumStats2 <- purrr::reduce(list(heterog_scores, perc_ploidy, anca_scores, aneupl_scores), full_join, by=c("category", "file_type")) 
    #ggplot(sumStats2, aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
    #                     color = category, shape= file_type)) +  #paste0(file_type, ": ",category))) + #category, shape=file_type)) + 
    #  geom_point( size=4, alpha=0.8) + theme_classic() +
    #  coord_fixed(ratio = 1)
    numX = as.numeric(numbX()) #input$numberOfX)
    numY = as.numeric(numbY()) #input$numberOfY)
    list_to_pass <- list(g2R(), s2R(), f1R()) %>% purrr::compact() #2018-05-05 issue here?
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
  
  output$sumryStatsTbl <- DT::renderDataTable({
    
    validate(
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$wgs_file), 'Please upload at least 1 file!')
    ) 
    DT::datatable(stsTbl(),       
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE))) %>% DT::formatRound(c(4:11), 2)
  })
  
  stsTblPerChr <- reactive({
    numX = as.numeric(numbX())
    numY = as.numeric(numbY()) 
    
    list_to_pass <- list(g2R(), s2R(), f1R()) %>% compact()
    aneupl_scores = purrr:::map_df(.x = list_to_pass, .f = calc_aneupl_score, retChr = TRUE, numX=numX, numY=numY)
    heterog_scores = purrr:::map_df(.x = list_to_pass, .f = calc_heterog_score, retChr = TRUE)
    #anca_scores = purrr:::map_df(.x = list_to_pass, .f = calc_anca_score, retChr = TRUE)
    anca_scores_normalized = purrr::map_df(.x = list_to_pass, .f = calc_anca_score_normalized, retChr = TRUE, numX=numX, numY=numY)
    #select(category, file_type, n, everything())
    sumStats <- purrr::reduce(list(aneupl_scores,heterog_scores, anca_scores_normalized), full_join, by=c("category","file_type", "chr")) %>%
      select(category, file_type, n, everything())
    return(sumStats)
  })
  
  output$sumryStatsTblPerChr <- DT::renderDataTable({
    validate(
      need(!is.null(input$sky_file) | !is.null(input$fish_files) | !is.null(input$wgs_file), 'Please upload at least 1 file!')
    ) 
    DT::datatable(stsTblPerChr(),       
                  filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE))) %>%
      DT::formatRound(5:7, 2)
  })
  
  #2018-05-29
  output$stats_report <- downloadHandler(
    filename =  paste0(Sys.Date(), "-aneuvis-stats.xlsx"), 
    content = function(file) {
      list_of_datasets <- list("Stats By Group" = stsTbl(), "Stats By Group and Chromosome" = stsTblPerChr())
      write.xlsx(list_of_datasets, file = file)
    }
  )
  
  
  
  #output$sumryStatsTblPerChr = downloadHandler('sumStatsTblPerChr-filt.csv', content = function(file) {
  #  s = input$sumryStatsTblPerChr_rows_all
  #  write.csv(stsTblPerChr()[s, , drop = FALSE], file)
  #})
  
  ### Adding ternary plots
  
  
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
  
  #difficult to make ternplot per chromosome 
  
  
  #### 2018-05-06 adding scatterplots for heterogeneity and aneuploidy scores
  output$aneuHeteroSctrPlt <- renderPlot({
    
    p2 <- ggplot(stsTbl(), aes(x= aneupl_score_bakker, y = heterog_score_bakker, 
                               color = category, shape= file_type)) +  #paste0(file_type, ": ",category))) + #category, shape=file_type)) + 
      geom_point( size=4, alpha=0.8) + theme_classic() +
      coord_fixed(ratio = 1)
    
    return(p2)
    #NULL
  })
  
  output$brush_info_aneuHeteroSctrPlt <- renderPrint({
    brushedPoints(data.frame(stsTbl()), input$brush_aneuHeteroSctrPlt)
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
  
  output$brush_info_aneuHeteroSctrPltPerChr <- renderPrint({
    brushedPoints(data.frame(stsTblPerChr()), input$brush_aneuHeteroSctrPltPerChr)
  })
  
  ##### 2018-05-06 adding heatmaps
  g4R <- reactive({
    #validate(
    #  need(!is.null(input$wgs_file), 'Please upload at least 1 sc-wgs file!')
    #) 
    
    #g2_to_g4 <- tryCatch(if(is.null(g2R())){
    #  return(NULL)
    #} else
    #    )
    if (is.null(input$wgs_file)) {
      return(NULL)
    }
    
    
    g2_to_g4 <- g2R() %>%  #g2
      spread(chr, num_chr) %>%
      group_by(category)  %>%
      unite(colPaste, -category, -smpl, -file_type,remove = FALSE) %>% #added -file_type
      count(colPaste) %>%
      mutate(prop = n / sum(n)) %>%
      separate(colPaste, c(1:22, "X", "Y"), sep = "_") %>%
      ungroup() %>%
      mutate(category = paste(row_number(), category, sep="___")) %>%
      gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
      mutate(chr= factor(chr, levels=c(1:22, "X", "Y","n")))  %>%
      mutate(num_chr = as.numeric(num_chr)) %>%
      separate(category,into = c("row_numb", "categ"), sep = "___", remove = FALSE) %>%
      mutate(row_numb=as.numeric(row_numb)) %>%
      arrange(categ, row_numb) %>%
      mutate(category = factor(category,levels=unique(category)))  #2018-05-27
    return(g2_to_g4)
  })
  
  output$chrHeatG2 <- renderPlot({
    #replaced g4R with g2_to_g4 2018-05-13
    #print("g4R():")
    #print(g4R())
    #print(g4R()$chr)
    
    validate(
      need(!is.null(g4R()), 'Please upload at least 1 sc-wgs file!')
    ) 
    #if(is.null(g4R())
    g4.0 <- g4R() %>% 
      mutate(num_chr_filt = ifelse(num_chr > 9, 9, num_chr),
             num_chr_filt = factor(num_chr, levels = 0:9),
             prop2 = cut(prop, breaks = c(seq(0, 0.2, by = 0.05), 0.3, 0.4, 0.5, 1)),
             num_chr_filt2=ifelse(chr == "n", as.character(prop2), as.character(num_chr_filt))) %>%
      mutate(num_chr_filt3 = factor(num_chr_filt2, levels=c(levels(num_chr_filt), levels(prop2)))) #%>%
    
    labels_g4 <- g4R() %>% select(category, categ) %>% distinct()
    
    colors <- c(brewer.pal(n = 9, name = "Blues")[c(5,3)], 
                "white",
                brewer.pal(n = 9, name = "Reds")[3:9], 
                brewer.pal(n = 8, name = "Greys"))
    #2018-05-27
    g4.01 <- ggplot(g4.0, aes(x=chr, y=category, fill=num_chr_filt3)) + 
      geom_tile(color = "white", size = 1) + 
      
      scale_fill_manual(values = colors,drop=FALSE,name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size=9),
                              axis.text.y = element_text(hjust = 1)) + #vjust=0.3, 
      xlab("Chromosome") + ylab("")+ 
      scale_y_discrete(breaks=labels_g4$category,
                       labels=labels_g4$categ, position = "right")
    return(g4.01)
    
    
    
    #g4.1 <- ggplot(filter(g4R(), chr %in% c(1:22,"X", "Y")), aes(x=chr, y=category, 
    #                                                        fill=factor(num_chr, levels=sort(unique(num_chr))))) + 
    #  geom_tile(color = "white", size = 1) + 
    #  scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
    #  theme_classic() + theme(axis.ticks = element_blank(),
    #                          axis.line = element_blank(),
    #                          axis.text.x = element_text(size= 8),
    #                          axis.text.y = element_text(vjust=0.3, hjust = 1)) +
    #  #coord_fixed(ratio = 1) + 
    #  xlab("Chromosome") + ylab("")+ 
    #  theme(legend.position="left", 
    #        plot.margin=grid::unit(c(0,0,0,0), "mm"),
    #        aspect.ratio=1)
    #
    #g4.2 <- ggplot(filter(g4R(), chr == "n"), aes(x=chr, y=category, fill=prop)) +
    #  geom_tile(color = "white", size = 1) + 
    #  scale_fill_gradient(low = "white", high = "black" ) +
    #  theme_classic() + theme(axis.ticks = element_blank(),
    #                          axis.line = element_blank(),
    #                          axis.text.x = element_text(size= 8),
    #                          axis.text.y = element_text(vjust=0.3, hjust = 1)) +
    #  coord_fixed(ratio = 1) + 
    #  xlab("n") + ylab("") + 
    #  scale_y_discrete(position = "right") + 
    #  theme(legend.position="right", 
    #        plot.margin=grid::unit(c(0,0,0,0), "mm"))
    #return(gridExtra::grid.arrange(g4.1, g4.2, ncol=2, widths=c(4,1)))#,layout_matrix=))
  })
  
  ### do the same for sky plots
  s4R <- reactive({
    
   # validate(
   #   need(!is.null(input$sky_file), 'Please upload at least 1 SKY file!')
   # ) 
    if (is.null(input$sky_file)) {
      return(NULL)
    }
    
    s2_to_s4 <- s2R() %>% 
      spread(chr, num_chr) %>%
      group_by(category)  %>%
      unite(colPaste, -category, -smpl, -file_type,remove = FALSE) %>% #added -file_type
      count(colPaste) %>%
      mutate(prop = n / sum(n)) %>%
      separate(colPaste, c(1:22, "X", "Y"), sep = "_") %>%
      ungroup() %>%
      mutate(category = paste(row_number(), category, sep="___")) %>%
      gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
      mutate(chr= factor(chr, levels=c(1:22, "X", "Y","n")))  %>%
      mutate(num_chr = as.numeric(num_chr)) %>%
      separate(category,into = c("row_numb", "categ"), sep = "___", remove = FALSE) %>%
      mutate(row_numb=as.numeric(row_numb)) %>%
      arrange(categ, row_numb) %>%
      mutate(category = factor(category,levels=unique(category))) 

    return(s2_to_s4)
  })
  
  output$chrHeatS2 <- renderPlot({
    
    validate(
      need(!is.null(s4R()), 'Please upload at least 1 sky file!')
    ) 
    print(head(s4R()))
    
    #if(is.null(g4R())
    s4.0 <- s4R() %>% 
      mutate(num_chr_filt = ifelse(num_chr > 9, 9, num_chr),
             num_chr_filt = factor(num_chr, levels = 0:9),
             prop2 = cut(prop, breaks = c(seq(0, 0.2, by = 0.05), 0.3, 0.4, 0.5, 1)),
             num_chr_filt2=ifelse(chr == "n", as.character(prop2), as.character(num_chr_filt))) %>%
      mutate(num_chr_filt3 = factor(num_chr_filt2, levels=c(levels(num_chr_filt), levels(prop2)))) #%>%
    
    labels_s4 <- s4R() %>% select(category, categ) %>% distinct()
    
    colors <- c(brewer.pal(n = 9, name = "Blues")[c(5,3)], 
                "white",
                brewer.pal(n = 9, name = "Reds")[3:9], 
                brewer.pal(n = 8, name = "Greys"))
    #2018-05-27
    s4.01 <- ggplot(s4.0, aes(x=chr, y=category, fill=num_chr_filt3)) + 
      geom_tile(color = "white", size = 1) + 
      
      scale_fill_manual(values = colors,drop=FALSE,name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size=9),
                              axis.text.y = element_text(hjust = 1)) + #vjust=0.3, 
      xlab("Chromosome") + ylab("")+ 
      scale_y_discrete(breaks=labels_s4$category,
                       labels=labels_s4$categ, position = "right")
    return(s4.01)
    
    #g4.1 <- ggplot(filter(s4R(), chr %in% c(1:22,"X", "Y")), aes(x=chr, y=category, 
    #                                                        fill=factor(chr_freq, levels=sort(unique(chr_freq))))) + 
    #  geom_tile(color = "white", size = 1) + 
    #  scale_fill_brewer(type = "div",palette = "RdBu", drop=FALSE, direction = -1, name = "Copy Number") +
    #  theme_classic() + theme(axis.ticks = element_blank(),
    #                          axis.line = element_blank(),
    #                          axis.text.x = element_text(size= 8),
    #                          axis.text.y = element_text(vjust=0.3, hjust = 1)) +
    #  #coord_fixed(ratio = 1) + 
    #  xlab("Chromosome") + ylab("")+ 
    #  theme(legend.position="left", 
    #        plot.margin=grid::unit(c(0,0,0,0), "mm"),
    #        aspect.ratio=1)
    #
    #g4.2 <- ggplot(filter(s4R(), chr == "n"), aes(x=chr, y=category, fill=prop)) +
    #  geom_tile(color = "white", size = 1) + 
    #  scale_fill_gradient(low = "white", high = "black" ) +
    #  theme_classic() + theme(axis.ticks = element_blank(),
    #                          axis.line = element_blank(),
    #                          axis.text.x = element_text(size= 8),
    #                          axis.text.y = element_text(vjust=0.3, hjust = 1)) +
    #  coord_fixed(ratio = 1) + 
    #  xlab("n") + ylab("") + 
    #  scale_y_discrete(position = "right") + 
    #  theme(legend.position="right", 
    #        plot.margin=grid::unit(c(0,0,0,0), "mm"))
    #return(gridExtra::grid.arrange(g4.1, g4.2, ncol=2, widths=c(4,1)))#,layout_matrix=))
  })
  
  #heatMapUI("sky_test"))
  #2018-05-30
  callModule(heatMap, "sky_test", input_df = s2R, file_type = "sky", orig_input = reactive(input$sky_file))
  callModule(heatMap, "scwgs_test", input_df = g2R, file_type = "scwgs", orig_input = reactive(input$wgs_file))
  #callModule(heatMap, "scwgs_test", input_df = f1R, file_type = "fish", orig_input = reactive(input$fish_files))
  
  #heatMap <- function(input, output, session, input_df, file_type){
    
  #### adding permutation plot modules - 2018-05-12 
  callModule(permPlotTbl, "fish", file_input = reactive(input$fish_files), 
             input_df = f1R, nPerms = reactive(input$Nperms))

  callModule(permPlotTbl, "sc-wgs", file_input = reactive(input$wgs_file), 
             input_df = g2R, nPerms = reactive(input$Nperms))
  
  callModule(permPlotTbl, "sky", file_input = reactive(input$sky_file),
             input_df = s2R, nPerms = reactive(input$Nperms))
  
  ###### adding shinyjs buttons - redirection 2018-05-10
  
  observeEvent(input$submit_fish, {
    updateTabsetPanel(session, "inTabset",
                      selected = "tableTab")
  })
  
  ### fish
  observe({
    shinyjs::toggleState("submit_fish", !is.null(input$fish_files))
  })
  
  observeEvent(input$submit_wgs, {
    updateTabsetPanel(session, "inTabset",
                      selected = "tableTab")
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
  
  observeEvent(input$returnToDataUpload, {
    updateTabsetPanel(session, "inTabset",
                      selected = "uploadTab")
  })
  
  observeEvent(input$goToVisualization, {
    updateTabsetPanel(session, "inTabset",
                      selected = "visTab")
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
        file_names <- input$fish_files$name
        
        maxChr <- 8
        maxChrPlus1 = maxChr + 1
        nchrs <-  length(unique(f1R()$chr))
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
                     g4 = g4R())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }#, input$fish_files$name
  )
      
  
})


shinyApp(ui, server)
#app <- shinyApp(ui, server)
#runApp(app, display.mode = "showcase")
#R -e "shiny::runApp('app.R')"

# file_name / smpl_id - done!
# add chr name to header - 
# polyploid, diploid and aneuploid
# rearrange

# remove negative sign from the p-value

# signficantly similar? no. 

#Elaine's paper
#why we cant reproduce figure 1?
#send the new figure