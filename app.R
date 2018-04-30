library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)

source("scripts/helper_scripts.R")

tabPanelFishMaster <- function(x){

  tabsetPanel(
    tabPanel("Grid Plots", uiOutput("gridPlots")),
    tabPanel("Grid Plots w grid.arrange", plotOutput("gridPlots2")),
    tabPanel(
      "Summary statistics",
      tableOutput("ft"),
      p(
        "Each column in this table represents
        a different file that was uploaded. The rows represent the following:",
        
        tags$ul(
          tags$li(
            "The first 3 rows represent
            the proportion of diploid (2n), polyploid, and aneuploid cells in
            each sample."
          ),
          tags$li(
            "The 4th row represents entropy, which was calculated
            using the values in the first 3 rows."
          ),
          tags$li(
            "The 5th row (n) represents the total number of cells analyzed within the file."
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
          ),
          tags$li(
            "The instability index (instab_idx_bayani) was calculated as in",
            tags$a(
              target = "_blank",
              href = "https://www.ncbi.nlm.nih.gov/pubmed/18813350",
              "Bayani et al 2008"
            ), "and", tags$a(
              target = "_blank",
              href = "https://www.ncbi.nlm.nih.gov/pubmed/9121588",
              "Langauer et al 1997"
            ), ". I believe this is equivalent to the anca_score."
          )
          ),
        "Also, none of these methods weigh the number of chromosomes/degree of aneuploidy, 
      which could present an opportunity for developing a new index."
      )
    ),
    tabPanel("Ternary Plot", plotOutput("ternPlot")),
    tabPanel("Entropy Plot", plotOutput("ploidyPlot")))
}



max_plots <- 50 # *maximum* total number of plots

ui <- fluidPage(
  title = "aneuvis",
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("spacelab"),
  titlePanel("aneuvis v.0.4"),
  sidebarPanel(
    radioButtons(
      inputId = "rb",
      label = "1. Select data type:",
      c(
        "2, 3, or 4-chromosome FISH" = "fish2",
        "Ginkgo: Single-cell CNV" = "ginkgo",
        "SKY" = "sky"
      )
    ),
    conditionalPanel(
      condition = "input.rb == 'fish2'",
      fileInput(
        inputId = "fish_files", #files
        label = "2. Upload FISH data (.xlsx or .xls)",
        multiple = TRUE,
        accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
      )),
    
    conditionalPanel(
      condition = "input.rb == 'ginkgo'",
      fileInput(
        inputId = "gnk_file",
        label = "2a. Upload Ginkgo Copy Number File (.txt)", #names must match those in gnko
        multiple = FALSE,
        accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
      ), fileInput(
        inputId = "gnk_key",
        label = "2b. Upload Ginkgo Key (xls or xlsx)", #names must match those in gnko
        multiple = FALSE,
        accept = c(".xlsx", ".xls")#, ".csv", ".txt", ".tsv")
      )),
    conditionalPanel(
      condition = "input.rb == 'sky'",
      fileInput(
        inputId = "sky_file", #files
        label = "2. Upload Sky data (.xlsx or .xls)",
        multiple = FALSE,
        accept = c(".xlsx", ".xls")
      )),
    actionButton("submit", "Submit"),

    p("Download example 2-chromosome FISH data ", 
      tags$a(target = "_blank", 
             href = "https://docs.google.com/uc?export=download&id=1CKh6feR7AmndtAvoF4Y-EFxaigjiFmba", "here"), 
      " (zip file)"),
    p("Download example Ginkgo data", 
      tags$a(target = "_blank", 
             href="http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441", "here"), 
      "(save as '.txt' file)"),
    p("Download example Ginkgo key", 
      tags$a(target = "_blank",
             href="https://docs.google.com/uc?export=download&id=1bhOEye8FR3Ut9w_hT-SMab2AaUEx9_5x", "here"))
  ),
  
  mainPanel(
    conditionalPanel(condition = "input.rb == 'fish2'",
                     tabPanelFishMaster()),
    conditionalPanel(condition = "input.rb == 'ginkgo'",
                     tabsetPanel(
                       tabPanel("Chromosomal Copy Number Heat Map",
                                plotOutput("g4P")),
                       tabPanel("Sample x Chromosome Summary Table", 
                                p("The table below gives the weighted average copy number per chromosome per sample.
                                  The table is also available for download."),
                                downloadButton("g2T.d", "Download"),
                                tableOutput("g2T")),
                       tabPanel("Summary statistics",
                        p("ginkgo_table:"),
                        downloadButton("g10T.d", "Download"),
                         tableOutput("gnk_score_tbl"))
                      )),
    conditionalPanel(condition = "input.rb == 'sky'",
                     tabsetPanel(
                       tabPanel("Plots",
                                plotOutput("skyPlotChr")),
                       tabPanel("Data Table",
                                tableOutput("s1R.t")),
                       tabPanel("Summary Statistics",
                                tableOutput("s7T"))
                     ))
    )
)



server <- shinyServer(function(input, output) {
  ###########
  #1. Read in raw ginkgo data
   gR <- eventReactive(input$submit, { #var. named with capital R for Reactive
    validate(need(input$gnk_file != "", "..."))
    
    if (is.null(input$gnk_file)) {
      return(NULL)
    }
    if(tools::file_ext(input$gnk_file$datapath[1]) %in% c("txt")){

      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      path_list <- as.list(input$gnk_file$datapath)
      #if(interactive()){
      #  path_list <- as.list( here::here("testDat/ginkgo_test_dat_copy_number.txt"))
      #}
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(path_list, read_delim, delim="\t")
      
      g <- map2(.x = path_list, .y= tbl_list,
                      .f = ~data.frame(clss=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        .[,colSums(!is.na(.)) > 0] %>%
        select(-clss)

      return(g)
    }
  })

   #2. read in the ginkgo key 
   
   gKR <- reactive({#eventReactive(input$submit, { #g for ginkgo K for Key, R for reactive
     validate(need(input$gnk_key != "", "..."))
     
     if (is.null(input$gnk_key)) {
       return(NULL)
     }
     gK <- read_xlsx(path = input$gnk_key$datapath[1], sheet = 1)
     
     print("head(gk):")
     print(head(gK))
     return(gK)
   })
   
   
  g2R <- reactive({
    g2 <- gR() %>% 
      gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
      group_by(CHR, smpl) %>% 
      mutate(reg_size = END - START) %>% 
      summarise(avg = weighted.mean(cp_nm,reg_size)) %>% 
      mutate(avgRound = round(avg)) %>%
      mutate(avgRound = ifelse(avgRound > 2, 1, ifelse(avgRound == 2, 0, -1))) %>%
      separate(CHR, c("chrRm", "chr"), sep=3) %>% 
      dplyr::select(-chrRm) %>% 
      filter(chr != "Y") %>% 
      mutate(chr = factor(chr, levels=c(1:22, "X")), 
             avgRound=factor(avgRound)) %>%
      left_join(gKR(), c("smpl" = "file_name"))
    return(g2)
  })
   
  g2.1R <- reactive({
    g2.t <- g2R() %>% select(-avgRound) %>%
      spread(key = chr, value = avg)
    return(g2.t)
  })
  
  output$g2T <- renderTable({
    g2.1R()
    })
  

  
  output$g2T.d <- downloadHandler(
    filename = function() {
      paste("ginkgo-chr-summary-",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(g2.1R(), file, row.names = FALSE)
    }
  )
  
  
 # output$g2P <- renderPlot({
 #   g2.p <- g2R() %>%
 #     select(chr, avgRound, category) %>%
 #     group_by(chr, avgRound, category) %>%
 #     summarise (n = n()) %>%
 #     mutate(freq = n / sum(n)) %>% 
 #     spread(key = avgRound, value = freq)
 #   
 #   g2.p <- g2R() %>%
 #     select(chr, avgRound, category) %>%
 #     group_by(chr, avgRound, category) %>%
 #     summarise (n = n()) %>%
 #     ungroup() %>%
 #     group_by(category, chr) %>%
 #     mutate(freq = n / sum(n)) %>% 
 #     mutate(freq_dir = freq*as.numeric(as.character(avgRound))) %>%
 #     arrange(chr, category, avgRound)
 #     #spread(key = avgRound, value = freq)
 #   ggplot(g2.p, aes(x=avgRound,y = freq, color=chr)) + 
 #     geom_jitter(alpha=0.5) + 
 #     geom_line(aes(group=chr))# , color=category))
 #   ggplot(g2.p, aes(y=avgRound,x = freq)) + 
 #     geom_jitter(alpha=0.5, aes(color=chr)) + 
 #     geom_line(aes(group=chr, color=category))# , color=category))
 #   
 #   
 # })
  
  g3R <- reactive({
    g3 <- g2R() %>%
      dplyr::select(avgRound, chr, category, smpl) %>%
      spread(chr, avgRound)
    return(g3)
  })
    
  g4R <- reactive({
    g4 <- g3R() %>% 
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
    return(g4)
  })


  output$g4P <- renderPlot({ #plot
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
            #aspect.ratio=1)
    #cowplot::plot_grid(g5.1, g5.2, labels = c("A", "B"), rel_widths = c(1, 1))
    #cowplot::plot_grid(g5.1, g5.2, align = "h", nrow = 1, ncol = 2, rel_widths = c(5, 1/4))
    return(gridExtra::grid.arrange(g4.1, g4.2, ncol=2, widths=c(4,1)))#,layout_matrix=))
  })
  
  g5R <- reactive({
    g5 <- gR() %>% #g5 joins the 2 data frames 
      gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
      group_by(CHR, smpl) %>% 
      separate(CHR, c("chrRm", "chr"), sep=3) %>% 
      dplyr::select(-chrRm) %>% 
      filter(chr != "Y") %>% 
      mutate(chr = factor(chr, levels=c(1:22, "X"))) %>%
      left_join(gKR(), c("smpl" = "file_name"))   
  })

  g6R <- reactive({
    
    g6 <- g5R() %>% #aneuploidy score for ginkgo data!! 2018-04-26
      mutate(ideal_nchr = 2) %>%
      mutate(ideal_obs_diff = abs(ideal_nchr - cp_nm)) %>%
      group_by(category) %>% 
      unite(col = bins, chr, START, END, remove = FALSE) %>%
      summarize(aneupl_score_bakker = sum(ideal_obs_diff) / (length(unique(bins))*length(unique(smpl))))
    return(g6)
  })
  
  g7R <- reactive({
    g7 <- g5R() %>% #heterogeneity score for ginkgo data!! 2018-04-26
      unite(col = bins, chr, START, END, remove = FALSE) %>%
      group_by(category, bins, cp_nm) %>%
      summarize(mft=n()) %>% 
      arrange(category, bins,  mft, cp_nm) %>%
      ungroup() %>%
      group_by(category, bins) %>%
      mutate(f = rev(1:n()-1)) %>%
      mutate(mft_f = mft * f) %>%
      ungroup() %>%
      group_by(category) %>%
      summarize(heterog_score_bakker = sum(mft_f)/ (length(unique(bins))*(ncol(gR()) - 3)))
    return(g7)
  })

  g8R <- reactive({ #anca_score_blegen
    g8 <- g5R() %>%
      mutate(diploid_bin = cp_nm == 2) %>%
      group_by(category, diploid_bin) %>% 
      summarise (n = n()) %>%
      spread(key = diploid_bin, value=n) %>%
      clean_names() %>%
      mutate(anca_score_blegen = false/ (true + false)) %>% 
      select(category, anca_score_blegen)
    return(g8)
  })
  
  g9R <- reactive({ #n per group
    g9 <- g5R() %>%
      select(category, smpl) %>%
      distinct() %>%
      group_by(category) %>%
      count()
    return(g9)
  })
  
  g10R <- reactive({ #ploidy per group
    g10 <- g3R() %>%
      mutate_at(3:ncol(.), as.numeric) %>%
      mutate(ploidy =  apply(.[,3:ncol(.)], 1, classifPloidy)) %>% 
      select(category, ploidy) %>% 
      mutate(category = factor(category), 
             ploidy = factor(ploidy, levels=c("diploid", "polyploid", "aneuploid"))) %>%
      group_by(category, ploidy) %>%
      summarise (n = n()) %>%
      mutate(freq = n / sum(n)) %>%
      tidyr::complete(ploidy, fill = list(n = 0, freq=0)) %>%
      distinct() %>%
      select(category, ploidy, freq) %>%
      spread(key = ploidy, value = freq)
    return(g10)
  })
  
  g20R <- reactive({
    purrr::reduce(list(g6R(), g7R(), g8R(), g9R(), g10R()), full_join, by="category")
  })
    
  output$gnk_score_tbl <- renderTable({
    g20R()
  })
  
  
  output$g10T.d <- downloadHandler(
    filename = function() {
      paste("ginkgo-stats-summary-",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(g20R(), file, row.names = FALSE)
    }
  )
  
  
  
  
if(interactive()){
      path_list <- as.list( here::here("testDat/ginkgo_test_dat_copy_number.txt"))

      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(path_list, read_delim, delim="\t")
      
  g <- map2(.x = path_list, .y= tbl_list,
            .f = ~data.frame(clss=.x, .y)) %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    .[,colSums(!is.na(.)) > 0] %>%
    select(-clss)
  gK <- read_xlsx(path = here::here("testDat/ginkgo_key.xlsx"), sheet = 1)
  
  g2 <- g %>% 
    gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
    group_by(CHR, smpl) %>% 
    summarise(avg = mean(cp_nm)) %>% 
    mutate(avgRound = round(avg)) %>%
    mutate(avgRound = ifelse(avgRound > 2, 1, ifelse(avgRound == 2, 0, -1))) %>%
    separate(CHR, c("chrRm", "chr"), sep=3) %>% 
    dplyr::select(-chrRm) %>% 
    filter(chr != "Y") %>% 
    mutate(chr = factor(chr, levels=c(1:22, "X")), 
           avgRound=factor(avgRound)) %>%
    left_join(gK, c("smpl" = "file_name"))
  
  g3 <- g2 %>%
    dplyr::select(avgRound, chr, category, smpl) %>%
    spread(chr, avgRound)
  
  g4 <- g3 %>% 
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
  
  
  ggplot(filter(g4, chr %in% c(1:22,"X")), aes(x=chr, y=category, 
                                               fill=factor(chr_freq, levels=sort(unique(chr_freq))))) + 
    geom_tile(color = "white", size = 1) + 
    scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.line = element_blank(),
                            axis.text.x = element_text(size= 8),
                            axis.text.y = element_text(vjust=0.3, hjust = 1)) +
    coord_fixed(ratio = 1) + xlab("Chromosome") + ylab("")
  
  ggplot(filter(g4, chr == "n"), aes(x=chr, y=category, fill=prop)) + 
    geom_tile(color = "white", size = 1) + 
    scale_fill_gradient(low = "white", high = "black" ) +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.line = element_blank(),
                            axis.text.x = element_text(size= 8),
                            axis.text.y = element_text(vjust=0.3, hjust = 1)) +
    coord_fixed(ratio = 1) + xlab("Chromosome") + ylab("")
  
  #aneupl_score
  
  aneuploidy_score_bakker <- f1R.t %>%
    gather(key = "bins", value= "nchr", 2:3) %>%
    mutate(ideal_nchr = 2) %>%
    mutate(ideal_obs_diff = abs(ideal_nchr - nchr)) %>%
    group_by(clss) %>% 
    summarize(sum_ideal_obs_diff = sum(ideal_obs_diff), n_bins_times_n_cells_per_group = n()) %>%
    mutate(aneupl_score_bakker = sum_ideal_obs_diff / n_bins_times_n_cells_per_group)  
  
  print(aneuploidy_score_bakker)
  
  

  
  ########
  heterogeneity_score_bakker <- f1R.t %>%
    select(-variable_) %>% 
    gather(key = "bins", value = "numChr", 2:3) %>%
    group_by(clss, bins, numChr) %>%
    summarize(mft=n()) %>% 
    arrange(clss, bins, mft,  numChr) %>% #category, bins,  mft, cp_nm
    ungroup() %>%
    group_by(clss, bins) %>%
    mutate(f = rev(1:n()-1)) %>%
    mutate(mft_f = mft * f) %>%
    ungroup() %>%
    group_by(clss) %>%
    summarize(heterog_score_bakker = sum(mft_f)/ (sum(mft)*length(unique(bins)))) #%>%
    #spread(key = clss, value=heterog_score_bakker) %>%
    #mutate(variable_=as.factor("heterog_score_bakker")) %>%
    #select(variable_, everything())

  g5 <- g %>% #g5 joins the 2 data frames 
    gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
    group_by(CHR, smpl) %>% 
    separate(CHR, c("chrRm", "chr"), sep=3) %>% 
    dplyr::select(-chrRm) %>% 
    filter(chr != "Y") %>% 
    mutate(chr = factor(chr, levels=c(1:22, "X"))) %>%
    left_join(gK, c("smpl" = "file_name"))   
   
  g6 <- g5 %>% #aneuploidy score for ginkgo data!! 2018-04-26
    mutate(ideal_nchr = 2) %>%
    mutate(ideal_obs_diff = abs(ideal_nchr - cp_nm)) %>%
    group_by(category) %>% 
    unite(col = bins, chr, START, END, remove = FALSE) %>%
    summarize(aneupl_score_bakker = sum(ideal_obs_diff) / (length(unique(bins))*length(unique(smpl))))
  
  
  g7 <- g5 %>% #heterogeneity score for ginkgo data!! 2018-04-26
    unite(col = bins, chr, START, END, remove = FALSE) %>%
    group_by(category, bins, cp_nm) %>%
    summarize(mft=n()) %>% 
    arrange(category, bins,  mft, cp_nm) %>%
    ungroup() %>%
    group_by(category, bins) %>%
    mutate(f = rev(1:n()-1)) %>%
    mutate(mft_f = mft * f) %>%
    ungroup() %>%
    group_by(category) %>%
    summarize(heterog_score_bakker = sum(mft_f)/ (length(unique(bins))*(ncol(g) - 3)))
  
  g8 <- g5 %>%
  mutate(diploid_bin = cp_nm == 2) %>%
    group_by(category, diploid_bin) %>% 
    summarise (n = n()) %>%
    spread(key = diploid_bin, value=n) %>%
    clean_names() %>%
    mutate(anca_score_blegen = false/ (true + false)) %>% 
    select(category, anca_score_blegen)
  
  g9 <- g5 %>%
    select(category, smpl) %>%
    distinct() %>%
    group_by(category) %>%
    count()
  
  g10 <- g3 %>%
    mutate_at(3:ncol(.), as.numeric) %>%
    mutate(ploidy =  apply(.[,3:ncol(.)], 1, classifPloidy)) %>% 
    select(category, ploidy) %>% 
    mutate(category = factor(category), 
           ploidy = factor(ploidy, levels=c("diploid", "polyploid", "aneuploid"))) %>%
    group_by(category, ploidy) %>%
    summarise (n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    tidyr::complete(ploidy, fill = list(n = 0, freq=0)) %>%
    distinct() %>%
    select(category, ploidy, freq) %>%
    spread(key = ploidy, value = freq)

}


  #sky_file
  s1R <- eventReactive(input$submit, { #reactive({
    validate(need(input$sky_file != "", "..."))
    
    if (is.null(input$sky_file)) {
      return(NULL)
    }
    
    maxChr = 8
    maxChrPlus1 = maxChr + 1
    
    #path_list <- as.list(input$sky_file$name)
    #tbl_list <- lapply(input$sky_file$datapath, read_xlsx)
    s1 <- read_xlsx(input$sky_file$datapath) %>% 
      clean_names() %>%
      filter(rowSums(is.na(.)) <= .50*ncol(.)) %>% #remove rows where > 50% of values are na
      filter(.[,1] != "Chr. No.") %>%
      set_names(nm=.[1,]) %>%
      .[-1,] %>%
      clean_names()
    sample_name <- read_xlsx(input$sky_file$datapath) %>% names(.)[2]
    print(sample_name)
    
     return(s1)

  })

  s1R.categ <- reactive({
    input$sky_file$name
  })
  
  output$s1R.t <- renderTable({s1R()})
  
  s2R <- reactive({
    s2 <- s1R() %>% 
      mutate_at(vars(starts_with("x")), 
                .funs = funs(ifelse(str_detect(., ","), 
                str_split_fixed(., ",", n=2)[1,1], .))) %>%
      mutate_at(vars(starts_with("x")), .funs = as.numeric)
    return(s2)
  })
  
  
  
  #sky_tot_chr <- c("tot", colSums(sky_cn_per_cell[,-1]))
  s3R <- reactive({
    s3 <- s2R() %>%  #can print this out!! 
      mutate_at(vars(starts_with("x")), .funs = as.numeric) %>%
      mutate(cell = sapply(strsplit(cell, " "), "[[", 2)) %>%
      rename("chr" = "cell")
    return(s3)
  }) 
  
  s4R <- reactive({ #sky_cn_per_cell.g
    s4 <- s3R() %>%
      gather(key = "smpl", value = "cp_nm", starts_with("x")) %>%
      mutate(category = s1R.categ()) %>%
      mutate(chr = factor(chr, levels = c(1:22, "X", "Y"))) %>%
      mutate(cp_nm = factor(cp_nm, levels=0:9))
    return(s4)
  }) 
  
  s5R <- reactive({
    s5 <- s4R() %>% spread(key=chr, value=cp_nm) %>%
      mutate(variable_ =  apply(.[,3:(ncol(.))], 1, classifPloidy))
    return(s5)
  })
  
  s6R <- reactive({
    s6 <- s5R() %>%
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
    return(s6)
  })
  
  s7R <- reactive({ #aneuploidy score, per class
   s7 <-  s4R() %>%
    mutate(ideal_nchr = 2) %>%
    mutate(cp_nm = as.numeric(as.character(cp_nm))) %>%
    mutate(ideal_obs_diff = abs(ideal_nchr - cp_nm)) %>%
    group_by(category) %>% 
    summarize(aneupl_score_bakker = sum(ideal_obs_diff) / (length(unique(chr))*length(unique(smpl))))
   return(s7)
  })
  
  s8R <- reactive({  #heterogeneity score
    s8 <- s4R() %>% #heterogeneity score for ginkgo data!! 2018-04-26
      group_by(category, chr, cp_nm) %>%
      summarize(mft=n()) %>% 
      arrange(category, chr,  mft, cp_nm) %>%
      ungroup() %>%
      group_by(category, chr) %>%
      mutate(f = rev(1:n()-1)) %>%
      mutate(mft_f = mft * f) %>%
      ungroup() %>%
      group_by(category) %>%
      summarize(heterog_score_bakker = sum(mft_f)/ (length(unique(chr))*(length(unique(s4R()$smpl)))))
    return(s8)
  })
  
  s9R <- reactive({  #anca_score_sky
    s4 <- s4R() %>% #anca score
      mutate(diploid_bin = as.numeric(as.character(cp_nm)) == 2) %>%
      group_by(category, diploid_bin) %>% 
      summarise (n = n()) %>%
      spread(key = diploid_bin, value=n) %>%
      clean_names() %>%
      mutate(anca_score_blegen = false/ (true + false)) %>% 
      select(category, anca_score_blegen)
    return(s4)
  })

  s10R <- reactive({  #dipl_aneupl_poly proportions
    s10 <- s5R() %>% 
      mutate(variable_= factor(variable_, levels=c("diploid", "polyploid", "aneuploid"))) %>%
      select(category, variable_) %>% 
      group_by(category, variable_) %>% 
      count() %>%
      complete(variable_,fill = list(n=0)) %>%
      mutate(freq = n / sum(n)) %>%
      select(-n) %>%
      replace_na(replace = list(freq = 0)) %>%
      spread(variable_, freq)
    return(s10)
  })
  
  s11R <- reactive({ #count of number of groups
    s11 <- s5R() %>% group_by(category) %>% count()
    return(s11)
  })
  
  output$s7T <- renderTable({
    s7 <- purrr::reduce(list(s7R(),s8R(), s9R(), s10R(), s11R()), full_join, by="category") 
    return(s7)                        #sky_cn_per_cell.g.dipl_aneupl_poly, sky_cn_per_cell.g.heterog_score, sky_cn_per_cell.g.aneupl_score, anca_score_sky), full_join, by="category")
  })
  
  
  
  output$skyPlotChr <- renderPlot({
    p1 <- ggplot(filter(s6R(), chr %in% c(1:22,"X")), aes(x=chr, y=category, 
                                                    fill=factor(chr_freq, levels=sort(unique(chr_freq))))) + 
      geom_tile(color = "white", size = 1) + 
      scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("Chromosome") + ylab("")
    
    p2 <- ggplot(filter(s6R(), chr == "n"), aes(x=chr, y=category, fill=prop)) + 
      geom_tile(color = "white", size = 1) + 
      scale_fill_gradient(low = "white", high = "black", limits = c(0,1)) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("Chromosome") + ylab("") + geom_text(aes(label=chr_freq))
    return(gridExtra::grid.arrange(p1, p2, ncol=2, widths=c(4,1))) #,layout_matrix=))
  })
  
  
  
  
  
  ######single cell FISH 
  f1R <- eventReactive(input$submit, { #reactive({
    validate(need(input$fish_files != "", "..."))
    
    if (is.null(input$fish_files)) {
      return(NULL)
    }
      
      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      path_list <- as.list(input$fish_files$name)
      tbl_list <- lapply(input$fish_files$datapath, read_xlsx)
      
      f1 <- map2(.x = path_list, .y= tbl_list,
                      .f = ~data.frame(clss=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        clean_names() %>%
        mutate(variable_ =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
        mutate_at(.vars = vars(starts_with("Chr")), 
                  .funs = ~ifelse(. == 0, 1, 
                                   ifelse(. <= maxChr, ., maxChrPlus1)))
      #print(head(aneuDat))
      return(f1)
    #}
  })
  
   f2R <- reactive({
     f2 <- f1R() %>% 
      mutate(variable_= factor(variable_, levels=c("diploid", "polyploid", "aneuploid"))) %>%
      group_by(clss, variable_) %>%
      summarise(n = n()) %>%
      complete(variable_) %>% 
      replace(is.na(.), 0) %>%
      mutate(freq = n / sum(n)) %>% 
      ungroup %>%
      select(-n) %>%
      spread(key = variable_, value = freq) %>%
      replace(is.na(.), 0)
    return(f2)
    # add a 0 here
  })
  
  
  f3R <- reactive({
    f3 <- f2R() %>%
      dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) #%>%
     return(f3)
  })
  
  output$ploidyPlot <- renderPlot({

    brw_clrs <- c(rev(RColorBrewer::brewer.pal(n = 8, name = "Purples")), "#ffffff",
                  RColorBrewer::brewer.pal(n = 8, name = "Oranges"))
    
    f3R() %>%
      gather(key = "variable_", value = "prop", 2:5) %>%
      mutate(variable_ = factor(variable_, 
                             levels = rev(c("diploid", "polyploid", "aneuploid", "entropy")))) %>%
      mutate(prop2 = ifelse(variable_ == "entropy", -prop, prop)) %>%
      mutate(prop_cut = cut(.$prop2, breaks = c(-Inf, seq(-1, -0.1, .15), -1e-6, 1e-6, 
                                                seq(0.1, 1, .15), Inf), 
                            labels = c(paste0(">", c(seq(1, 0.1, -.15),0)), 0, 
                                       rev(paste0(">", c(seq(1, 0.1, -.15),0), " ")))),
             prop_cut = factor(prop_cut, levels = rev(levels(prop_cut))))  %>%
      ggplot(aes(x=clss, y=variable_, fill=prop_cut)) + 
      geom_tile(color="black") + 
      geom_text(aes(label = round(prop, 2))) + 
      scale_fill_manual(values = brw_clrs, drop=FALSE, name = "") + 
      theme_classic() + 
      coord_fixed() + 
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            line = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1)) + 
      guides(colour = guide_legend(reverse=T))

  })
  
  
  output$ternPlot <- renderPlot({
    p <- ggtern() + 
      geom_point(data=f2R(), aes(x = aneuploid,y=diploid,z=polyploid,
                     fill = clss, label = clss), 
                 size = 3, alpha = 0.4, pch= 21, color = "black", stroke = 1) + 
      limit_tern(1.03,1.03,1.03) + 
      xlab("") + 
      Tlab("Diploid") +
      Llab("Aneuploid") +
      Rlab("Polyploid") 
    return(p)
    NULL
  })
  
  
  f6R <- reactive({ #aneupl_score_bakker
    f6 <- f1R() %>%
      gather(key = "bins", value= "nchr", 2:(ncol(.)-1)) %>%
      mutate(ideal_nchr = 2) %>%
      mutate(ideal_obs_diff = abs(ideal_nchr - nchr)) %>%
      group_by(clss) %>% 
      summarize(sum_ideal_obs_diff = sum(ideal_obs_diff), n_bins_times_n_cells_per_group = n()) %>%
      mutate(aneupl_score_bakker = sum_ideal_obs_diff / n_bins_times_n_cells_per_group) %>%
      select(clss, aneupl_score_bakker)
    return(f6)
  })

  f7R <- reactive({ #heterogeneity_score_bakker
    f7 <- f1R() %>%
      select(-variable_) %>% 
      gather(key = "bins", value = "numChr", 2:(ncol(.)-1)) %>%
      group_by(clss, bins, numChr) %>%
      summarize(mft=n()) %>% 
      arrange(clss, bins, mft,  numChr) %>% #category, bins,  mft, cp_nm
      ungroup() %>%
      group_by(clss, bins) %>%
      mutate(f = rev(1:n()-1)) %>%
      mutate(mft_f = mft * f) %>%
      ungroup() %>%
      group_by(clss) %>%
      summarize(heterog_score_bakker = sum(mft_f)/ (sum(mft)*length(unique(bins)))) 
    return(f7)
  })
  
  f8R <- reactive({
    f8 <- f1R() %>% #anca score
      select(-variable_) %>% 
      gather(key = "chrom", value= "nchr", 2:(ncol(.)-1)) %>%
      mutate(diploid_bin = nchr == 2) %>%
      group_by(clss, diploid_bin) %>% 
      summarise (n = n()) %>%
      spread(key = diploid_bin, value=n) %>%
      clean_names() %>%
      mutate(anca_score_blegen = false/ (true + false)) %>% 
      select(clss, anca_score_blegen)
    return(f8)
  }) 
  
  f9R <- reactive({ #n_samples_per_grp
     f9 <- f1R() %>% 
      group_by(clss) %>% 
      summarise (n = n())
     return(f9)
  })

  
  f20R <- reactive({
    #print(f6R())
    purrr::reduce(list(f6R(), f7R(), f8R(), f9R(), f3R()), full_join, by="clss")
  })
  
  output$ft <- renderTable({
    f20R()
  })
  
    
  
  #download table
  
  #set up the container for the plots
  
  output$gridPlots2 <- renderPlot({
    classes <- unique(f1R()$clss)
    file_names <- input$fish_files$name #input$fish_files$name})
    
    maxChr <- 8
    maxChrPlus1 = maxChr + 1
    nchrs <- f1R() %>% ncol(.) - 2
    chr_pairs <- combn(1:nchrs, 2)
    
    
    plot_list <- list()
    for(ea_chr_pair in 1:ncol(chr_pairs)){
      f1R.t2 <- f1R() %>% select(c(1, chr_pairs[,ea_chr_pair]+1), ncol(.))

      for (ea_class in 1:length(classes)){
        matr_plot <- return_chr_prop_matr(f1R.t2,classes[ea_class], maxPair = maxChrPlus1)
        plt <- create_perc_matr2(matr_plot, title = classes[ea_class], minChr = 1, 
                                 maxChr = maxChrPlus1, xlab = "", ylab="")
        #save plotted object
        plot_list[[ea_class + (length(classes)*(ea_chr_pair - 1))]] <- plt
      
      }
    }
    #plot_list.arr <- grid.arrange(grobs = plot_list, ncol = ncol(chr_pairs)) ## display plot
    plot_list.arr <- grid.arrange(grobs = plot_list, ncol =1) ## display plot
    
    return(plot_list.arr)
    })
  #})
  

  classes <- reactive({unique(f1R()$clss)})
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  file_names <- reactive({input$fish_files$name})

  output$gridPlots <- renderUI({
    nchrs <- f1R() %>% ncol(.) - 2
    chr_pairs <- combn(1:nchrs, 2)
    
    cl_ln <- length(unique(f1R()$clss))
    plot_output_list <- lapply(1:(cl_ln*ncol(chr_pairs)), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    #create a tagList of all plots
    do.call(tagList, plot_output_list)
  })
  
  
all_combos_chr_pairs_and_classes <- reactive({
    nchrs <- f1R() %>% ncol(.) - 2
    chr_pairs <- combn(1:nchrs, 2)
    classes <- unique(f1R()$clss)
    expand.grid(1:ncol(chr_pairs),1:length(classes))
  })

  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        
        classes <- unique(f1R()$clss)
        file_names <- input$fish_files$name #input$fish_files$name})
        
        maxChr <- 8
        maxChrPlus1 = maxChr + 1
        nchrs <- f1R() %>% ncol(.) - 2
        chr_pairs <- combn(1:nchrs, 2)
      
        #plot_list <- list()
        #for(ea_chr_pair in 1:ncol(chr_pairs)){
          f1R.t2 <- f1R() %>% select(c(1, chr_pairs[,all_combos_chr_pairs_and_classes()[my_i,1]]+1), ncol(.))
          
        #  for (ea_class in 1:length(classes)){
          
            matr_plot <- return_chr_prop_matr(f1R.t2,classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                              maxPair = maxChrPlus1)
            x_y_axis_lab <- colnames(matr_plot)[4:5]
            plt <- create_perc_matr2(matr_plot, title = classes[all_combos_chr_pairs_and_classes()[my_i,2]], 
                                     minChr = 1, 
                                     maxChr = maxChrPlus1, xlab = x_y_axis_lab[1], ylab=x_y_axis_lab[2])
            return(plt)

      })
    })
  }
})
  
#})



if(interactive()){
  
  maxChr = 8
  maxChrPlus1 = maxChr + 1
  #input$files$name
  path_list <- list.files(here::here("testDat/"), pattern = "^test_aneupl_4color")
  #path_list <- list.files(here::here("testDat/"), pattern = "^test_aneupl_file")
  tbl_list <- lapply(here::here("testDat", path_list), read_xlsx)
  
  f1R.t <- map2(.x = path_list, .y= tbl_list,
                .f = ~data.frame(clss=.x, .y)) %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    clean_names() %>%
    mutate(variable_ =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
    mutate_at(.vars = vars(starts_with("Chr")), 
              .funs = ~ifelse(. == 0, 1, 
                              ifelse(. <= maxChr, ., maxChrPlus1))) 
  
  f2R.t <- f1R.t %>% 
    mutate(variable_= factor(variable_, levels=c("diploid", "polyploid", "aneuploid"))) %>%
    group_by(clss, variable_) %>%
    summarise(n = n()) %>%
    complete(variable_) %>% 
    replace(is.na(.), 0) %>%
    mutate(freq = n / sum(n)) %>% 
    ungroup %>%
    select(-n) %>%
    spread(key = variable_, value = freq) %>%
    replace(is.na(.), 0)
  
  f3R.t <- f2R.t %>%
    dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) #%>%
    #gather(key = "variable_", value = "prop", 2:5) %>%
    #mutate(variable_ = factor(variable_, 
    #                          levels = rev(c("diploid", "polyploid", "aneuploid", "entropy"))))
  
  #f2R.t %>% mutate_at(vars(ends_with("ploid")), quo(+ 0.1))# as.character)#sum(., 0.01))
  
  #ggplot(f2R.t, aes(x = aneuploid, y=diploid, z=polyploid,
  #                  fill = clss, label = clss)) + geom_point() + coord_tern(expand = TRUE)
  p <- ggtern::ggtern(data=data.frame(f2R.t), 
                      mapping = aes(x = aneuploid, y=diploid, z=polyploid,
                                    fill = clss, label = clss)) + 
    ggtern::coord_tern(expand = TRUE) + 
    geom_point(size = 3, alpha = 0.4, pch= 21, color = "black", stroke = 1) + 
    ggtern::limit_tern(1.03,1.03,1.03) + 
    xlab("") + 
    ggtern::Tlab("Diploid") +
    ggtern::Llab("Aneuploid") +
    ggtern::Rlab("Polyploid") 
  

  
  
  
  aneuploidy_score_bakker <- f1R.t %>%
    gather(key = "bins", value= "nchr", 2:(ncol(.)-1)) %>%
    mutate(ideal_nchr = 2) %>%
    mutate(ideal_obs_diff = abs(ideal_nchr - nchr)) %>%
    group_by(clss) %>% 
    summarize(sum_ideal_obs_diff = sum(ideal_obs_diff), n_bins_times_n_cells_per_group = n()) %>%
    mutate(aneupl_score_bakker = sum_ideal_obs_diff / n_bins_times_n_cells_per_group) %>%
    select(clss, aneupl_score_bakker)
  
  
  heterogeneity_score_bakker <- f1R.t %>%
    select(-variable_) %>% 
    gather(key = "bins", value = "numChr", 2:(ncol(.)-1)) %>%
    group_by(clss, bins, numChr) %>%
    summarize(mft=n()) %>% 
    arrange(clss, bins, mft,  numChr) %>% #category, bins,  mft, cp_nm
    ungroup() %>%
    group_by(clss, bins) %>%
    mutate(f = rev(1:n()-1)) %>%
    mutate(mft_f = mft * f) %>%
    ungroup() %>%
    group_by(clss) %>%
    summarize(heterog_score_bakker = sum(mft_f)/ (sum(mft)*length(unique(bins)))) 
  
  
  n_samples_per_grp <- f1R.t %>% 
    group_by(clss) %>% 
    summarise (n = n())
  
  anca_idx <- f1R.t %>% #anca score
    select(-variable_) %>% 
    gather(key = "chrom", value= "nchr", 2:(ncol(.)-1)) %>%
    mutate(diploid_bin = nchr == 2) %>%
    group_by(clss, diploid_bin) %>% 
    summarise (n = n()) %>%
    spread(key = diploid_bin, value=n) %>%
    clean_names() %>%
    mutate(anca_score_blegen = false/ (true + false)) %>% 
    select(clss, anca_score_blegen)
  
  print("anca indx:")
  print(head(anca_idx))
  
  classes <- unique(f1R.t$clss)
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  #path_list <- list.files(here::here("testDat/"), pattern = "^test_aneupl_file")
  path_list <- list.files(here::here("testDat/"), pattern = "^test_aneupl_4color")
  
  #tbl_list <- lapply(here::here("testDat", path_list), read_xlsx)
  
  file_names <- path_list #input$fish_files$name})
  
  maxChr <- 8
  maxChrPlus1 = maxChr + 1
  nchrs <- f1R.t %>% ncol() - 2
  chr_pairs <- combn(1:nchrs, 2)
  
  plot_list <- list()
  for(ea_chr_pair in 1:ncol(chr_pairs)){
    #ea_chr_pair = 2
    f1R.t2 <- f1R.t %>% select(c(1, chr_pairs[,ea_chr_pair]+1), ncol(.))
    
  for (ea_class in 1:length(classes)){ #i in 1:max_plots) {
     
      matr_plot <- return_chr_prop_matr(f1R.t2,classes[ea_class], maxPair = maxChrPlus1)
      plt <- create_perc_matr2(matr_plot, title = classes[ea_class], minChr = 1, 
                               maxChr = maxChrPlus1, xlab = "", ylab="")
      #save plotted object
      #plot_list[[ea_chr_pair + (3*(ea_class - 1))]] <- plt
      plot_list[[ea_class + (length(classes)*(ea_chr_pair - 1))]] <- plt
      #plot_list[[ea_class + (nchr*(ea_chr_pair - 1))]] <- plt

    }
  }
    grid.arrange(grobs = plot_list, ncol = 1) ## display plot
    
    return(plt)
  } #)

#}


shinyApp(ui, server)
#app <- shinyApp(ui, server)
#runApp(app, display.mode = "showcase")