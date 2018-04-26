library(shiny)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)

source("scripts/helper_scripts.R")
#LOCALTEST = FALSE

tabPanelFishMaster <- function(x){
 # tabsetPanel(
 #   tabPanel("Single cell plots"),
 #   tabPanel("Single cell statistics"),
 #   tabPanel("function_test panel")
 # )
  tabsetPanel(
    tabPanel("Grid Plots", uiOutput("gridPlots")),
    tabPanel(
      "Summary statistics",
      tableOutput("table"),
      tableOutput("aneuDat_ploidy_tbl_2"),
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



max_plots <- 20 # *maximum* total number of plots

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
    
    fileInput(
      inputId = "files",
      label = "2. Upload files (for now, only FISH data)",
      multiple = TRUE,
      accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
    ),
    fileInput(
      inputId = "gnk_key",
      label = "Upload Ginkgo Key (xls or xlsx)", #names must match those in gnko
      multiple = FALSE,
      accept = c(".xlsx", ".xls")#, ".csv", ".txt", ".tsv")
    ),
    actionButton("submit", "Submit"),
    #submitButton("Submit"),
    #textInput("")
    p("Download example 2-chromosome FISH data ", 
      tags$a(target = "_blank", 
             href = "https://docs.google.com/uc?export=download&id=1CKh6feR7AmndtAvoF4Y-EFxaigjiFmba", "here"), 
      " (zip file)"),
    p("Download example Ginkgo data", 
      tags$a(target = "_blank", 
             href="http://qb.cshl.edu/ginkgo/uploads/_t10breast_navin/SegCopy?uniq=1210441", "here"), 
      "(save as '.txt' file)"),
    p("Download Ginkgo key", 
      tags$a(target = "_blank",
             href="https://docs.google.com/uc?export=download&id=1bhOEye8FR3Ut9w_hT-SMab2AaUEx9_5x", "here"))
  ),
  
  mainPanel(
    conditionalPanel(condition = "input.rb == 'fish2'",
                     tabPanelFishMaster()),
    conditionalPanel(condition = "input.rb == 'sky'",
                    tabsetPanel(
                     tabPanel("Sky Plots"),
                     tabPanel("Sky stats")
                   )),
    conditionalPanel(condition = "input.rb == 'ginkgo'",
                     tabsetPanel(
                       tabPanel("Chromosomal Copy Number Heat Map",
                                #uiOutput("gridPlots_ginkgo"),
                                plotOutput("g5P")),
                       tabPanel("Sample x Chromosome Summary Table", 
                                p("The table below gives the weighted average copy number per chromosome per sample.
                                  The table is also available for download."),
                                downloadButton("g2T.d", "Download"),
                                tableOutput("g2T"))
                                #plotOutput("plot_test")),
                       # tabPanel("Summary statistics",
                       #  p("ginkgo_table:"),
                       #  tableOutput("ginkgo_table_out"),
                       #  p("ginkgo key:"),
                       #  tableOutput("gink_key"),
                       #  p("ginkgo_freq_tbl:"),
                       #  tableOutput("ginkgo_freq_tbl"))
                       #tabPanel("Ternary Plot", plotOutput("ternPlot")),
                       #tabPanel("Entropy Plot", plotOutput("ploidyPlot")))
                     ))
    
    )
)



server <- shinyServer(function(input, output) {
  ###########
  #1. Read in raw ginkgo data
  
  #gR <- reactive({
   gR <- eventReactive(input$submit, { #var. named with capital R for Reactive
    validate(need(input$files != "", "..."))
    
    if (is.null(input$files)) {
      return(NULL)
    }
    if(tools::file_ext(input$files$datapath[1]) %in% c("txt")){

      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      path_list <- as.list(input$files$datapath)
      if(interactive()){
        path_list <- as.list( here::here("testDat/ginkgo_test_dat_copy_number.txt"))
      }
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(path_list, read_delim, delim="\t")
      
      g <- map2(.x = path_list, .y= tbl_list,
                      .f = ~data.frame(clss=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        .[,colSums(!is.na(.)) > 0] %>%
        select(-clss)

      #print(head(g))
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
     
     if(LOCALTEST == TRUE){
       gK <- read_xlsx(path = here::here("testDat/ginkgo_key.xlsx"), sheet = 1) #%>%
     }
     
     print("head(gk):")
     print(head(gK))
     return(gK)
   })
   
   
  g2R <- reactive({
    g2 <- g %>% 
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
      left_join(key_xl, c("smpl" = "file_name"))
    return(g2)
  })
   
  g2.1R <- reactive({
    g2.t <- g2 %>% select(-avgRound) %>%
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
  
  
  output$g2P <- renderPlot({
    g2.p <- g2 %>%
      select(chr, avgRound, category) %>%
      group_by(chr, avgRound, category) %>%
      summarise (n = n()) %>%
      mutate(freq = n / sum(n)) %>% 
      spread(key = avgRound, value = freq)
    
    g2.p <- g2 %>%
      select(chr, avgRound, category) %>%
      group_by(chr, avgRound, category) %>%
      summarise (n = n()) %>%
      ungroup() %>%
      group_by(category, chr) %>%
      mutate(freq = n / sum(n)) %>% 
      mutate(freq_dir = freq*as.numeric(as.character(avgRound))) %>%
      arrange(chr, category, avgRound)
      #spread(key = avgRound, value = freq)
    ggplot(g2.p, aes(x=avgRound,y = freq, color=chr)) + 
      geom_jitter(alpha=0.5) + 
      geom_line(aes(group=chr))# , color=category))
    ggplot(g2.p, aes(y=avgRound,x = freq)) + 
      geom_jitter(alpha=0.5, aes(color=chr)) + 
      geom_line(aes(group=chr, color=category))# , color=category))
    
    
  })
  
  g3R <- reactive({
    g3 <- g2R %>%
      dplyr::select(avgRound, chr, category, smpl) %>%
      spread(chr, avgRound)
    return(g3)
  })
    
  g4R <- reactive({
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
    return(g4)
  })


  output$g5P <- renderPlot({ #plot
    g5.1 <- ggplot(filter(g4R(), chr %in% c(1:22,"X")), aes(x=chr, y=category, 
                                                 fill=factor(chr_freq, levels=sort(unique(chr_freq))))) + 
      geom_tile(color = "white", size = 1) + 
      scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      #coord_fixed(ratio = 1) + 
      xlab("Chromosome") + ylab("")+ 
      theme(legend.position="top", 
            plot.margin=grid::unit(c(0,0,0,0), "mm"),
            aspect.ratio=1)
    
    g5.2 <- ggplot(filter(g4R(), chr == "n"), aes(x=chr, y=category, fill=prop)) +
      geom_tile(color = "white", size = 1) + 
      scale_fill_gradient(low = "white", high = "black" ) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + 
      xlab("") + ylab("") + 
      theme(legend.position="top", 
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
            #aspect.ratio=1)
    #cowplot::plot_grid(g5.1, g5.2, labels = c("A", "B"), rel_widths = c(1, 1))
    #cowplot::plot_grid(g5.1, g5.2, align = "h", nrow = 1, ncol = 2, rel_widths = c(5, 1/4))
    return(gridExtra::grid.arrange(g5.1, g5.2, ncol=2, widths=c(4,1)))#,layout_matrix=))
  })
  

  
  
  
  
if(interactive()){
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
    left_join(key_xl, c("smpl" = "file_name"))
  
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
}


 
#  if(interactive()){
#    
#  maxChr = 8
#  maxChrPlus1 = maxChr + 1
#  #input$files$name
#  path_list <- list.files(here::here("testDat/"), pattern = "^test_aneupl_file")
#  tbl_list <- lapply(here::here("testDat", path_list), read_xlsx)
#  
#  f1R.t <- map2(.x = path_list, .y= tbl_list,
#                .f = ~data.frame(clss=.x, .y)) %>% 
#    do.call(rbind, .) %>% 
#    as_tibble() %>% 
#    clean_names() %>%
#    mutate(variable_ =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
#    mutate_at(.vars = vars(starts_with("Chr")), 
#              .funs = ~ifelse(. == 0, 1, 
#                              ifelse(. <= maxChr, ., maxChrPlus1))) 
#  
#  f2R.t <- f1R.t %>% 
#    mutate(variable_= factor(variable_, levels=c("diploid", "polyploid", "aneuploid"))) %>%
#    group_by(clss, variable_) %>%
#    summarise(n = n()) %>%
#    complete(variable_) %>% 
#    replace(is.na(.), 0) %>%
#    mutate(freq = n / sum(n)) %>% 
#    ungroup %>%
#    select(-n) %>%
#    spread(key = variable_, value = freq) %>%
#    replace(is.na(.), 0)
#  
#  f3R.t <- f2R.t %>%
#    dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) %>%
#    gather(key = "variable_", value = "prop", 2:5) %>%
#    mutate(variable_ = factor(variable_, 
#                              levels = rev(c("diploid", "polyploid", "aneuploid", "entropy"))))
#  
#  f2R.t %>% mutate_at(vars(ends_with("ploid")), quo(+ 0.1))# as.character)#sum(., 0.01))
#  
#  ggplot(f2R.t, aes(x = aneuploid, y=diploid, z=polyploid,
#                    fill = clss, label = clss)) + geom_point() + coord_tern(expand = TRUE)
#  p <- ggtern::ggtern(data=data.frame(f2R.t), 
#         mapping = aes(x = aneuploid, y=diploid, z=polyploid,
#                   fill = clss, label = clss)) + 
#    ggtern::coord_tern(expand = TRUE) + 
#    geom_point(size = 3, alpha = 0.4, pch= 21, color = "black", stroke = 1) + 
#    ggtern::limit_tern(1.03,1.03,1.03) + 
#    xlab("") + 
#    ggtern::Tlab("Diploid") +
#    ggtern::Llab("Aneuploid") +
#    ggtern::Rlab("Polyploid") 
# } #print(p)
  
  ######single cell FISH 
  f1R <- reactive({
    validate(need(input$files != "", "..."))
    
    if (is.null(input$files)) {
      return(NULL)
    }
      
      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      #path_list <- list.files(here::here("testDat/"), pattern = "^test_aneupl_file")
      print("input$files$datapath:")
      print(input$files$datapath)
      path_list <- as.list(input$files$name)
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(input$files$datapath, read_xlsx)
      
      f1R.t <- map2(.x = path_list, .y= tbl_list,
                      .f = ~data.frame(clss=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        clean_names() %>%
        mutate(variable_ =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
        mutate_at(.vars = vars(starts_with("Chr")), 
                  .funs = ~ifelse(. == 0, 1, 
                                   ifelse(. <= maxChr, ., maxChrPlus1)))
      #print(head(aneuDat))
      return(f1R.t)
    #}
  })
  
   f2R <- reactive({
     f2R.t <- f1R() %>% 
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
    return(f2R.t)
    # add a 0 here
  })
  
  aneuDat_ploidy_tbl_2 <- renderTable({
    head(f2R())
  })
  
  f3R <- reactive({
    f3R.t <- f2R() %>%
      dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) %>%
      gather(key = "variable_", value = "prop", 2:5) %>%
      mutate(variable_ = factor(variable_, 
                             levels = rev(c("diploid", "polyploid", "aneuploid", "entropy"))))
    return(f3R.t)
  })
  
  output$ploidyPlot <- renderPlot({

    brw_clrs <- c(rev(RColorBrewer::brewer.pal(n = 8, name = "Purples")), "#ffffff",
                  RColorBrewer::brewer.pal(n = 8, name = "Oranges"))
    
    f3R() %>%
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
    #p <- ggtern::ggtern(data=f2R(), 
    #                    mapping = aes(x = aneuploid, y=diploid, z=polyploid,
    #                                  fill = clss, label = clss)) + 
    #  ggtern::coord_tern(expand = TRUE) + 
    #  geom_point(size = 3, alpha = 0.4, pch= 21, color = "black", stroke = 1) + 
    #  ggtern::limit_tern(1.03,1.03,1.03) + 
    #  xlab("") + 
    #  ggtern::Tlab("Diploid") +
    #  ggtern::Llab("Aneuploid") +
    #  ggtern::Rlab("Polyploid") 
    #return(p)
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
    
  })
  
  
  output$table <- renderTable({
    #03-24-2018
    aneuDat_chr_instab_idx <- f1R() %>% 
      select(-variable_) %>% 
      gather(key = "chr", value = "numChr", 2:3) %>%
      group_by(clss) %>% 
      summarise (n = n()/length(unique(chr))) %>%
      mutate(freq = n / sum(n))
    
    aneuploidy_score_bakker <- f1R() %>%
      gather(key = "bins", value= "nchr", 2:3) %>%
      mutate(ideal_nchr = 2) %>%
      mutate(ideal_obs_diff = abs(ideal_nchr - nchr)) %>%
      group_by(clss) %>% 
      summarize(aneupl_score_bakker = sum(ideal_obs_diff) / (length(unique(bins))*sum(nchr))) %>% 
      spread(key = clss, value=aneupl_score_bakker) %>%
      mutate(variable_=as.factor("aneupl_score_bakker")) %>%
      select(variable_, everything())
    print(aneuploidy_score_bakker)

    #heterogeneity_score_bakker <- aneuDat_test %>% 
    heterogeneity_score_bakker <- f1R() %>%
      select(-variable_) %>% 
      gather(key = "bins", value = "numChr", 2:3) %>%
      mutate(numChrFromEupl = numChr - 2) %>%
      group_by(clss, bins, numChr) %>%
      summarize(mft=n()) %>% 
      arrange(clss, bins, numChr, mft) %>%
      ungroup() %>%
      group_by(clss, bins) %>%
      mutate(f = 1:n()-1) %>%
      mutate(mft_f = mft * f) %>%
      ungroup() %>%
      group_by(clss) %>%
      summarize(heterog_score_bakker = sum(mft_f)/ (sum(mft)*length(unique(bins)))) %>%
      spread(key = clss, value=heterog_score_bakker) %>%
      mutate(variable_=as.factor("heterog_score_bakker")) %>%
      select(variable_, everything())
    print(heterogeneity_score_bakker)
    
    #instability_idx_bayani <-  aneuDat_test %>% 
    instability_idx_bayani <- f1R() %>%
      select(-variable_) %>% 
      gather(key = "bins", value = "numChr", 2:3) %>%
      mutate(isDipl = numChr == 2) %>%
      group_by(clss, bins, isDipl) %>%
      count() %>%
      spread(key = isDipl, value = n) %>%
      clean_names() %>%
      mutate(instab_idx_bayani = false / (false + true)) %>% #per probe
      group_by(clss) %>%
      summarise(instab_idx_bayani = mean(instab_idx_bayani)) %>% #now average
      spread(key = clss, value=instab_idx_bayani) %>%
      mutate(variable_=as.factor("instab_idx_bayani")) %>%
      select(variable_, everything())
    
    anca_idx <- f1R() %>%
    #anca_idx <- aneuDat_test %>% 
      select(-variable_) %>% 
      gather(key = "chrom", value= "nchr", 2:3) %>%
      mutate(diploid_bin = nchr == 2) %>%
      group_by(clss, diploid_bin) %>% 
      summarise (n = n()) %>%
      spread(key = diploid_bin, value=n) %>%
      clean_names() %>%
      mutate(anca_score_blegen = false/ (true + false)) %>% 
      select(clss, anca_score_blegen) %>%
      spread(key = clss, value=anca_score_blegen) %>%
      mutate(variable_=as.factor("anca_score_blegen")) %>%
      select(variable_, everything())
    
    print("anca indx:")
    print(head(anca_idx))
    
    aneuDat_chr_instab_idx_n <- aneuDat_chr_instab_idx %>%
      select(-freq) %>%
      spread(key = clss, value=n) %>%
      mutate(variable_=as.factor("n")) %>%
      select(variable_, everything())
    print("head(aneuDat_chr_instab_idx_n):")
    print(head(aneuDat_chr_instab_idx_n))
    
      
    #print("f3R(): ")
    #print(head(f3R()))
    p <- f3R() %>% 
      spread(key = clss, value=prop) %>% #chgnd  #  #
      mutate(variable_ = factor(variable_, levels=c("diploid", "polyploid", "aneuploid", "entropy"))) %>%
      arrange(variable_) %>%
      bind_rows(aneuDat_chr_instab_idx_n, anca_idx, heterogeneity_score_bakker, aneuploidy_score_bakker, instability_idx_bayani)
    print("p:")
    print(dim(p))
    print(p)
    return(p)
  })
  
  #set up the container for the plots
  output$gridPlots <- renderUI({
    cl_ln <- length(unique(f1R()$clss))
    plot_output_list <- lapply(1:cl_ln, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    #create a tagList of all plots
    do.call(tagList, plot_output_list)
  })
  
  classes <- reactive({unique(f1R()$clss)})
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  file_names <- reactive({input$files$name})
  
  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        cls <- classes()[my_i]
        maxChr = 8
        maxChrPlus1 = maxChr + 1
        matr_plot <- return_chr_prop_matr(f1R(),cls, maxPair = maxChrPlus1)
        plt <- create_perc_matr2(matr_plot, title = file_names()[my_i], minChr = 1, 
                                 maxChr = maxChrPlus1, xlab = "", ylab="")
        return(plt)
        
      })
    })
  }
  
})
shinyApp(ui, server)




## shinyServer(function(input, output) {
##   # Insert the right number of plot output objects into the web page
##   output$plots <- renderPlot({
##     plot_output_list <- sapply(1:length(plots), function(i) {
##       plotname <- paste("plot", i , sep="")
##       plotOutput(plotname, height = 280, width = 250)
##     })
##     multiplot(plot_output_list)
##   })
## })