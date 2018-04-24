#app_ginkgo
library(shiny)
#library(shinyjs)
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
library(grid)

source("scripts/helper_scripts.R")


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
    #submitButton("Submit"),
    #textInput("")
    p("Download example 2-chromosome FISH data ", tags$a(target = "_blank", 
                                                         href = "https://docs.google.com/uc?export=download&id=1CKh6feR7AmndtAvoF4Y-EFxaigjiFmba", "here"), " (zip file)")
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
                       tabPanel("Grid plot",
                                uiOutput("gridPlots_ginkgo"),
                                plotOutput("ginkgo_plot")),
                       #plotOutput("plot_test")),
                       tabPanel("Summary statistics",
                                tableOutput("gingko_table"))
                       #tabPanel("Ternary Plot", plotOutput("ternPlot")),
                       #tabPanel("Entropy Plot", plotOutput("ploidyPlot")))
                     ))
    
  )
)



server <- shinyServer(function(input, output) {
  ###########
  #ginkgo data
  ginkgo_dat <- reactive({
    validate(need(input$files != "", "..."))
    
    if (is.null(input$files)) {
      return(NULL)
    }
    if(tools::file_ext(input$files$datapath[1]) %in% c("txt")){
      
      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      path_list <- as.list(input$files$datapath)
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(input$files$datapath, read_delim, delim="\t")
      
      g <- map2(.x = input$files$name, .y= tbl_list,
                .f = ~data.frame(clss=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        clean_names() #%>%
      # mutate(variable_ =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
      # mutate_at(.vars = vars(starts_with("Chr")), 
      #           .funs = ~ifelse(. == 0, 1, 
      #                           ifelse(. <= maxChr, ., maxChrPlus1)))
      print(head(g))
      return(g)
    }
  })
  
  output$gingko_table <- renderTable({
    ginkgo_dat()[1:5, 1:5]
  })
  
  output$ginkgo_plot <- renderPlot({
    gnk_dt <- ginkgo_dat() %>% 
      #gnk %>% 
      gather(key = smpl, value=cp_nm, 5:ncol(.)) %>% 
      mutate(cp_nm = as.numeric(cp_nm)) %>%
      group_by(chr, smpl) %>% 
      summarise(avg = mean(cp_nm)) %>% 
      mutate(avgRound = round(avg)) %>%
      mutate(avgRound = ifelse(avgRound > 2, 1, ifelse(avgRound == 2, 0, -1))) #%>%
    print("gnk_dt: ")
    head(gnk_dt)
    
    p <- ggplot(filter(gnk_dt, chr %in% paste0("chr", c(1:22,"X"))), 
                aes(x=chr, y=smpl, fill=factor(avgRound))) + 
      geom_tile(color = "white", size = 1) + 
      scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("Chromosome") + ylab("")
    
    #return(multiplot(list(plotOutput(p), plotOutput(p), plotOutput(p))))
    return(p)
    #separate(chr, c("chrRm", "chr"), sep=3) %>% 
    #dplyr::select(-chrRm) %>% 
    #filter(chr != "Y") %>% 
    #mutate(chr = factor(chr, levels=c(1:22, "X")), 
    #       avgRound=factor(avgRound)) %>%
    #left_join(pldyk3, by = "smpl") %>%
    #left_join(gkStats, by = "smpl")
  })
  
  output$ginkgo_plot_reactive <- reactive({
    gnk_dt <- ginkgo_dat() %>% 
      #gnk %>% 
      gather(key = smpl, value=cp_nm, 5:ncol(.)) %>% 
      mutate(cp_nm = as.numeric(cp_nm)) %>%
      group_by(chr, smpl) %>% 
      summarise(avg = mean(cp_nm)) %>% 
      mutate(avgRound = round(avg)) %>%
      mutate(avgRound = ifelse(avgRound > 2, 1, ifelse(avgRound == 2, 0, -1))) #%>%
    print("gnk_dt: ")
    head(gnk_dt)
    
    p <- ggplot(filter(gnk_dt, chr %in% paste0("chr", c(1:22,"X"))), 
                aes(x=chr, y=smpl, fill=factor(avgRound))) + 
      geom_tile(color = "white", size = 1) + 
      scale_fill_brewer(type = "div",palette = "RdBu",drop=FALSE, direction = -1, name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size= 8),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("Chromosome") + ylab("")
    
    #return(multiplot(list(plotOutput(p), plotOutput(p), plotOutput(p))))
    return(p)
    #separate(chr, c("chrRm", "chr"), sep=3) %>% 
    #dplyr::select(-chrRm) %>% 
    #filter(chr != "Y") %>% 
    #mutate(chr = factor(chr, levels=c(1:22, "X")), 
    #       avgRound=factor(avgRound)) %>%
    #left_join(pldyk3, by = "smpl") %>%
    #left_join(gkStats, by = "smpl")
  })
  
  
  
  
  #set up the container for the plots
  output$gridPlots_ginkgo <- renderUI({
    cl_ln <- 3 #length(unique(aneuDat_r()$clss))
    plot_output_list <- lapply(1:cl_ln, function(i) {
      plotname <- paste("plot_gnk", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    #create a tagList of all plots
    do.call(tagList, plot_output_list)
  })
  
  #classes <- reactive({unique(aneuDat_r()$clss)})
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  #file_names <- reactive({input$files$name})
  
  for (i in 1:3){#max_plots) {
    local({
      my_i <- i
      plotname <- paste("plot_gnk", my_i, sep="")
      
      output[[plotname]] <- renderPlot(ginkgo_plot_reactive()) #renderPlot({
      #cls <- classes()[my_i]
      #maxChr = 8
      #maxChrPlus1 = maxChr + 1
      #matr_plot <- return_chr_prop_matr(aneuDat_r(),cls, maxPair = maxChrPlus1)
      #plt <- create_perc_matr2(matr_plot, title = file_names()[my_i], minChr = 1, 
      #                         maxChr = maxChrPlus1, xlab = "", ylab="")
      #return(plt)
      
      #})
    })
  }
  
  
  
  
  
  
  #  output$plot_test <- renderPlot({
  #    plot_output_list <- sapply(1:length(plots), function(i) {
  #      plotname <- paste("plot", i , sep="")
  #      plotOutput(plotname, height = 280, width = 250)
  #    })
  #    multiplot(plot_output_list)
  #  })
  
  #################
  #if
  #if(input$rb == "fish2"){
  
  #### single cell WGS from Ginkgo
  #if(tools::file_ext(input$files$datapath[1]) %in% c("xlsx", "xls")){
  
  ######single cell FISH 
  aneuDat_r <- reactive({
    validate(need(input$files != "", "..."))
    
    if (is.null(input$files)) {
      return(NULL)
    }
    if(tools::file_ext(input$files$datapath[1]) %in% c("xlsx", "xls")){
      #else {
      
      maxChr = 8
      maxChrPlus1 = maxChr + 1
      
      path_list <- as.list(input$files$datapath)
      #fileinput: 'name', 'size', 'type' and 'datapath'.
      tbl_list <- lapply(input$files$datapath, read_xlsx)
      
      aneuDat <- map2(.x = input$files$name, .y= tbl_list,
                      .f = ~data.frame(clss=.x, .y)) %>% 
        do.call(rbind, .) %>% 
        as_tibble() %>% 
        clean_names() %>%
        mutate(variable_ =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
        mutate_at(.vars = vars(starts_with("Chr")), 
                  .funs = ~ifelse(. == 0, 1, 
                                  ifelse(. <= maxChr, ., maxChrPlus1)))
      print(head(aneuDat))
      return(aneuDat)
    }
  })
  
  aneuDat_ploidy_tbl <- reactive({
    #aneuDat_r() %>% 
    #aneuDat_test2 <- aneuDat_test %>%
    ad_pldy_tb <- aneuDat_r() %>% 
      mutate(variable_= factor(variable_, levels=c("diploid", "polyploid", "aneuploid"))) %>%
      group_by(clss, variable_) %>%
      summarise(n = n()) %>%
      complete(variable_) %>% #clss, 
      replace(is.na(.), 0) %>%
      mutate(freq = n / sum(n)) %>% 
      ungroup %>%
      select(-n) %>%
      spread(key = variable_, value = freq) %>%
      replace(is.na(.), 0)
    print("head(ad_pldy_tb)")
    head(ad_pldy_tb)
    return(ad_pldy_tb)
    # add a 0 here
  })
  
  aneuDat_ploidy_tbl_for_plot <- reactive({
    aneuDat_ploidy_tbl() %>%
      #aneuDat_test3 <- aneuDat_test2 %>%
      dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) %>%
      gather(key = "variable_", value = "prop", 2:5) %>%
      mutate(variable_ = factor(variable_, 
                                levels = rev(c("diploid", "polyploid", "aneuploid", "entropy"))))
  })
  
  output$ploidyPlot <- renderPlot({
    
    brw_clrs <- c(rev(RColorBrewer::brewer.pal(n = 8, name = "Purples")), "#ffffff",
                  RColorBrewer::brewer.pal(n = 8, name = "Oranges"))
    
    aneuDat_ploidy_tbl_for_plot() %>%
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
      geom_point(data=aneuDat_ploidy_tbl(), 
                 aes(x = aneuploid,y=diploid,z=polyploid,
                     fill = clss, label = clss), 
                 size = 3, alpha = 0.4, pch= 21, color = "black", stroke = 1) + 
      limit_tern(1.03,1.03,1.03) + 
      xlab("") + 
      Tlab("Diploid") +
      Llab("Aneuploid") +
      Rlab("Polyploid") 
    print(p)
    NULL
  })
  
  
  output$table <- renderTable({
    #03-24-2018
    aneuDat_chr_instab_idx <- aneuDat_r() %>% 
      #aneuDat_chr_instab_idx <- aneuDat_test %>% 
      select(-variable_) %>% 
      gather(key = "chr", value = "numChr", 2:3) %>%
      group_by(clss) %>% 
      summarise (n = n()/length(unique(chr))) %>%
      mutate(freq = n / sum(n))
    
    print(head(aneuDat_r()))
    print(dim(aneuDat_chr_instab_idx))
    print(aneuDat_chr_instab_idx)
    
    #aneuploidy_score_bakker <- aneuDat_test %>%
    aneuploidy_score_bakker <- aneuDat_r() %>%
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
    heterogeneity_score_bakker <- aneuDat_r() %>%
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
    instability_idx_bayani <- aneuDat_r() %>%
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
    
    anca_idx <- aneuDat_r() %>%
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
    
    
    #print("aneuDat_ploidy_tbl_for_plot(): ")
    #print(head(aneuDat_ploidy_tbl_for_plot()))
    p <- aneuDat_ploidy_tbl_for_plot() %>% 
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
    cl_ln <- length(unique(aneuDat_r()$clss))
    plot_output_list <- lapply(1:cl_ln, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    #create a tagList of all plots
    do.call(tagList, plot_output_list)
  })
  
  classes <- reactive({unique(aneuDat_r()$clss)})
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
        matr_plot <- return_chr_prop_matr(aneuDat_r(),cls, maxPair = maxChrPlus1)
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