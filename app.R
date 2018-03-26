library(shiny)
#library(shinyjs) 
library(readxl)
library(tidyverse)
library(here)
library(janitor)
library(ggtern)
source("scripts/helper_scripts.R")

max_plots <- 20 # *maximum* total number of plots

ui <- fluidPage(title = "aneuvis", 
  shinythemes::themeSelector(), 
  titlePanel("aneuvis v.0.2"),
  sidebarPanel(
    radioButtons(inputId = "rb", label = "1. Select data type:",
                 c("FISH" = "fish",
                   "SKY" = "sky",
                   "Single-cell" = "sc")),
    fileInput(inputId = "files", label = "2. Upload files (for now, only FISH)", multiple = TRUE, accept = c(".xlsx"))
  ),
  
      mainPanel(
        conditionalPanel(
          condition = "input.rb == 'fish'", 
            tabsetPanel(
              tabPanel("Grid Plots", uiOutput("gridPlots")), 
              tabPanel("Table", tableOutput("table")),
              tabPanel("Ternary Plot", plotOutput("ternPlot")),
              tabPanel("Entropy Plot", plotOutput("ploidyPlot"))
          )
  ), conditionalPanel(condition = "input.rb == 'sky'",
                      tabsetPanel(
                        tabPanel("Sky Plots"),
                        tabPanel("Sky stats")
                        )
                    ),
    conditionalPanel(condition = "input.rb == 'sc'",
                     tabsetPanel(
                       tabPanel("Single cell Plots"),
                       tabPanel("Single cell statistics")
                     )
                    )
              )
)



server <- shinyServer(function(input, output) {
  
  #if
  #if(input$rb == "fish"){
   
  aneuDat_r <- reactive({
    validate(need(input$files != "", "aneupl prop..."))
    
    if (is.null(input$files)) {
      return(NULL)
    } else {
      
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
        mutate(ploidy =  apply(.[,2:ncol(.)], 1, classifPloidy)) %>% 
        mutate_at(.vars = vars(starts_with("Chr")), 
                  .funs = ~ifelse(. == 0, 1, 
                                  ifelse(. <= maxChr, ., maxChrPlus1)))
      return(aneuDat)
    }
  })
  
  aneuDat_ploidy_tbl <- reactive({
    #aneuDat_r() %>% 
    #aneuDat_test2 <- aneuDat_test %>%
    aneuDat_r() %>% 
      mutate(ploidy= factor(ploidy, levels=c("diploid", "polyploid", "aneuploid"))) %>%
      group_by(clss, ploidy) %>%
      summarise(n = n()) %>%
      complete(ploidy) %>% #clss, 
      replace(is.na(.), 0) %>%
      mutate(freq = n / sum(n)) %>% 
      ungroup %>%
      select(-n) %>%
      spread(key = ploidy, value = freq) %>%
      replace(is.na(.), 0)
    # add a 0 here
  })
  
  aneuDat_ploidy_tbl_for_plot <- reactive({
    aneuDat_ploidy_tbl() %>%
      #aneuDat_test3 <- aneuDat_test2 %>%
      dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) %>%
      gather(key = "ploidy", value = "prop", 2:5) %>%
      mutate(ploidy = factor(ploidy, 
                             levels = rev(c("diploid", "polyploid", "aneuploid", "entropy"))))
  })
  
  output$ploidyPlot <- renderPlot({

    brw_clrs <- c(rev(RColorBrewer::brewer.pal(n = 8, name = "Purples")), "#ffffff",
                  RColorBrewer::brewer.pal(n = 8, name = "Oranges"))
    
    aneuDat_ploidy_tbl_for_plot() %>%
      mutate(prop2 = ifelse(ploidy == "entropy", -prop, prop)) %>%
      mutate(prop_cut = cut(.$prop2, breaks = c(-Inf, seq(-1, -0.1, .15), -1e-6, 1e-6, 
                                                seq(0.1, 1, .15), Inf), 
                            labels = c(paste0(">", c(seq(1, 0.1, -.15),0)), 0, 
                                       rev(paste0(">", c(seq(1, 0.1, -.15),0), " ")))),
             prop_cut = factor(prop_cut, levels = rev(levels(prop_cut))))  %>%
      ggplot(aes(x=clss, y=ploidy, fill=prop_cut)) + 
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
      select(-ploidy) %>% gather(key = "chr", value = "numChr", 2:3) %>%
      select(-chr) %>% group_by(clss) %>%  
      summarise (n = n()/2) %>%
      mutate(freq = n / sum(n)) #%>%
      #data.frame() #%>%
      #t()
   # print(head(aneuDat_r()))
    print(dim(aneuDat_chr_instab_idx))
    print(aneuDat_chr_instab_idx)
    
    aneuDat_chr_instab_idx2 <- aneuDat_r() %>%
      gather(key = "chrom", value= "nchr", 2:3) %>%
      mutate(ideal_nchr = 2) %>%
      mutate(ideal_obs_diff = abs(ideal_nchr - nchr)) %>%
      group_by(clss) %>% 
      summarize(norm_sum_ideal_obs_diff = sum(ideal_obs_diff) / sum(nchr)) %>%
      spread(key = clss, value=norm_sum_ideal_obs_diff) %>%
      mutate(ploidy=as.factor("norm_sum_ideal_obs_diff")) %>%
      select(ploidy, everything())
    print(aneuDat_chr_instab_idx2)

    
    aneuDat_chr_instab_idx_n <- aneuDat_chr_instab_idx %>%
      select(-freq) %>%
      spread(key = clss, value=n) %>%
      mutate(ploidy=as.factor("n")) %>%
      select(ploidy, everything())
    print("head(aneuDat_chr_instab_idx_n):")
    print(head(aneuDat_chr_instab_idx_n))
      
    print("aneuDat_ploidy_tbl_for_plot(): ")
    print(head(aneuDat_ploidy_tbl_for_plot()))
    p <- aneuDat_ploidy_tbl_for_plot() %>% 
      spread(key = clss, value=prop) %>% #chgnd  #  #
      mutate(ploidy = factor(ploidy, levels=c("diploid", "polyploid", "aneuploid", "entropy"))) %>%
      arrange(ploidy) %>%
      bind_rows(aneuDat_chr_instab_idx_n, aneuDat_chr_instab_idx2)
    print("p:")
    print(dim(p))
    print(p)
    return(p)
  })
  
  output$gridPlots <- renderUI({
    cl_ln <- length(unique(aneuDat_r()$clss))
    plot_output_list <- lapply(1:cl_ln, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 450, width = 450)
    })
    
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
