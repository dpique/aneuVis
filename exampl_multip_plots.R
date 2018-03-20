
max_plots <- 20 # *maximum* total number of plots

ui <- shinyUI(pageWithSidebar(
  
  headerPanel("aneuvis vers. 0.2"),
  
  sidebarPanel(
      fileInput(inputId = "files", label = "Upload", multiple = TRUE, accept = c(".xlsx"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Grid Plots", uiOutput("gridPlots")), 
      tabPanel("Table", tableOutput("table")),
      tabPanel("Ternary Plot", plotOutput("ternPlot")),
      tabPanel("Entropy Plot", plotOutput("ploidyPlot"), plotOutput("entropyPlot"))
    )
  )
))


server <- shinyServer(function(input, output) {

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
      dplyr::mutate(entropy=purrr::pmap_dbl(.[,-1], ~entropy::entropy(c(...)))) %>%
      gather(key = "ploidy", value = "prop", 2:5) %>%
      mutate(ploidy = factor(ploidy, 
                             levels = rev(c("diploid", "polyploid", "aneuploid", "entropy"))))
  })
  
  output$ploidyPlot <- renderPlot({
    #aneuDat_test2
    aneuDat_ploidy_tbl_for_plot() %>% 
      filter(ploidy != "entropy") %>%
      ggplot(aes(x=clss, y=ploidy, fill=as.numeric(prop))) + 
      geom_tile(color="black") + 
      geom_text(aes(label = round(prop, 2))) + 
      scale_fill_distiller(type = "seq", palette = "Purples", direction = 1, name = "% Ploidy") + 
      theme_classic() + 
      coord_fixed() + 
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            line = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$entropyPlot <- renderPlot({
    #aneuDat_test2
   aneuDat_ploidy_tbl_for_plot() %>% 
      filter(ploidy == "entropy") %>%
      ggplot(aes(x=clss, y=ploidy, fill=as.numeric(prop))) + 
      geom_tile(color="black") + 
      geom_text(aes(label = round(prop, 2))) + 
      scale_fill_distiller(type = "seq", palette = 5, direction = 1, name = "Entropy") + 
      theme_classic() + 
      coord_fixed() + 
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            line = element_blank(),
           axis.text.x = element_text(angle = 90, hjust = 1))
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
    #Tarrowlab("Top Arrow Label") + Larrowlab("Left Arrow Label")
    print(p)
    NULL
  })
  
  
  output$table <- renderTable({
    aneuDat_ploidy_tbl()
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
