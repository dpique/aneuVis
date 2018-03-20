
max_plots <- 10 # *maximum* total number of plots

ui <- shinyUI(pageWithSidebar(
  
  headerPanel("Dynamic number of plots"),
  
  sidebarPanel(
      fileInput(inputId = "files", label = "Upload", multiple = TRUE, accept = c(".xlsx"))
  ),
  
  mainPanel(
    # This is the dynamic UI for the plots
    uiOutput("plots")
  )
))


server <- shinyServer(function(input, output) {
  # Insert the right number of plot output objects into the web page
  
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
      
      aneuDat <- map2(.x = input$files$name, .y= tbl_list, .f = ~data.frame(clss=.x, .y)) %>% 
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
  
  
  output$plots <- renderUI({
    #input$n replaced with cl_ln
    cl_ln <- length(unique(aneuDat_r()$clss))
    plot_output_list <- lapply(1:cl_ln, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 280, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })

  classes <- reactive({unique(aneuDat_r()$clss)})
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        cls <- classes()[my_i]
        #aneuDat2 <- aneuDat_r() %>% filter(clss == cls)
        #hist(aneuDat2$chr_17, main = classes()[my_i])
        
        maxChr = 8
        maxChrPlus1 = maxChr + 1
        matr_plot <- return_chr_prop_matr(aneuDat_r(),cls, maxPair = maxChrPlus1) #
        plt <- create_perc_matr2(matr_plot, title = "", minChr = 1, 
                          maxChr = maxChrPlus1, xlab = "", ylab="")
        return(plt)
        
      })
    })
  }
})
shinyApp(ui, server)
