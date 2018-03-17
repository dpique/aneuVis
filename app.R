#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#aneuDat = young1
#saveRDS(aneuDat, "testDat.rds")
#aneuDat <- readRDS("testDat.rds")#./testDat.rds")
library(tidyverse)
library(shiny)

create_perc_matr2 <- function(matr, title, minChr, maxChr, xlab, ylab){
  tot= sum(matr$n)
  gridSize <- maxChr - minChr + 1
  x <- ggplot(matr, aes(x = X1, y = X2, fill= propFill)) + 
    geom_tile(color = "black") +  
    theme_classic() +
    theme(axis.text=element_text(size=19, colour = "black"), 
          axis.line = element_blank(), axis.ticks = element_blank()) +
    scale_fill_gradient(low = "white", high = "firebrick3", limits = c(0,2)) + 
    geom_text(size = 4.5, aes(label = propChar)) + 
    coord_fixed() +
    xlab(xlab) + 
    ylab(ylab) + 
    scale_x_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    scale_y_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    ggtitle(paste0("% Aneuploidy Across ", tot, " ", title, "Observations"))
  #ggsave(filename = paste0(outDir, "/aneupl_", title, ".jpeg"), plot=x, device="jpeg", width = 6, height = 6, units = "in")
  return(x)
}


ui <- fluidPage(
  titlePanel("aneuvis v.0.1"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      
      fileInput("file1", "Choose Excel File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           "application/vnd.ms-excel",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      tags$hr(),
      textInput(inputId = "titletxt", label = "Insert main title here", value = "title"),
      textInput(inputId = "xlab", label = "x-axis title:", value = "Chr_column_1"),
      textInput(inputId = "ylab", label = "y-axis title:", value = "Chr_column_2"),
      
      
      sliderInput("minChr", "Smallest Chr #:",
                  min = 0, max = 10,
                  value = 1),
      sliderInput("maxChr", "Largest Chr. #:",
                  min = 2, max = 100,
                  value = 9)
    ), 
    
    mainPanel = mainPanel(
      h3(textOutput("caption")),
      plotOutput("gridPlot"))
    )
  )


server <- function(input, output) {

  formulaText <- reactive({
    paste(input$titletxt)
  })
 
  output$caption <- renderText({
    formulaText()
  })

  output$gridPlot <- renderPlot({
    req(input$file1)
    df <- readxl::read_excel(input$file1$datapath)
    aneuDat <- df#[1:2,]
    minChr <- input$minChr#1
    maxChr <- input$maxChr#9
    gridSize <- maxChr - minChr + 1
    all_perms = data.frame(gtools::permutations(maxChr-minChr + 1, 2, repeats.allowed = T)) + minChr - 1
    
    all_perms2 = all_perms %>%
      as_tibble() %>%
      mutate(join_chr_state=paste(X1, X2, sep="_"))
    
    aneuDatTbl <- table(aneuDat) %>% 
      as_tibble %>% 
      unite("join_chr_state", 1:2, sep = "_") %>%
      right_join(all_perms2, by=c("join_chr_state")) %>% 
      replace_na(list(n=0)) %>% 
      select(n, X1, X2) %>% 
      mutate(prop=n/sum(n),
             prop100=round(prop*100, 1),
             propChar=ifelse(prop100 !=0, prop100, "·"),
             propFill=log(prop*100+1, 10))

    create_perc_matr2(aneuDatTbl, title = "test", 
                      minChr=input$minChr, maxChr=input$maxChr, 
                      xlab = input$xlab, ylab = input$ylab)
  })
  
}
# Run the application 

shinyApp(ui = ui, server = server)

