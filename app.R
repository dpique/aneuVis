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

minChr <- 3#1
maxChr <- 9
#gridSize <- maxChr - minChr + 1
all_perms = data.frame(gtools::permutations(maxChr-minChr + 1, 2, repeats.allowed = T)) + minChr - 1

all_perms2 = all_perms %>%
  as_tibble() %>%
  mutate(join_chr_state=paste(X1, X2, sep="_"))
  
aneuDatTbl <- table(aneuDat) %>% 
  as_tibble %>% 
  mutate(join_chr_state=paste(Ch.12, Ch.9, sep="_")) %>% 
  right_join(all_perms2, by=c("join_chr_state")) %>% 
  replace_na(list(n=0)) %>% 
  select(n, X1, X2) %>% 
  mutate(prop=n/sum(n),
         prop100=round(prop*100, 1),
         propChar=ifelse(prop100 !=0, prop100, "·"),
         propFill=log(prop*100+1, 10))

matr=aneuDatTbl

create_perc_matr2 <- function(matr, title, minChr, maxChr){#, outDir){
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
    xlab("Copies of Chromosome 9") + 
    ylab("Copies of Chromosome 12") + 
    #xlim(c(minChr, maxChr)) + 
    #ylim(c(minChr, maxChr)) +
    scale_x_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    scale_y_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    ggtitle(paste0("% Aneuploidy Across ", tot, " ", title, " Fibroblasts (FISH)"))
  #ggsave(filename = paste0(outDir, "/aneupl_", title, ".jpeg"), plot=x, device="jpeg", width = 6, height = 6, units = "in")
  return(x)
}
create_perc_matr2(matr, "test",minChr = minChr, maxChr = maxChr)
#create_perc_matr, maxChrPlus1, outDir=here("figures")

library(shiny)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  # App title ----
  titlePanel("AneuVis V0.1"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel = sidebarPanel(
      
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      textInput(inputId = "titletxt", label = "Insert title here", value = "title"),
      
      sliderInput("minChr", "Smallest Chr #:",
                  min = 0, max = 10,
                  value = 1),
      sliderInput("maxChr", "Largest Chr. #:",
                  min = 2, max = 100,
                  value = 9)
      # Input: Selector for variable to plot against mpg ----,
     #selectInput() 
      # Input: Checkbox for whether outliers should be included ----
      #checkboxInput("outliers", "Show outliers", TRUE)
      
    ), 
    
    # Main panel for displaying outputs ----
    mainPanel = mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      #plotOutput("mpgPlot")
      plotOutput("gridPlot"))
    )
  )


# Define server logic required to draw a histogram
#mpgData <- mtcars
#mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$titletxt)
  })
 
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
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
                      minChr=input$minChr, maxChr=input$maxChr)
  })
  
}
# Run the application 

shinyApp(ui = ui, server = server)

