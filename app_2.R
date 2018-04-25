#this file also contains extra code removed from app.R


library(shiny)
library(ggplot2)
library(gridExtra)

u <- shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("donum1", "Make #1 plot", value = T),
                             checkboxInput("donum2", "Make #2 plot", value = F),
                             checkboxInput("donum3", "Make #3 plot", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="plotgraph", 
                                              width = "100%", #"500px", #textOutput("nInputs"),
                                              height="400px")) #))
                ))))

s <- shinyServer(function(input, output) 
{
  set.seed(123)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
  })
  
  output$nInputs = renderText({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    return(paste0(length(ptlist)*500, "px"))#(length(ptlist)*500)
    #length(ptlist)*500
    
    #if (length(ptlist)==0) return(NULL)
    #grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
  
  
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
})
shinyApp(u,s)





## fluidPage(
##   titlePanel("submitButton example"),
##   fluidRow(
##     column(3, wellPanel(
##       sliderInput("n", "N:", min = 10, max = 1000, value = 200,
##                   step = 10),
##       textInput("text", "Text:", "text here"),
##       submitButton("Submit")
##     )),
##     column(6,
##            plotOutput("plot1", width = 400, height = 300),
##            verbatimTextOutput("text")
##     )
##   )
## )## 

#read_xlsx(path="testDat/ginkgo_key.xlsx")




library(shiny)
library(ggplot2)
library(gridExtra)

u <- shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("donum1", "Make #1 plot", value = T),
                             checkboxInput("donum2", "Make #2 plot", value = F),
                             checkboxInput("donum3", "Make #3 plot", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="plotgraph", width="500px",height="400px"))
                ))))

s <- shinyServer(function(input, output) 
{
  set.seed(123)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
  })
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
})
shinyApp(u,s)





########
library(AneuFinder)

results <- system.file("extdata", "primary-lung", "hmms", package="AneuFinderData")
files.lung <- list.files(results, full.names=TRUE)
results <- system.file("extdata", "metastasis-liver", "hmms", package="AneuFinderData")
files.liver <- list.files(results, full.names=TRUE)
## Get karyotype measures
k.lung <- karyotypeMeasures(files.lung)
k.liver <- karyotypeMeasures(files.liver)
## Print the scores in one data.frame
df <- rbind(lung = k.lung$genomewide, liver = k.liver$genomewide)
print(df)
## Aneuploidy Heterogeneity
## lung 0.9818370 0.4638431
## liver 0.9262749 0.3106615
## While the aneuploidy is similar between both cancers, the heterogeneity is
## nearly twice as high for the primary lung cancer.
plotHeterogeneity(hmms.list = list(lung=files.lung, liver=files.liver))

if (is.matrix(normalChromosomeNumbers)) {
  if (nrow(normalChromosomeNumbers) != length(hmms)) {
    stop("nrow(normalChromosomeNumbers) must be equal to length(hmms)")
  }
}
hmms <- loadFromFiles(hmms, check.class = class.univariate.hmm)
ptm <- startTimedMessage("Making consensus template ...")
binsizes <- unlist(lapply(hmms, function(x) {
  width(x$bins)[1]
}))
mask <- !sapply(hmms, function(hmm) {
  is.null(hmm$segments) | is.null(hmm$bins$state)
})
hmms <- hmms[mask]
if (all(binsizes == binsizes[1])) {
  consensus <- hmms[[1]]$bins
  mcols(consensus) <- NULL
  constates <- matrix(NA, ncol = length(hmms), nrow = length(consensus))
  for (i1 in 1:length(hmms)) {
    hmm <- hmms[[i1]]
    multiplicity <- initializeStates(levels(hmm$bins$state))$multiplicity
    constates[, i1] <- multiplicity[as.character(hmm$bins$state)]
  }
}
else {
  consensus <- consensusSegments(hmms)
  constates <- consensus$copy.number
}
stopTimedMessage(ptm)
if (!is.null(exclude.regions)) {
  ind <- findOverlaps(consensus, exclude.regions)@from
  constates <- constates[-ind, ]
  consensus <- consensus[-ind]
}
ptm <- startTimedMessage("Karyotype measures ...")
result <- list()
S <- ncol(constates)
physioState <- matrix(2, nrow = dim(constates)[1], ncol = dim(constates)[2], 
                      dimnames = list(chromosome = rownames(constates), sample = NULL))
if (!is.null(normalChromosomeNumbers)) {
  if (is.vector(normalChromosomeNumbers)) {
    mask <- rownames(physioState) %in% names(normalChromosomeNumbers)
    physioState[mask, ] <- normalChromosomeNumbers[rownames(physioState)[mask]]
  }
  else if (is.matrix(normalChromosomeNumbers)) {
    mask <- rownames(physioState) %in% colnames(normalChromosomeNumbers)
    physioState[mask, ] <- t(normalChromosomeNumbers[, 
                                                     rownames(physioState)[mask]])
  }
}
consensus$Aneuploidy <- rowMeans(abs(constates - physioState))
tabs <- apply(constates - physioState, 1, function(x) {
  sort(table(x), decreasing = TRUE)
})
if (is.list(tabs) | is.vector(tabs)) {
  consensus$Heterogeneity <- unlist(lapply(tabs, function(x) {
    sum(x * 0:(length(x) - 1))
  }))/S
}
else if (is.matrix(tabs)) {
  consensus$Heterogeneity <- colSums(tabs * 0:(nrow(tabs) - 
                                                 1))/S
}
weights <- as.numeric(width(consensus))
result[["genomewide"]] <- data.frame(Aneuploidy = stats::weighted.mean(consensus$Aneuploidy, 
                                                                       weights), Heterogeneity = stats::weighted.mean(consensus$Heterogeneity, 
                                                                                                                      weights))
consensus.split <- split(consensus, seqnames(consensus))
weights.split <- split(weights, seqnames(consensus))
result[["per.chromosome"]] <- data.frame(Aneuploidy = unlist(mapply(function(x, 
                                                                             y) {
  stats::weighted.mean(x$Aneuploidy, y)
}, consensus.split, weights.split)), Heterogeneity = unlist(mapply(function(x, 
                                                                            y) {
  stats::weighted.mean(x$Heterogeneity, y)
}, consensus.split, weights.split)))
if (!is.null(regions)) {
  regions <- subsetByOverlaps(regions, consensus)
  ind <- findOverlaps(consensus, regions, select = "first")
  consensus.split <- split(consensus, ind)
  weights.split <- split(weights, ind)
  regions$Aneuploidy <- unlist(mapply(function(x, y) {
    stats::weighted.mean(x$Aneuploidy, y)
  }, consensus.split, weights.split))
  regions$Heterogeneity <- unlist(mapply(function(x, y) {
    stats::weighted.mean(x$Heterogeneity, y)
  }, consensus.split, weights.split))
  result[["regions"]] <- regions
}
stopTimedMessage(ptm)
return(result)
