library(shiny)

allSame <- function(x) length(unique(x)) == 1

num_list = as.list(1:26)
names(num_list) = letters

char_to_num <- function(char){
  if(is.null(char) | is.na(char)){
    return(NULL)
  }
  splt <- unlist(strsplit(char, split = ""))
  numbrs <- num_list[splt]
  return(as.numeric(paste0(numbrs, collapse = "")))
}

ui <- shinyUI(bootstrapPage(
  headerPanel("Reset / Submit file input example"),
  sidebarPanel(
    fileInput(
      'file1', 
      span(".xlsx or .xls",
           tags$a(
             target = "_blank", 
             "(example data)",
             href = "https://docs.google.com/uc?export=download&id=1ZO9jWicY-5WohvGbQi_WrQWcDaram5wZ"
           )
      ),
      accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"),
      multiple = TRUE 
    ),    fluidRow(
      column(4,
             actionButton('reset', 'Reset Input')
      ))
  ),
  
  mainPanel(
    h4("Summary"),
    verbatimTextOutput("summary"),
    verbatimTextOutput("txt"),
    p("Message:"),
    verbatimTextOutput("msg")
  )
))

server <- shinyServer(function(input, output, session) {
  values <- reactiveValues(
    upload_state = NULL,
    count = 0
  )
  
  observeEvent(input$file1, {
    values$upload_state <- 'uploaded'
    values$count <- values$count + 1
    print("values$count - file1")
    print(values$count)
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
    values$count <- values$count + 1
    print("values$count - reset")
    print(values$count)
  })
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  
  
  df_test <- eventReactive({char_to_num(paste0(unlist(input$file1$name), collapse = "")) | input$reset}, {
    df <- retFishDf_head3(fish_name = file_input()$name, fish_datapath = file_input()$datapath)
    print("df:")
    print(df)
    return(df)
  })
  
  msg <- eventReactive({char_to_num(paste0(unlist(input$file1$name), collapse = "")) | input$reset}, {
    #df_test()
    #df <- retFishDf_head3(fish_name = file_input()$name, fish_datapath = file_input()$datapath)
    if(is.null(df_test())){
      return("Plz upload data")
    } else if(allSame(df_test())){
      return("Data uploaded successfully!")
    } else {
      return("Please check column headers")
    }
    
  })
  
  output$txt <- renderText({unlist(df_test())})
  
  output$msg <- renderText({msg()})
  
  #txt <- renderText({
  #  file_input()
  #})
  
  output$summary <- renderText({
    return(paste("Uploaded file:", file_input()$name))
  })
})

shinyApp(ui = ui, server = server)