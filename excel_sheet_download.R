filename = "result.xlsx"
content = function(file) {
  column_data = data()[[input$column]]
  unique_values = unique(column_data)
  write.xlsx(data()[data()[[input$column]] == unique_values[1],], file, as.character(unique_values[1]))
  for (i in 2:length(unique_values)) {
    write.xlsx(data()[data()[[input$column]] == unique_values[i],], file, as.character(unique_values[i]), append = TRUE)
  }
}


#library(xlsx) #load the package
require(openxlsx)
list_of_datasets <- list("Scores By Group" = g4, "Scores By Group and Chromosome" = g4)
write.xlsx(list_of_datasets, file = paste0(Sys.Date(), "-aneuvis-stats.xlsx"))


saveWorkbook(workbook.sheets, "test.excelfile.xlsx") # and of course you need to save it.
