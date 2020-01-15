library(shiny)
library(shinyalert)
library(shinyBS)
library(rmarkdown)
library(tidyr)
library(dplyr)

clean_data <- function(table,cleanRowsCreek,cleanRowsMulberry) {
  
  #Delete data dictionary.
  #table = table[-1:-193,]
  
  
  #Change col names to be able select Screening Variables
  #Take the first row as column names
  colnames(table) = as.matrix(table[1, ])
  
  
  #Delete first row
  table = table[-1, ]
  
  #Rename first 4 column, file format doesn't change
  colnames(table)[1:4] = c("Screening", "Screening",  "Screening",  "Screening")
  
  #Delete Event Occurence column which is an empty col. There is also another reason to delete them...
  #Dplyr doesn't like same column names. When we delete a column from data frame. Col names are renamed as
  #Screening.1 ,  Screening.2
  table = table[,-5]
  
  #Select Screening Variables
  screeningVariables = table %>%
    select(starts_with("Screening"))
  #Change col names 
  colnames(screeningVariables) = as.matrix(screeningVariables[3, ])
  
  #Delete first 3 rows
  screeningVariables = screeningVariables[-1:-3, ]
  
  # Create a data frame to grab the variable names and match them with the appropriate ones
  var = colnames(screeningVariables)
  dummy = c(1:length(var))
  distVariables = data.frame(var,dummy)
  
  # Create another data frame for variables such as SportType_E1_C18_1, SportType_E1_C19_1,
  # SportType_E1_C18_11, SportType_E1_C19_1. We'll select them with combination of start with and the last 2 strings...
  #new column for names
  distVariables$name = distVariables$var
  endWith = distVariables %>%
    separate(var,into = c('var1', 'var2'), sep = -2, convert = TRUE)
  
  #To be able to use "match" function for last two string of each variable, we need to add $ sign at the end of each variable
  endWith$var2 = sapply(endWith$var2, paste, "$", sep="")
  
  #Get rid off the strings starts with _C
  distVariables$var = sub("_C.*", "", distVariables$var)
  
  #Combine the columns
  for(i in 1:nrow(distVariables)) {
    select_column = screeningVariables %>%
      select(starts_with(as.character(distVariables[i,1]))) %>%
      select(-matches("_E5_.._|_E5_..._|_E5_...._"))%>%
      colnames()
    
    if(length(select_column) > 0) {
      screeningVariables= screeningVariables %>%
        unite(!!distVariables[i,1],select_column, sep = "") }
    else  {
      select_column = screeningVariables %>%
        select(starts_with(as.character(distVariables[i,1]))) %>%
        select(matches(as.character(endWith[i,2]))) %>%
        colnames()
      screeningVariables = screeningVariables %>%
        unite(!!endWith[i,4],select_column, sep = "")
    }
  }
  
  #Get rid off unnecessary strings
  colnames(screeningVariables) = sub("_E.*","",colnames(screeningVariables))
  
  #There are same column names which have same name. We'll add and delete a dummy column to have the column name
  # like Sportype.1, Sportype.2
  dummy_2 = c(1:length(screeningVariables$`Study Subject ID`))
  screeningVariables$dummy_2 = dummy_2 
  screeningVariables = screeningVariables[,-ncol(screeningVariables)]
  
  #Remove dots
  names(screeningVariables) =gsub("\\.", "", names(screeningVariables))
  
  #Remove spaces among Subject ID if any. This is important since we filter them based on nchar == 8
  screeningVariables$`Study Subject ID`<- gsub('\\s+', '', screeningVariables$`Study Subject ID`)
  
  #Delete rows which don't have length of 8 for Subject ID
  goodRows = ifelse(nchar(screeningVariables$`Study Subject ID`) == 8, TRUE, FALSE)
  screeningVariables = screeningVariables[goodRows,]
  
  #Cleaning rows based on completed visits files Creek and Mulberry
  #Merge Creekside and Mulberry
  completedVisit1 = merge(cleanRowsCreek,cleanRowsMulberry,all = TRUE)
 
  colnames(completedVisit1) [1:9] = c("Subject", "Study Arm", "Age", "Age Group", "Gender", "Completed_V1", "Completed_V2", "Completed_V3", "Completed_V4_5")
  
  #filter out incompleted first visits
  completedVisit1 = completedVisit1 %>%
    filter(grepl("Y", Completed_V1, ignore.case = TRUE))
  
  eligible = completedVisit1$Subject
  filtered_table = filter(screeningVariables, `Study Subject ID` %in% eligible)
  
  return(filtered_table) #Return the finished data.frame
}


options(shiny.maxRequestSize=30*1024^2) 



shinyServer(function(input, output) {
  data <- reactive({
    infile <- input$clinCapture #clinCapture file
    if(is.null(infile)){
      #use has not upload it yet
      return(NULL)}
    
    table <- read.csv(file=infile$datapath,sep = ",")
    return(table)
  })
  
  #data for Creekside completed visits
  data2 <- reactive({
    infile2 <- input$completedCreekside #Creek completed visits
    if(is.null(infile2)){
      #use has not upload it yet
      return(NULL)}
    
    table2 <- read.csv(file=infile2$datapath,sep = ",")
    return(table2)
  })
  
  #data for Mulberry completed visits
  data3 <- reactive({
    infile3 <- input$completedMulberry #Creek completed visits
    if(is.null(infile3)){
      #use has not upload it yet
      return(NULL)}
    
    table3 <- read.csv(file=infile3$datapath,sep = ",")
    return(table3)
  })
  
  
  buttonClicked <- eventReactive(input$clean, {
    shinyalert::shinyalert("Warning!", "This process may take up to 2 minutes", type = "info")
    clean_data(data(),data2(),data3())
  })
  
  output$text <- renderText({
    "Click Download! Make sure the file has been successfully downloaded before closing this window. If not, click Download again."
  })
  
  output$downloadData <- downloadHandler(
    filename = "cleanedData.csv",
    content = function(file) {
      write.csv(buttonClicked(),file,row.names = FALSE,na="")
    }
  )
  ##########REPORT GENERATOR
  # dataReport <- reactive({
  #  infileReport <- input$cleanedFile #clinCapture file
  # if(is.null(infileReport)){
  #use has not upload it yet
  #  return(NULL)}
  
  #tableReport <- read.csv(file=infileReport$datapath,sep = ",")
  #return(tableReport)
  #})
  
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      shinyalert::shinyalert("Warning!", "This process may take up to 2 minutes", type = "info")
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      infile4 <- input$cleanedFile
      params <- list(n = infile4$datapath)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      
    }
  )
  
})



#runApp()
