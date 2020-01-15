library(shiny)
library(shinyjs)
library(dplyr)
library(shinycssloaders)
library(shinyalert)
library(shinyBS)
library(shinythemes)

shinyUI(fluidPage(
  tags$style("
              body {
             -moz-transform: scale(1, 1); /* Moz-browsers */
             zoom: 1; /* Other non-webkit browsers */
             zoom: 100%; /* Webkit browsers */
             }"),
  
  useShinyalert(),
  fluidPage(theme = shinytheme("journal")),
  fluidRow(
    column(4,style="background-color:#F3EFEF",
        div(style="height:35px;"),
        div(style="font-size: 50px;font-family:Garamond;font-style:italic;font-style:bold;color:#914646","Data Cleaner"),
        
        br(),
        
        fileInput("clinCapture", "Upload clinCapture CSV File",
              accept = c("text/csv","text/comma-separated-values,text/plain",
                         ".csv")),
        br(),
    
        fileInput("completedCreekside", "Upload Creekside Completed Visits CSV File",
              accept = c("text/csv","text/comma-separated-values,text/plain",
                         ".csv")), 
        br(),
      
        fileInput("completedMulberry", "Upload Mulberry Completed Visits CSV File",
                accept = c("text/csv","text/comma-separated-values,text/plain",
                           ".csv")), 
        br(),
      
        popify(actionButton("clean","Clean Data",class="btn-primary"),"Make sure file-type is CSV!!!",placement="top",trigger = "hover"),
        br(),
        br()
    ),
    column(4,div(style="height:250px;")),
    
    column(4,style="background-color:#F3EFEF;",
        div(style="height:35px;"),
        div(style="font-size: 50px;font-family:Garamond;font-style:italic;font-style:bold;color:#914646","Report Generator"),
        
        br(),
        
        fileInput("cleanedFile", "Upload Cleaned CSV File",
        
        accept = c("text/csv","text/comma-separated-values,text/plain",
        ".csv")),
        br(), #There should be a better way
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        downloadButton("report","Generate Report",class="btn-primary"),
        br(),
        br()
        
      )
    ),
  
  mainPanel(
    
    bsModal("popup","Download CSV file","clean",size="large",textOutput("text"),
            downloadButton("downloadData","Download"))
    
  )
))

