library(shiny)
library(ggplot2)
library(DT)
library(xtable)
library(plotly)

ExampleDataset = c("Mixed Normal and Multinormial" = "Cars93")
Index = c("AIC", "BIC", "ICL")

shinyUI(
  fluidPage(
  titlePanel('Latent Class Clustering'),
  
  sidebarPanel(
    
    ## load data 
    h4("Dataset:"),
    selectInput("data", "Select Dataset", c("Example","Load Data")),
    
    conditionalPanel(
      condition = "input.data == 'Example'",
      selectInput("ExampleData", "Example Dataset", ExampleDataset )
    ),
    
    conditionalPanel(
      condition = "input.data == 'Load Data'",
      fileInput('LoadData', 'Load Data',
                accept = c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
    ),
    
    
    uiOutput("variSelector"),
    hr(),
    
    h4("Variable Type:"),
    uiOutput("typeSelector"),
    
    # set number of cluster
    sliderInput("nCluster",h4("Number of Clusters:"), 
                min = 1, max = 20, value = c(2,5), step =1),
    
    hr(),
    uiOutput("indexSelector"),
    selectInput("mChoose", h4("Choose Model By:"), Index)
  ),
  
  mainPanel(
    h3("Data View:"),
    tabPanel('DataView', DT::dataTableOutput('dataView')),
    
    h3("Summary"),
    plotOutput('plotSummary'),
    tabPanel('DataView', DT::dataTableOutput('summary')),
    
    
    h3("Profile Plot:"),
    plotlyOutput(outputId = "plot1"),
    # verbatimTextOutput("test"),
    
    
    verbatimTextOutput("profile")
  )
  
  
))