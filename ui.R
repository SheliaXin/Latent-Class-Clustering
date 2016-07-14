
ExampleDataset = c("Mixed Normal and Multinormial" = "Cars93")
Index = c("AIC", "BIC", "ICL")

shinyUI(
  fluidPage( 
    theme = "simple.css",
    
  headerPanel(                                  #d9230f
    h1('Latent Class Clustering',
        style = "height: 200px; color: #A52A2A; background-color: 	#DCDCDC;
                 font-size: 300%; font-weight: 600; 
                 line-height: 3cm; padding-left: 1cm;")),
  
  sidebarPanel(
    h4("Dataset:"),                            ## load data 
    selectInput("data", "Select Dataset", c("Example","Upload Dataset")),
    
    conditionalPanel(
      condition = "input.data == 'Example'",    # Example dataset 
      selectInput("ExampleData", "Example Dataset", ExampleDataset )),
    
    conditionalPanel(                           # load dataset 
      condition = "input.data == 'Upload Dataset'",
      fileInput('LoadData', 'Upload dataset',
                accept = c('text/csv', 
                          'text/comma-separated-values,text/plain', 
                          '.csv'))),
    hr(),
    uiOutput("variSelector"),                   ## select variables 
    uiOutput("freqSelector"),                   ## select frequency 
   
    
    h4("Variable Type:"),                       ## set variables' type 
    uiOutput("typeSelector"),
    hr(),
    sliderInput("nCluster",h4("Number of Clusters:"),   ## set number of clusters
                min = 1, max = 20, value = c(2,5), step =1),
    actionButton("run", "Run Model")            ## run the model 
    
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data",
               h3("Raw Data:"),
               DT::dataTableOutput('dataView'),
               h3("Summary:"),
               tabsetPanel(
                 tabPanel('plot', plotOutput('dataplot')),
                 tabPanel('Table', align = 'center', 
                          style = "margin-top:50px;",
                          tableOutput('dataSummary')))
               ),
             
      tabPanel("Clustering", 
               
               h3("Summary"),
               
               tabPanel('DataView', DT::dataTableOutput('summary')), # summary table 
               uiOutput("indexSelector"),                  # summary display 
               plotOutput('plotSummary'),                  # summary plot
               
               
               hr(),
               
               selectInput("mChoose", h4("Choose Model By:"), Index),
               
               h3("Profile Plot:"),
               plotlyOutput(outputId = "plot1"),
               verbatimTextOutput("test"),
               verbatimTextOutput("profile")
               )
      
  
  )
)
  
  
))