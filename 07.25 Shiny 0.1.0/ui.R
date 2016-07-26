



textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

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
      selectInput("data", "Select Dataset", c("Example","Upload Dataset","Simulate Dataset")),
      
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
      uiOutput("orderSelector"),
      hr(),
      sliderInput("nCluster",h4("Number of Clusters:"),   ## set number of clusters
                  min = 1, max = 20, value = c(2,5), step =1),
      actionButton("run", "Run Model")            ## run the model 
      
      
    ),
    
    mainPanel(
      tabsetPanel( id = "tabset",
                   
        ### Data ###
        tabPanel("Data",
          h3("Raw Data:"),
          DT::dataTableOutput('dataView'),
          h3("Summary:"),
          tabsetPanel(
            tabPanel('Plot', 
                     plotOutput('dataplot'),
                     checkboxInput("ggpairs", label = "ggpairs", value = FALSE)
            ),
            
            tabPanel('Table', align = 'center', 
                     style = "margin-top:50px;",
                     tableOutput('dataSummary'))
          )
          
        ),
        
        ### Clustering  ###
        tabPanel("Clustering", 
          
          h3("Summary"),
          tabPanel('DataView', DT::dataTableOutput('summary')), # summary table 
          uiOutput("indexSelector"),                  # summary display 
          plotOutput('plotSummary'),                  # summary plot
        
          hr(),
          selectInput("mChoose", h4("Choose Model By:"), c(Index,"Number of cluster")),
          conditionalPanel(
            condition= 'input.mChoose == "Number of cluster"',
            uiOutput("nclust")
          ),
          
          tabsetPanel(
            tabPanel("Profile",
                     fluidRow(
                       align = 'center',
                       style = "margin-top:50px;",
                       tableOutput("profile" )
                     ),
                     checkboxInput("detail", label = "Show details", value = FALSE),
                     hr(),
                     plotlyOutput(outputId = "plot1")
            ),
            tabPanel("ProbMeans",
                     fluidRow(
                       align = 'center',
                       style = "margin-top:50px;",
                       tableOutput("probMeans" )
                     ),
                     checkboxInput("detail1", label = "Show details", value = FALSE)
            )
          )
        ),
        
        ### Output ###
        tabPanel('Output',
          selectInput('outputSet', h3('Choose a output dataset:'),
                      choices = c("Dataset (with cluster)", "Profile", "ProbMeans")),
          downloadButton('downloadOutput','Download')),
        
        ### Simulate ###
        tabPanel('Simulate',
          style = "margin-top:5px;",
          
          numericInput('ngroup', h4('Number of groups'),value =3, min = 2, step =1),
          h4('Size of each group:'),
          fluidRow(
            uiOutput('sizeSelector')
          ),
          
          h4('Number of each kinds of variables:'),
          fluidRow(
            column(width = 2, numericInput('ncont', 'continuous',value = 3, min = 0, step = 1)),
            column(width = 2, numericInput('nord', 'ordinal',value = 2, min = 0, step = 1)),
            column(width = 2, numericInput('nnom', 'nominal',value = 2, min = 0, step = 1)),
            column(width = 2, numericInput('ncount', 'count',value = 2, min = 0, step = 1))
          ),
          
          checkboxInput('defineDT','Define Details', value = FALSE),
          
          fluidRow( uiOutput('cateOrd')),
          fluidRow( uiOutput('cateNom')),

          rHandsontableOutput("hot", width = 350),
          
          hr(),
          h3("Simulated Data:"),
          DT::dataTableOutput('simulatedData'),
          hr(),
          h3("Clustering Table:"),
          tableOutput("clustTable")
        )
      )
    )
  )
)