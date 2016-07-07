library(flexmix)
library(fpc)
library(ggplot2)
library(DT)
library(shiny)
library(MASS)
library(plotly)
library(reshape2)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
VariableChoose = c("Continuous", "Normial", "Ordinal", "Count (Poisson)", "Count (Binomial)")
mIndex = c("nCluster","logLik", "AIC", "BIC", "ICL", "converged","iter")
dfIndex = c("nCluster","logLik", "AIC", "BIC", "ICL")
data(Cars93)

shinyServer(function(input, output, session) {
  # load/choose dataset
  dataset <- reactive({
    if(input$data == 'Example'){
      if(input$ExampleData == 'Cars93'){
        data <- Cars93[,c(3,5,8,10)]
      }
    }else{
      file <- input$LoadData
      data <- read.csv(file$datapath)
    }
    return(data)
  })
  
  # choose variables 
  output$variSelector <- renderUI({
    choose <- as.list(names(dataset()))
    checkboxGroupInput("VariSelected", h4("Variable Select:"),
                       choices = choose, selected = choose)
  })
  
  datasetS <- reactive({
    dataset()[,input$VariSelected, drop = FALSE]
  })
  
  # view data 
  output$dataView <- DT::renderDataTable({
    DT::datatable(datasetS(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })
  
  # choose variable types 
  output$typeSelector <- renderUI({
    choose <- as.list(input$VariSelected)
    L <- vector("list",length(choose))
    auto <- lapply(dataset(), class)
    for(i in 1:length(choose)){
      if(auto[[choose[[i]]]] != 'factor'){
        autoSelected <- 'Continuous'
      }else{
        autoSelected <- 'Normial'
      }
      L[[i]] <- selectInput(choose[[i]], choose[i], VariableChoose,
                            selected = autoSelected)
    }
    L
  })

  variableType <- reactive({
    choose <- as.list(input$VariSelected)
    Ltype <- vector("list",length(VariableChoose))
    names(Ltype) <- VariableChoose
     for(i in 1:length(choose)){
       for(j in 1:length(VariableChoose)){
         if(input[[choose[[i]]]] == VariableChoose[j]){
           Ltype[[VariableChoose[j]]] <- append(Ltype[[VariableChoose[j]]], i)
         }
       }
    }
    Ltype
  })
  
#   output$test <- renderPrint({
#     variableType()
#   })
  
  # run algorithm and calculate clusters
  clusters <- reactive({
    
    cc <- discrete.recode(datasetS(), xvarsorted = FALSE, 
                          continuous = variableType()$Continuous, 
                          discrete = variableType()$Normial)
    
    initFlexmix(cc$data ~ 1, k = input$nCluster[1]:input$nCluster[2],
            model = lcmixed(continuous = cc$continuous,
                            discrete = cc$discrete,
                            ppdim = cc$ppdim,
                            diagonal = TRUE))
  })
  
  
  # choose model index
  output$indexSelector <- renderUI({
    checkboxGroupInput("indexSelected", h4("Variable Select:"),
                       choices = mIndex, selected = dfIndex)
  })
   

  # output summary stuff
  summaryIndex <- reactive({
        d <- capture.output(clusters())
        dd <- tail(d, input$nCluster[2]- input$nCluster[1]+2)
        tt <- strsplit(dd,"\\s+")
        t <- do.call(rbind, lapply(seq_along(tt), function(i){tt[[i]]}))
        t[1,1] <- "nCluster" 
        colnames(t) <- t[1,]
        t[-1,input$indexSelected]
  })
    
  output$summary <- DT::renderDataTable({
    DT::datatable(summaryIndex(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })
  
  output$plotSummary <- renderPlot({ 
    plot(clusters())
  })
  
  
  mod <- reactive({
    getModel(clusters(), which = input$mChoose)
  })
   
  
  # plot of each parameters
  para <- reactive({  # parameter 
    parameters(mod())
  })
  
  # data used in ggplot
  data_long <- reactive({ 
    n_c <- length(variableType()$Continuous) 
    datap <- as.data.frame(para())[-seq(n_c + 1, n_c + n_c*n_c),]  # exclude covariance ,need to change if diaginal = FALSE
    
    # names the parameters
    nameDis <- melt(lapply(datasetS(),levels)[variableType()$Normial])[,c(2,1)]
    namep <- c(names(datasetS()[,variableType()$Continuous]),  
               apply(nameDis, 1, paste, collapse = "-"))
    datap <- cbind("id" = namep, datap)
    # datap <- cbind("id" = rownames(datap),datap)
    
    # scale (0-1) mean of continuous data 
    minV <- apply(datasetS()[,variableType()$Continuous],2,min)
    maxV <- apply(datasetS()[,variableType()$Continuous],2,max)
    datap[1:n_c, -1] <- (datap[1:n_c, -1] - minV) / (maxV - minV) 
    as.data.frame(melt(datap, id = "id"), stringsAsFactors = F)
  })
  
  output$plot1 <- renderPlotly({
    a <- list(
      title = "",
      showticklabels = TRUE,
      tickangle = 20,
      tickfont = list(size = 10, color = "black")
    )
    
    p <- data_long() %>%
      plot_ly(x = id, y = value,  color = variable) %>%
      layout(xaxis = a)
  })
  
  output$profile <- renderPrint({
    para()
  })
  
})