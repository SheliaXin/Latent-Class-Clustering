data(Cars93)
options(stringsAsFactors = TRUE)
shinyServer(function(input, output, session) {
  options(stringsAsFactors = TRUE)
  # load/choose dataset
  
  
  dataset <- reactive({
    if(input$data == 'Example'){        
      if(input$ExampleData == 'Cars93')
        data <- Cars93[ ,c(3,5,8,10)]
      updateTabsetPanel(session, "tabset", selected = "Data")
    }else if(!is.null(input$LoadData)){                              
      file <- input$LoadData 
      data <- read.csv(file$datapath) 
      updateTabsetPanel(session, "tabset", selected = "Data")
    }else if(input$data =='Simulate Dataset'){
      data <- as.data.frame(simudata()$data[,-1])
      updateTabsetPanel(session, "tabset", selected = "Simulate")
    }else{
      data <- NULL
      updateTabsetPanel(session, "tabset", selected = "Data") }
    return(data)
  })
  
  
  
  #####################
  
  # select variables & frequency to be used in model 
  choices <- reactive( {                                # choices of variables
    as.list(names(dataset()))})
  
  output$variSelector <- renderUI({                     # select variables 
    checkboxGroupInput("VariSelected", h4("Select Variables:"),
                       choices = choices(), selected = choices())})
  
  output$freqSelector <- renderUI({                    # set frequency index 
    selectInput("freq", h4("Frequency Index:"), 
                c("NULL", choices()[!choices() %in% input$VariSelected]))})
  
  datasetS <- reactive({                               # final dataset 
    dataset()[,input$VariSelected, drop = FALSE] })
  
  output$typeSelector <- renderUI({                    # set variable types 
    if(!is.null(input$VariSelected)){
      L <- vector("list", length(input$VariSelected)) 
      
      auto <- lapply(dataset(), class)                  #  set variable type automaticly (normal/multinomial)
      for(i in 1:length(input$VariSelected)){
        
        if(auto[[input$VariSelected[i]]] != 'factor')
          autoSelected <- 'Continuous'
        else
          autoSelected <- 'Nominal'
        
        # set type for each input
        L[[i]] <- selectInput(paste0("tpVar_",i), input$VariSelected[i], varChoices ,
                              selected = autoSelected)
        
        }
      L 
    }})
  
  # segment variables by type
  variableType <- reactive({ 
    Ltype <- vector("list", length(varChoices))
    names(Ltype) <- varChoices 
    if(length(input$VariSelected)>0){
      for(i in 1:length(input$VariSelected)){
        for(j in 1:length(varChoices)){
          if(input[[paste0("tpVar_",i)]] == varChoices[j])
            Ltype[[varChoices[j]]] <- append(Ltype[[varChoices[j]]], i)
        }}
    }
    
    Ltype
  })
  
  # define the order for ordinal varibale
  output$orderSelector <- renderUI({
    L <- list()
    l <- length(variableType()[['Ordinal']])
    if(l >0){
      for( i in 1:l){
        j <- variableType()[['Ordinal']][i]
        M <- levels(datasetS()[,j])
        L[[i]] <- selectInput(paste0("odVar_",i), paste0("Order of ", colnames(datasetS())[j]), 
                              choices = M, multiple = TRUE)
      }
    }
    L
  })
  
  
  ####################
  
  # OUTPUT: view data in a table 
  output$dataView <- DT::renderDataTable({
    DT::datatable(datasetS(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })
  
  
  
  # OUTPUT: pairs plot
  output$dataplot <- renderPlot({
    if(!is.null(datasetS())){
      if(input$ggpairs){
          ggpairs(datasetS() )
      }else{
        if(input$run)
          pairs(datasetS(), 
                pch = 19,
                cex =0.75,
                
                col = rainbow( length(unique(mod()@cluster)),alpha = 0.5, start = 0.6, end = 0.10)[mod()@cluster] )
        else
          pairs( datasetS() )
      }
    }
  })
  

  
  # OUTPUT: summary table 
  output$dataSummary <- renderTable({
    if(!is.null(datasetS())){
      options(digits =3)
      summary(datasetS())
    }
  })
  
  #####################
  

  
  
  # run algorithm and calculate clusters
  clusters <- eventReactive(input$run, {
    # add frequency index [space cost; to improve!!]
    if(input$freq != "NULL"){
      df <- datasetS()
      df <- df[rep(seq_len(nrow(df)), as.integer(dataset()[,input$freq])),]
    }else
      df <- datasetS()
    
    ord = NULL
    if(length(variableType()$Ordinal) >0){
      ord <- vector("list", length(variableType()$Ordinal))
      for(i in 1:length(variableType()$Ordinal)){
        newOrd <- input[[paste0("odVar_",i)]]
        oldOrd <- levels(as.factor(datasetS()[,variableType()$Ordinal[i]]))
        if(!is.null(newOrd) && length(newOrd) == length(oldOrd)){
          ord[[i]] <- newOrd
        }else{
          ord[[i]] <- oldOrd
        }
      }
    }
    
    
    
    cc <- data.recode(df, continuous = variableType()$Continuous,
                      ordinal = variableType()$Ordinal,
                      nominal = variableType()$Nominal,
                      count = variableType()$Count_Poisson, order=ord )
    
    mdl <- initFlexmix(cc$data ~ 1, k = input$nCluster[1]:input$nCluster[2],
                model = mcmixed(continuous = cc$con.len,
                                ordinal = cc$ord.len,
                                nominal = cc$nom.len,
                                count.poi = cc$count.len,
                                ppdim = cc$ppdim[1:cc$nom.len],
                                diagonal = TRUE))
    
    return(list(cc = cc, mdl = mdl))
  })
  

    # OUTPUT: choose index to show 
    output$indexSelector <- renderUI({
      checkboxGroupInput("indexSelected", h4("Summary Display:"),
                         choices = mIndex, selected = dfIndex,inline = T)
    })

  
  # OUTPUT: summary stuff
    summaryIndex <- reactive({
      t <- show(clusters()$mdl)
      t[,input$indexSelected]
    })
    

  output$summary <- DT::renderDataTable({   # summary table 
    DT::datatable(summaryIndex(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })
  
  output$plotSummary <- renderPlot({        # summary plot
    plot(clusters()$mdl)
  })
  
  output$nclust <- renderUI({
    selectInput("Nclust", "Number of cluster",choices = as.character(input$nCluster[1]:input$nCluster[2]))
  })
  
  
  # Select model by some index 
  mod <- reactive({
    if(input$mChoose != "Number of cluster")
      m <- getModel(clusters()$mdl, which = input$mChoose)
    else
      m <- getModel(clusters()$mdl, which = input$Nclust) 

    m
  })
  
  
  # get the profile 
  prof <- reactive({
   # m <- getModel(clusters()$mdl, which = input$mChoose)
    profile(mod(), clusters()$cc)
  })
  
  
  output$plot1 <- renderPlotly({    # profile plot
    a <- list(                      # xaxis style
      title = "",
      showticklabels = TRUE,
      tickangle = 20,
      tickfont = list(size = 10, color = "black")
    )
    
    df <- cbind('id' = rownames(prof()$dfPlt), prof()$dfPlt)
    data_long <- as.data.frame(melt(df, id = "id"), stringsAsFactors = F)
    
    p <- data_long %>%
      plot_ly(x = id, y = value,  color = variable) %>%
      layout(xaxis = a)
  })
  
  output$profile <- renderTable({     # profile table 
    d <- prof()$dfPro
    if(clusters()$cc$con.len >0 && !input$detail){
      exRow <- as.vector(sapply(1:clusters()$cc$con.len,function(x) 1 + 2:6 + (x-1) * 6))
      return(d[-exRow,])
    }else
      return(d)
  },include.rownames=FALSE)
  
  output$probMeans <- renderTable({     # profile table 
    d <- prof()$dfPrM
    if(clusters()$cc$con.len >0 && !input$detail1){
      exRow <- as.vector(sapply(1:clusters()$cc$con.len,function(x) 1 + 2:6 + (x-1) * 6))
      return(d[-exRow,])
    }else
      return(d)
  },include.rownames=FALSE)
  
  # output$test <- renderTable({
  #   parameters(mod())
  # })
  dfClust <- reactive({
    cbind(dataset(), cIdx = mod()@cluster )
  })
  
  datasetOutput <- reactive({
    switch(input$outputSet,
           "Dataset (with cluster)" = dfClust(),
           "Profile" = prof()$dfPro,
           "ProbMeans"= prof()$dfPrM)
  })
  
  output$downloadOutput <- downloadHandler(
    filename = function() { paste(input$outputSet, '.csv', sep ='')},
    content = function(file){
      write.csv(datasetOutput(), file)
  })
  
  
  output$sizeSelector <- renderUI({
    L <- list()
    for(i in 1:input$ngroup){
      L[[i]] <- column(width = 2, numericInput(paste0('size',i), paste0('group',i),value = 50, min = 1, step = 1))
    }
    L
  })
  
  output$cateOrd <- renderUI({
    L <- list()
    if(input$defineDT && input$nord > 0){
      L[[1]] <- h4('Categories of each ordinal variables:')
      for(i in 1:input$nord){
        L[[i+1]] <- column(width = 2, numericInput(paste0('cateOrd',i), paste0('Variable',i),value = 3, min = 2, step = 1))
      }
    }
    L
  })
  
  output$cateNom <- renderUI({
    L <- list()
    if(input$defineDT && input$nnom > 0){
      L[[1]] <- h4('Categories of each nominal variables:')
      for(i in 1:input$nnom){
        L[[i+1]] <- column(width = 2, numericInput(paste0('cateNom',i), paste0('Variable',i),value = 3, min = 2, step = 1))
      }
    }
    L
  })
  
  
  # simulate parameters 
  simupara <- reactive({
    size <-c()
    for(i in 1:input$ngroup){
      size <- c(size, input[[paste0('size',i)]])
    }
    podim = NULL
    ppdim = NULL
    
    if(input$defineDT){
      if(input$nord>0){
        podim <- c()
        for(j in 1:input$nord){
          podim <- c(podim, input[[paste0("cateOrd",j)]])
        }
      }
      if(input$nnom>0){
        ppdim <- c()
        for(j in 1:input$nnom){
          ppdim <- c(ppdim, input[[paste0("cateNom",j)]])
        }
      }
      
    }
    
    
  
    DF <- simuData(ngroup = input$ngroup, size = size, 
                   cont = input$ncont, ord = input$nord, nom = input$nnom, count = input$ncount,
                   podim = podim, ppdim =ppdim)
    DF$statistics
  })  
  
  values = reactiveValues()
  
  data1 = reactive({
    DF = as.data.frame(t(simupara())) 
    
    if (!is.null(input$hot) ){
      DFnew = hot_to_r(input$hot)
      if(all(rownames(DFnew) == rownames(DF)))
        DF = DFnew
    }
    values[["para"]] = DF
    DF
  })
  
  
  simudata <- reactive({
    size <-c()
    for(i in 1:input$ngroup){
      size <- c(size, input[[paste0('size',i)]])
    }
    
    center = NULL; var = NULL
    centerOrd = NULL; varOrd = NULL; podim = NULL
    paraNom = NULL; ppdim = NULL
    lambda = NULL
    s <- 1
    if(input$defineDT){
      df <- as.data.frame(t( data1()))
      #size <- unlist(df[1,])
      if(input$ncont>0){
        center <- df[,2:(1+input$ncont),drop= FALSE]; s <- s+input$ncont
        var <- df[,(s+1):(s + input$ncont),drop= FALSE]; s<- s+input$ncont
      }
      if(input$nord>0){
        centerOrd <- df[,(s+1):(s+input$nord),drop= FALSE]; s <- s+input$nord
        varOrd <- df[,(s+1):(s+input$nord),drop= FALSE]; s <- s+input$nord
        podim <- c()
        for(j in 1:input$nord){
          podim <- c(podim, input[[paste0("cateOrd",j)]])
        }
      }
      if(input$nnom>0){
        ppdim <- c()
        for(j in 1:input$nnom){
          ppdim <- c(ppdim, input[[paste0("cateNom",j)]])
        }
        paraNom <- list()
        for(i in 1:input$nnom){
          paraNom[[i]] = df[,(s+1):(s+ppdim[i])]
          s<- s+ppdim[i]
        }
      }
      if(input$ncount >0){
        lambda = df[,(s+1):(s+input$ncount), drop = FALSE]
      }
      
    }

    
   DF <- simuData(ngroup = input$ngroup, size = size, 
                  cont = input$ncont, ord = input$nord, nom = input$nnom, count = input$ncount,
                  center = center, var = var, 
                  centerOrd = centerOrd, varOrd = varOrd, podim = podim,
                  paraNom = paraNom, ppdim = ppdim,
                  lambda = lambda)
   DF
  })  
  

  
  
  output$hot <- renderRHandsontable({
    if(input$defineDT){
      DF = data1()
      rhandsontable(DF, stretchH = "all", rowHeaderWidth = 100)
    }
  })
  
  output$simulatedData <- DT::renderDataTable({
    df <- simudata()$data
    df[,2:(1+input$ncont)] <- round(df[,2:(1+input$ncont)],2)
    DT::datatable(df,
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5,autoWidth = TRUE))
    
  })
  
  output$clustTable <- renderTable({
    t <- table(simudata()$data[,1], mod()@cluster)
  })

  
})