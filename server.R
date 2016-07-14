
data(Cars93)

shinyServer(function(input, output, session) {
  
  # load/choose dataset
  dataset <- reactive({
    if(input$data == 'Example'){        
      if(input$ExampleData == 'Cars93')
        data <- Cars93[ ,c(3,5,8,10)]
    }else if(!is.null(input$LoadData)){                              
      file <- input$LoadData 
      data <- read.csv(file$datapath) 
    }else{
      data <- NULL }
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
                              selected = autoSelected)}
      L 
    }})
  
  ####################
  
  # OUTPUT: view data in a table 
  output$dataView <- DT::renderDataTable({
    DT::datatable(datasetS(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })
  
  # OUTPUT: pairs plot
  output$dataplot <- renderPlot({
    if(!is.null(datasetS()))
      pairs(datasetS())
  })
  
  # OUTPUT: summary table 
  output$dataSummary <- renderTable({
    if(!is.null(datasetS())){
      options(digits =3)
      summary(datasetS())
    }
  })
  
  #####################
  
  # segment variables by type
  variableType <- reactive({ 
    Ltype <- vector("list", length(varChoices))
    names(Ltype) <- varChoices 
    
    for(i in 1:length(input$VariSelected)){
      for(j in 1:length(varChoices)){
        if(input[[paste0("tpVar_",i)]] == varChoices[j])
          Ltype[[varChoices[j]]] <- append(Ltype[[varChoices[j]]], i)
      }}
    Ltype
  })
  
  # run algorithm and calculate clusters
  clusters <- eventReactive(input$run, {
    
    # add frequency index [space cost; to improve!!]
    if(input$freq != "NULL"){
      df <- datasetS()
      df <- df[rep(seq_len(nrow(df)), as.integer(dataset()[,input$freq])),]
    }else
      df <- datasetS()
    
    cc <- data.recode(df, continuous = variableType()$Continuous,
                      ordinal = variableType()$Ordinal,
                      nominal = variableType()$Nominal,
                      count = variableType()$Count_Poisson)
    
    initFlexmix(cc$data ~ 1, k = input$nCluster[1]:input$nCluster[2],
                model = mcmixed(continuous = cc$con.len,
                                ordinal = cc$ord.len,
                                nominal = cc$nom.len,
                                count.poi = cc$count.len,
                                ppdim = cc$ppdim[(cc$ord.len+1):(cc$ord.len+cc$nom.len)],
                                diagonal = TRUE))
  })
  

    # OUTPUT: choose index to show 
    output$indexSelector <- renderUI({
      checkboxGroupInput("indexSelected", h4("Summary Display:"),
                         choices = mIndex, selected = dfIndex,inline = T)
    })

  
  # OUTPUT: summary stuff, cut the tail several lines of the output from clusters() 
  summaryIndex <- reactive({
    d <- capture.output(clusters())
    dd <- tail(d, input$nCluster[2]- input$nCluster[1]+2)
    tt <- strsplit(dd,"\\s+")
    t <- do.call(rbind, lapply(seq_along(tt), function(i){tt[[i]]}))
    t[1,1] <- "nCluster" 
    colnames(t) <- t[1,]
    t[-1,input$indexSelected]
  })
  
  output$summary <- DT::renderDataTable({   # summary table 
    DT::datatable(summaryIndex(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })
  
  output$plotSummary <- renderPlot({        # summary plot
    plot(clusters())
  })
  
  
  # Select model by some index 
  mod <- reactive({
    getModel(clusters(), which = input$mChoose)
  })
  
  
  # plot of each parameters  ## change this !
  para <- reactive( {                  # parameters
    parameters(mod())
  })
  
  ##############  stop here 07.12
  ######## make this to a function  
  
  
  data_long <- reactive({             # long data for plot 
    n_c <- length(variableType()$Continuous) 
    n_o <- length(variableType()$Ordinal)
    n_n <- length(variableType()$Nominal)
    
    # names the parameters
    namep <- c()
    if(n_c >0){
      datap <- as.data.frame(para())[ -seq(n_c + 1, n_c + n_c*n_c),]   # exclude covariance, need to change if diaginal = FALSE
      namep <- append(namep, names(datasetS()[,variableType()$Continuous]))
    }else{
      datap <- as.data.frame(para())
    }
    
    if( n_o >0){
      datap <-datap[-seq(n_c+n_o+1, n_c+n_o + n_o*n_o),] # exclude covariance 
      nameDis <- melt(lapply(datasetS(),levels)[variableType()$Ordinal])[,c(2,1)]
      namep <- append(namep, apply(nameDis, 1, paste, collapse = "-"))
    }
    
    if( n_n >0){
      nameDis <- melt(lapply(datasetS(),levels)[variableType()$Nominal])[,c(2,1)]
      namep <- append(namep, apply(nameDis, 1, paste, collapse = "-"))
    }
    
    datap <- cbind("id" = namep, datap)
    
    # scale (0-1) mean of continuous data 
    if(n_c>1){
      minV <- apply(datasetS()[,variableType()$Continuous],2,min)
      maxV <- apply(datasetS()[,variableType()$Continuous],2,max)
      datap[1:n_c, -1] <- (datap[1:n_c, -1] - minV) / (maxV - minV) 
    }else if(n_c ==1){
      minV <- min(datasetS()[,variableType()$Continuous])
      maxV <- max(datasetS()[,variableType()$Continuous])
      datap[1, -1] <- (datap[1, -1] - minV) / (maxV - minV) 
    }
    
    as.data.frame(melt(datap, id = "id"), stringsAsFactors = F)
  })
  
  
  
  output$plot1 <- renderPlotly({
    a <- list(                  # xaxis style
      title = "",
      showticklabels = TRUE,
      tickangle = 20,
      tickfont = list(size = 10, color = "black")
    )
    p <- data_long() %>%
      plot_ly(x = id, y = value,  color = variable) %>%
      layout(xaxis = a)
  })
  
  output$profile <- renderPrint({  # profile 
    para()
  })
  
  output$test <- renderPrint({
    head(data_long())
  })
  
})