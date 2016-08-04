
shinyServer(function(input, output, session) {
  options(stringsAsFactors = TRUE)
  options(shiny.trace = FALSE)

  # choose dataset
  dataset <- reactive({
    if(input$data == 'Example'){
      updateTabsetPanel(session, "tabset", selected = "Data")
      if(input$ExampleData == 'Cars93'){
        data(Cars93)
        data <- Cars93[ ,c(3,5,8,10)]
      }
    }else if(input$data == 'Upload Dataset' && !is.null(input$LoadData)){
      updateTabsetPanel(session, "tabset", selected = "Data")
      file <- input$LoadData
      data <- read.csv(file$datapath, stringsAsFactors = TRUE)

    }else if(input$data == 'Simulate Dataset' ){
      updateNavlistPanel(session, "tabset", selected = "Simulate")
      if(!is.null(simudata())){
        data <- as.data.frame(simudata()$data[,-1, drop=FALSE])
      }else
        data <- NULL
    }else{
      updateTabsetPanel(session, "tabset", selected = "Data")
      data <- NULL }
    return(data)
  })

  # show simulate panel while using simulate dataset
  observe({
    toggle(condition = input$data == 'Simulate Dataset', selector = "#tabset li a[data-value=Simulate]")
  })

  # choose variables & frequency index
  choices <- reactive( {                              # choices of variables
    # valid check
    validate(
      need(!is.null(dataset()) , "Please choose a valid dataset")
    )
    if(!is.null(colnames(dataset()))){
      c <- as.list(colnames(dataset()))
    }else
      c <- paste0("V_", 1:ncol(dataset()))
    c
  })

  output$variSelector <- renderUI({                     # select variables
    checkboxGroupInput("VariSelected", h4("Variables:"),
                       choices = choices(), selected = choices())
  })

  output$freqSelector <- renderUI({                    # set frequency index
    if(input$containFreq){
      selectInput("freq", h5("Frequency Index:"),
                  choices()[!choices() %in% input$VariSelected])
    }
  })

  output$covariate <- renderUI({                       # set covariates
    if(input$containCov){
      checkboxGroupInput("CovSelected", h5("Covariates:"),
                         choices = choices()[!choices() %in% input$VariSelected])
    }else{
      NULL
    }
  })

  datasetS <- reactive({                               # dataset with selected variables
    validate(need(!is.null(input$VariSelected),"Please choose some variables"))
    df <- dataset()[,input$VariSelected, drop = FALSE]
    if(!is.null(input$CovSelected)){
      df <- data.frame(df, dataset()[ ,input$CovSelected, drop = FALSE])
    }
    df
  })

  ## test
  output$test <- renderTable({
    head(datasetS())
  })

  # covariate data frame
  covariates <- reactive({
    if(!is.null(input$CovSelected)){
      dataset()[,input$CovSelected, drop = FALSE]
    }else{
      NULL
    }

  })

  output$typeSelector <- renderUI({                    # set variable types
    L <- list()
    if(!is.null(dataset()) && !is.null(input$VariSelected)){

      auto <- lapply(dataset(), class)                  #  set automaticly (normal/multinomial)
      for(i in 1:length(input$VariSelected)){
        if(auto[[input$VariSelected[i]]] != 'factor')
          autoSelected <- 'Continuous'
        else
          autoSelected <- 'Nominal'
        L[[i]] <- selectInput(paste0("tpVar_",i), input$VariSelected[i], varChoices ,
                              selected = autoSelected)
      }
      L
    }})

  # list of variable types
  variableType <- reactive({
    Ltype <- vector("list", length(varChoices))
    names(Ltype) <- varChoices
    if(length(input$VariSelected)>0){
      for(i in 1:length(input$VariSelected)){
        for(j in 1:length(varChoices)){
          if(input[[paste0("tpVar_",i)]] == varChoices[j])
            Ltype[[varChoices[j]]] <- append(Ltype[[varChoices[j]]], i)
        }
      }
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
        M <- levels(as.factor(datasetS()[,j]))
        L[[i]] <- selectInput(paste0("odVar_",i),
                              paste0("Order of ", colnames(datasetS())[j]),
                              choices = M, multiple = TRUE)
      }
    }
    L
  })

  ########################
  # Main Panel
  ########################

  ### Data ###

  # OUTPUT: data table
  output$dataView <- DT::renderDataTable({
    DT::datatable(datasetS(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })

  # OUTPUT: pairs plot
  output$dataplot <- renderPlot({
    if(!is.null(datasetS())){
      if(input$ggpairs || dim(datasetS())[2]==1){
        ggpairs(datasetS())
      }else{
        if(input$run)
          pairs(datasetS(), pch = 19, cex = 0.75,
                col = rainbow( length(unique(mod()@cluster)),alpha = 0.5,
                               start = 0.6, end = 0.10)[mod()@cluster] )
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

  # recommend&select packages
  output$pkgs <- renderUI({
    df <- data.frame(recom = rep("(recommend)",length(cpkgs)),
                     row.names = cpkgs, stringsAsFactors =FALSE)
    if( !is.null(variableType()$Continuous) ){
      df["poLCA",1] <- "(Cut continuous to nominal)"
    }
    if(!is.null(variableType()$Nominal) || !is.null(variableType()$Ordinal) ){
      df["mclust",1] <- "(All as continuous!)"
    }
    pkg <- paste(rownames(df),df$recom)
    choice <- sapply(cpkgs,list)
    names(choice) <- pkg
    recom <-  cpkgs[df[,1]=="(recommend)"]
    checkboxGroupInput("Pkgs", h4("Recommend Packages:"),
                       choices = choice, selected = recom)
  })

  observe({
    toggle(condition = "flexmix" %in% input$Pkgs ,
           selector = "#Clustering li a[data-value=flexmix]")
  })

  observe({
    toggle(condition = "poLCA" %in% input$Pkgs ,
           selector = "#Clustering li a[data-value=poLCA]")
  })

  observe({
    toggle(condition = "mclust" %in% input$Pkgs ,
           selector = "#Clustering li a[data-value=mclust]")
  })

  ### Clustering ###
  # add frequency [space cost; to improve!!]
  dataPre <- eventReactive(input$run, {
    if(input$containFreq == TRUE){
      df <- datasetS()
      df <- df[rep(seq_len(nrow(df)), as.integer(dataset()[,input$freq])),]
    }else
      df <- datasetS()
    df
  })

  #  flexmix Model & Algorithm
  mfclusters <- eventReactive(input$run, {

    # use self-defined order
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

    if(!is.null(input$CovSelected)){
      concomitant =  which(colnames(dataPre()) == input$CovSelected)
    }else{
      concomitant = NULL
    }

    res <- fmclust(dataPre(),
                   concomitant = concomitant,
                   continuous = variableType()$Continuous,
                   ordinal = variableType()$Ordinal,
                   nominal = variableType()$Nominal,
                   count = variableType()$Count_Poisson, order=ord,
                   nClust = input$nCluster[1]:input$nCluster[2],
                   diagonal = TRUE)
    return(res)
  })


  # poLCA Model & Algorithm
  poclusters <- eventReactive(input$run, {

    data = dataPre()                     # data preparation
    if(!is.null(variableType()$Continuous)){
      data[,variableType()$Continuous] <- lapply(data[ , variableType()$Continuous, drop=FALSE],
                                                 function(x) factor(cut(x,5)))
    }

    covariate = covariates()              # cut covariates if continuous
    if(!is.null(covariate)){
      tem <- lapply(covariates(), factor)
      dims <- unlist(lapply(tem, function(x) length(levels(x))))
      for(i in 1:length(dims)){
        if(dims[i]>10){
          covariate[,i] <- factor(cut(covariate[ ,i], 5))
        }
      }
    }

    invisible(capture.output(res <- poLCA.mdls(nClust = input$nCluster[1]:input$nCluster[2],
                                               covariate = covariates(),
                                               data = data, nrep = 5)))
    return(res)
  })


  # #mclust
  # mcclusters <- eventReactive(input$run,{
  #   Mclust(dataPre(), G = input$nCluster[1]:input$nCluster[2])
  # })


  # Clustering Output
  # Step 1: summary of all models
  ## fmclust
  output$indexSelector <- renderUI({      # UI: choose index [fmclust]
    checkboxGroupInput("indexSelected", h5("Summary Display:"),
                       choices = mIndex, selected = dfIndex, inline = T)
  })

  summaryIndex <- reactive({               # summary stuff [fmclust]
    validate(
      need(input$indexSelected > 0, "Please Choose some Index")
    )
    t <- mfclusters()$summary
    t[,input$indexSelected, drop = FALSE]
  })

  output$summary <- DT::renderDataTable({   # Output: summary table [fmclust]
    DT::datatable(summaryIndex(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })

  output$plotSummary <- renderPlot({        # Output: summary plot [fmclust]
    plot(mfclusters()$mdls)
  })


  ## poLCA
  output$poindexSelector <- renderUI({      # UI: choose index [poLCA]
    checkboxGroupInput("poindexSelected", h5("Summary Display:"),
                       choices = poIndex, selected = dfIndex, inline = T)
  })

  posummaryIndex <- reactive({               # summary stuff [poLCA]
    validate(
      need(input$poindexSelected > 0, "Please Choose some Index")
    )
    t <- poclusters()$summary
    t[,input$poindexSelected, drop = FALSE]
  })

  output$posummary <- DT::renderDataTable({   # Output: summary table [poLCA]
    DT::datatable(posummaryIndex(),
                  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  })

  output$poplotSummary <- renderPlot({        # Output: summary plot [fmclust]
    df <- poclusters()$summary[,c("AIC","BIC")]
    minIdx <- apply(df, 2, which.min)
    ylim <- c(min(df), max(df))
    plot(poclusters()$nClust, df[,"AIC"], type = "b", col = "black", ylim = ylim, cex = 0.8,
         xlab = "number of components", ylab = "")
    points(poclusters()$nClust[minIdx["AIC"]], df[minIdx["AIC"],"AIC"],
           pch = 19, col = "black")
    lines(poclusters()$nClust, df[,"BIC"], type ="b", col= "red", pch = 2, cex = 0.7)
    points(poclusters()$nClust[minIdx["BIC"]], df[minIdx["BIC"],"BIC"],
           pch = 19, col = "red")
    legend("topright", legend = c("AIC ", "BIC "), col= c("black", "red"),
           pch = c(1,2), cex = 0.75)
  })

  # Step 2: choose one model and show details
  ## fmclust
  output$nclust <- renderUI({               # UI: choose model by # cluster
    selectInput("Nclust", "Number of cluster",
                choices = as.character(input$nCluster[1]:input$nCluster[2]))
  })

  mod <- reactive({                        # Select model by some index
    if(input$mChoose != "Number of cluster")
      m <- getModel(mfclusters()$mdls, which = input$mChoose)
    else
      m <- getModel(mfclusters()$mdls, which = input$Nclust)
    m
  })

  prof <- reactive({                       # profile
    profile(mod(), mfclusters()$cc)
  })

  output$plot1 <- renderPlotly({          # Output: profile plot
    a <- list(                            ## xaxis style
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

  output$profile <- renderTable({        # Output: profile table
    d <- prof()$dfPro
    if(mfclusters()$cc$con.len >0 && !input$detail){
      exRow <- as.vector(sapply(1:mfclusters()$cc$con.len,
                                function(x) 1 + 2:6 + (x-1) * 6))
      return(d[-exRow,])
    }else
      return(d)
  },include.rownames=FALSE)

  output$probMeans <- renderTable({     # Output: probmean table
    d <- prof()$dfPrM
    if(mfclusters()$cc$con.len >0 && !input$detail1){
      exRow <- as.vector(sapply(1:mfclusters()$cc$con.len,
                                function(x) 1 + 2:6 + (x-1) * 6))
      return(d[-exRow,])
    }else
      return(d)
  },include.rownames=FALSE)

  ## poLCA
  output$ponclust <- renderUI({               # UI: choose model by # cluster
    selectInput("poNclust", "Number of cluster",
                choices = as.character(input$nCluster[1]:input$nCluster[2]))
  })

  poMod <- reactive({                        # Select model by some index
    if(input$pomChoose != "Number of cluster")
      m <- poGetModel(poclusters(), which = input$pomChoose)
    else
      m <- poGetModel(poclusters(), which = input$poNclust)
    m
  })

  output$poplot <- renderPlotly({          # Output: profile plot
    a <- list(                            ## xaxis style
      title = "",
      showticklabels = TRUE,
      tickangle = 20,
      tickfont = list(size = 10, color = "black")
    )

    df <- data.frame('id' = rownames(poMod()$plot), poMod()$plot)
    data_long <- as.data.frame(melt(df, id = "id"), stringsAsFactors = F)

    p <- data_long %>%
      plot_ly(x = id, y = value,  color = variable) %>%
      layout(xaxis = a)
  })

  output$poprofile <- renderTable({        # Output: profile table
    d <- poMod()$profile
  },include.rownames=FALSE)

  output$poprobMeans <- renderTable({     # Output: probmean table
    d <- poMod()$probMeans
  },include.rownames=FALSE)

  # mclust

  mcdata <- reactive({
    as.data.frame(lapply(dataPre(), as.numeric))
  })

  mcmdls <- reactive({
    Mclust(mcdata(), G = input$nCluster[1]:input$nCluster[2])
  })

  mcBIC <- reactive({
    if(input$mcifBIC == "ICL"){
      res <- mclustICL(mcdata(), G = input$nCluster[1]:input$nCluster[2])
    }else{
      res <- mcmdls()$BIC
    }
    res
  })

  # BIC/ICL summary table
  mcBICtable <- reactive({
    x <- mcBIC()
    Glabels <- dimnames(x)[[1]]
    modelNames <- dimnames(x)[[2]]
    x <- matrix(as.vector(x), nrow = length(Glabels), ncol =length(modelNames))
    rownames(x) <- Glabels
    colnames(x) <- modelNames
    x
  })

  output$mcBICsum <- DT::renderDataTable({
    mcBICtable()
  }, options = list(scrollX = TRUE, dom = 't' ))


  # BIC/ICL plot
  output$mcplot <- renderPlot({
    plot( mcBIC())
  })


  # Choose model and cluster
  output$mcModel <- renderUI({
    L <- list()
    tem <- unlist(strsplit(names(summary(mcBIC())[1]),","))
    L[[1]] <- selectInput("mcmodel","Choose Model:", choices = colnames(mcBICtable()),
                          selected = tem[1])
    L[[2]] <- selectInput("mcclust","Choose Clust:", choices = rownames(mcBICtable()),
                          selected = tem[2])
    L
  })

  mcmod <- reactive({
    Mclust(mcdata(), G = input$mcclust, modelNames = input$mcmodel)
  })

  output$mcSum <- renderTable({
    res <- rbind(summary(mcBIC()))
    res
  })

  output$mcprofile <- renderTable({
    rbind( "size"= mcmod()$parameters$pro, mcmod()$parameters$mean)
  })

  output$mcProfilePlot <- renderPlotly({
    a <- list(                            ## xaxis style
      title = "",
      showticklabels = TRUE,
      tickangle = 20,
      tickfont = list(size = 10, color = "black")
    )
    df <- mcmod()$parameters$mean
    dfmax <- apply(mcdata(),2,max)
    dfmin <- apply(mcdata(),2,min)
    df <- data.frame('id' = rownames(df), (df-dfmin)/(dfmax-dfmin))
    data_long <- as.data.frame(melt(df, id = "id"), stringsAsFactors = F)

    p <- data_long %>%
      plot_ly(x = id, y = value,  color = variable) %>%
      layout(xaxis = a)
  })


  output$mcplot1 <- renderPlot({
    plot(mcmod(), what = "classification")
  })



  ### Output ###

  dfClust <- reactive({                   # data frame with cluster
    cbind(dataset(), cIdx = mod()@cluster )
  })

  datasetOutput <- reactive({             # choose output data frame
    switch(input$outputSet,
           "Dataset (with cluster)" = dfClust(),
           "Profile" = prof()$dfPro,
           "ProbMeans"= prof()$dfPrM)
  })

  output$downloadOutput <- downloadHandler(  # downloader
    filename = function() { paste(input$outputSet, '.csv', sep ='')},
    content = function(file){
      write.csv(datasetOutput(), file)
    })


  ### Simulate ###
  output$sizeSelector <- renderUI({       # UI: choose size for each group
    L <- list()
    for(i in 1:ngroup_s()){
      L[[i]] <- column(width = 2, numericInput(paste0('size',i), paste0('group',i),
                                               value = 50, min = 1, step = 1))
    }
    L
  })

  output$cateOrd <- renderUI({            # UI: choose categories for each ordincal variable
    L <- list()
    if(input$defineDT && input$nord > 0){
      L[[1]] <- h4('Categories of each ordinal variables:')
      for(i in 1:input$nord){
        L[[i+1]] <- column(width = 2, numericInput(paste0('cateOrd',i), paste0('Variable',i),value = 3, min = 2, step = 1))
      }
    }
    L
  })

  output$cateNom <- renderUI({            # UI: choose categories for each nominal variable
    L <- list()
    if(input$defineDT && input$nnom > 0){
      L[[1]] <- h4('Categories of each nominal variables:')
      for(i in 1:input$nnom){
        L[[i+1]] <- column(width = 2, numericInput(paste0('cateNom',i), paste0('Variable',i),value = 3, min = 2, step = 1))
      }
    }
    L
  })

  ngroup_s <- reactive({                     # Number of group
    validate(                                ## valid check
      need(!is.na(input$ngroup) && input$ngroup >= 1,
           "Please enter a valid number of groups")
    )
    as.integer(input$ngroup)
  })

  size_s <- reactive({                        # size of each group
    size <- c()
    for(i in 1:ngroup_s()){
      temp <- input[[paste0('size',i)]]
      validate(need(!is.na(temp) && temp >= 1,  "Invalid number of size"))
      size <- c(size, as.integer(temp))
    }
    size
  })

  # simulate parameters
  simupara <- reactive({
    size <- size_s()
    podim = NULL
    ppdim = NULL

    validate(
      need(input$ncont+input$nord+input$nnom+input$ncount > 0,
           "Invalid number of variables")
    )

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

    DF <- simuData(ngroup = ngroup_s(), size = size,
                   cont = input$ncont, ord = input$nord, nom = input$nnom, count = input$ncount,
                   podim = podim, ppdim = ppdim)
    DF$statistics
  })

  # define parameters for each variable (handsontable)
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

  output$hot <- renderRHandsontable({
    if(input$defineDT){
      DF = data1()
      rhandsontable(DF, stretchH = "all", rowHeaderWidth = 100)
    }
  })

  # simulate data
  simudata <- reactive({
    size <- size_s()
    center = NULL; var = NULL
    centerOrd = NULL; varOrd = NULL; podim = NULL
    paraNom = NULL; ppdim = NULL; lambda = NULL
    s <- 1
    validate(
      need(input$ncont+input$nord+input$nnom+input$ncount > 0,
           "Invalid number of variables")
    )

    if(input$defineDT){                 # if defined parameters, use these self-defined parameters

      df <- as.data.frame(t( data1()))
      if(input$ncont>0){
        center <- df[,2:(1+input$ncont), drop = FALSE]; s <- s+input$ncont
        var <- df[,(s+1):(s + input$ncont), drop = FALSE]; s<- s+input$ncont
      }
      if(input$nord>0){
        centerOrd <- df[,(s+1):(s+input$nord),drop = FALSE]; s <- s+input$nord
        varOrd <- df[,(s+1):(s+input$nord),drop = FALSE]; s <- s+input$nord
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
          paraNom[[i]] = df[, (s+1):(s+ppdim[i])]
          s <- s+ppdim[i]
        }
      }
      if(input$ncount >0){
        lambda <- df[,(s+1):(s+input$ncount), drop = FALSE]
      }
    }


    DF <- simuData(ngroup = ngroup_s(), size = size,
                   cont = input$ncont, ord = input$nord, nom = input$nnom, count = input$ncount,
                   center = center, var = var,
                   centerOrd = centerOrd, varOrd = varOrd, podim = podim,
                   paraNom = paraNom, ppdim = ppdim,
                   lambda = lambda)
    DF
  })

  output$simulatedData <- DT::renderDataTable({   # Simulated data table
    df <- simudata()$data
    df[,2:(1+input$ncont)] <- round(df[,2:(1+input$ncont)],2)
    DT::datatable(df,options = list(scrollX = TRUE, dom = 't' ))
  })


  # clustering table
  output$fmctable <- renderTable({
    table(simudata()$data[,1], mod()@cluster)
  })

  output$fmclustTable <- renderUI({              # cluster table
    L <- list()

    if(input$data == 'Simulate Dataset' && !is.na(simudata()$data) && length(mod()@cluster) == length(simudata()$data[,1])){
      L[[1]] <- h3("Clustering Table (flexmix):")
      L[[2]] <- tableOutput("fmctable")
    }
    L
  })

  # poLCA
  output$poctable <- renderTable({
    table(simudata()$data[,1], poMod()$predClust)
  })

  output$poclustTable <- renderUI({              # cluster table
    L <- list()
    if(input$data == 'Simulate Dataset' && !is.na(simudata()$data) && length( poMod()$predClust) == length(simudata()$data[,1])){
      L[[1]] <- h3("Clustering Table (poLCA):")
      L[[2]] <- tableOutput("poctable")
    }
    L
  })

  # mclust
  output$mcctable <- renderTable({
    table(simudata()$data[,1], mcmod()$classification)
  })

  output$mcclustTable <- renderUI({              # cluster table
    L <- list()
    if(input$data == 'Simulate Dataset' && !is.na(simudata()$data) && length(mcmod()$classification) == length(simudata()$data[,1])){
      L[[1]] <- h3("Clustering Table (mclust):")
      L[[2]] <- tableOutput("mcctable")
    }
    L
  })

})

