
# install & require packages
pkgs <- c("flexmix", "fpc", "MASS","mvtnorm", "reshape2",
          "shiny","ggplot2","shiny","plotly","xtable", "shinythemes","GGally","rhandsontable")
newPkg <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newPkg))
  install.packages(newPkg, dependencies = TRUE)
sapply(pkgs, require, character.only = TRUE)


# different choices used in server
varChoices  = c("Continuous", "Nominal", "Ordinal", "Count_Poisson", "Count_BiNomial")
mIndex = c("logLik", "AIC", "BIC", "ICL", "Npar", "converged","iter")
dfIndex = c("logLik", "AIC", "BIC", "ICL", "Npar")


#######################
# Function: mcmixed 
# Description: M-step used in flexmix, it could deal with continuous/ordinal/nominal/count data
# Input:
#     formula:  use default when do clustering,
#     continuous/ordinal/nominal/count.poi:  number of continuous/ordinal/nominal/count variables
#     diagonal: TRUE if assume local independence; FALSE set full variance in continuous data
#     pred.ordinal: 
#     printlik:
# Output:
#######################
mcmixed <- function ( formula = .~. , continuous = 0, ordinal = 0, nominal = 0, count.poi =0, ppdim,
                      diagonal = TRUE, pred.ordinal = FALSE, printlik = FALSE)
{
  retval <- new ("FLXMC", weighted = TRUE,
                 formula = formula , dist = "mixed-mode",
                 name = "latent class for normal/multinomial/poisson mixed data")
  
  # check response type (for Poisson)   ### dhouble check !!! ###
  retval@preproc.y <- function(x){
    storage.mode(x) <- "integer"
    x
  }
  
  retval@defineComponent <- expression ({
    
    # loglikelihood
    logLik <- function (x, y) {
      # print("logLik")

      # continuous
      if (continuous!=0){
        out <- dmvnorm (as.matrix(y)[ , 1:continuous, drop = FALSE], 
                        mean = center, sigma = cov, log = TRUE )
      }else{
        out <- 0}
      
      # ordinal
      if(ordinal != 0){
        ###   use cut probability 
        for( k in 1:ordinal){
          d <- as.matrix(y)[ ,continuous+k]
          
          perc.cut <- qnorm(c(0, cumsum(table(d)/nrow(y))), 
                            mean = mean(d), sd =  sqrt(var(d)))  # replicate
          resp.probs <- pnorm(perc.cut[2:length(perc.cut)],
                              mean = centerOrd[k], sd = sqrt(diag(covOrd)[k]))
                      - pnorm(perc.cut[1:(length(perc.cut)-1)],
                              mean = centerOrd[k], sd = sqrt(diag(covOrd)[k]))
          out <- out + log(resp.probs[as.matrix(y)[,continuous+k]])   ###########
      }}
      
      
      # nominal 
      start = continuous + ordinal 
      if (nominal>0){
        for (k in 1:nominal)
          out <- out + log(pp[[k]][y[ , start + k]])
      }
      
      
      # poisson
      start = continuous + ordinal + nominal 
      if(count.poi >0){
          yy <- as.matrix(y)[ ,(start +1):(start + count.poi), drop = FALSE]
          out <- out + colSums(dpois(t(yy), lambda, log = TRUE))
      }
  
      
      if (printlik){
        cat("LogLikelihood= ",sum(out), "\n")
        cat("pp= ",pp[[k]],"\n") }
      out }
    
    
    predict <- function (x) {
      # print("predict")
      
      # continuous
      if (!is.null(center))
        out <- matrix (center , nrow = nrow (x),
                       ncol = length(center), byrow = TRUE )
      else
        out <- matrix(0, ncol=2, nrow=nrow(x))   ## check why ncol =2
      
      # ordinal
      if(!is.null(centerOrd))
        out <- matrix (centerOrd , nrow = nrow (x),
                       ncol = length(centerOrd), byrow = TRUE )
      else
        out <- matrix(0, ncol=2, nrow=nrow(x))
      
      # nominal
      start = continuous + ordinal 
      if (nominal > 0){
        for (k in 1:nominal){
          if (pred.ordinal)
            out[, start + k] <- sum((1:ppdim[k])*pp[[k]])  # if ordinal, use weighted mean ## KEEP?  
          else  
            out[, start + k] <- which.max(pp[[k]])  # else, use mode 
        }}
      
      # poisson
      start = continuous + ordinal + nominal 
      if(count.poi >0){
        out[,( start + 1):(start + count.poi)] <- matrix(lambda, nrow = nrow(x), ncol = length(lambda), byrow = TRUE ) }
      out }
    
    if (continuous == 0){
      center <- NULL
      cov <- NULL
    }
    
    new ("FLXcomponent",
         parameters = list ( center = center , cov = cov ,centerOrd = centerOrd, covOrd = covOrd, pp = pp, lambda = lambda),
         df = df , logLik = logLik , predict = predict )
  })
  
  
  retval@fit <- function (x, y, w) {
    # print("fit")
    n <- nrow(x)
    sw <- sum(w)
    
    # continuous 
    if (continuous!=0)
      para <- cov.wt(as.matrix(y)[, 1:continuous, drop=FALSE], wt = w)[c("center", "cov")]
    else
      para <- list(center=NULL, cov=NULL)
    
    # ordinal
    if(ordinal != 0 )
      para[c("centerOrd", "covOrd")] <- cov.wt(as.matrix(y)[, (continuous + 1): (continuous + ordinal), drop=FALSE], wt = w)[c("center", "cov")]
    else
      para[c("centerOrd", "covOrd")] <- list(centerOrd=NULL,covOrd=NULL)
    
    
    # nominal
    para$pp <- list()
    start = continuous + ordinal 
    if (nominal>0){
      for (k in 1:nominal){
        para$pp[[k]] <- numeric(0) 
        for (l in 1:ppdim[k])
          para$pp[[k]][l] <- sum(w*(as.matrix(y)[, start + k] == l))/sw  ## check!!!
    }}
    
    # poisson
    para$lambda <- list()
    start = continuous + nominal + ordinal 
    if(count.poi>1)
      para$lambda <- colSums(w * (as.matrix(y)[ ,(start +1):(start + count.poi)])/sw)
    else if(count.poi == 1)
      para$lambda <- sum(w * (as.matrix(y)[ ,(start + 1)])/sw)
    
    
    # degree of freedom 
    df <- (3 * continuous + continuous^2)/2
    if (continuous>0){
      if ( diagonal ) {
        if (ncol(para$cov)>1)
          para$cov <- diag ( diag ( para$cov ))   ## better to put it ahead 
          df <- 2 * continuous 
    }}
    if(ordinal >0 )
      df <- df + 2*ordinal # did not consider local dependence 
    if (nominal>0)
      df <- df + sum(ppdim-1)
    if(count.poi>0 )
      df <- df + count.poi
    # print(df)
    with (para , eval ( retval@defineComponent ))
  }
  retval
}


#########################
# Function: data.recode
# Description: Recodes discrete/categorical variables to standard numbering, mainly used in `mcmixed`
# Input: 
#      x: dataframe
#      continuous/ordinal/nominal/count.poi:  
#         vacter indicates the continuous/ordinal/nominal/count.poi variable columns
# Output: a list
#      $data: a numeric matrix
#      $ppdim: dims of each nominal variables
#      $con.name/ord.name/count.name: name of each variables
#      $ord.levels/nom.levels: a list of levels for each ordinal/nominal variables
#      $con.len/ord.len/nom.len/count.len: number of each kinds of variables
#      $ordata: original data after exchange columns 
#########################
data.recode <- function(x, continuous = NULL, ordinal = NULL, nominal = NULL, count = NULL, order = NULL){
  
  x.data <- data.matrix(x)
  x <- x[,c(continuous, ordinal, nominal, count), drop = FALSE]
  x.data <- x.data[,c(continuous, ordinal, nominal, count), drop = FALSE]
  con.len <- length(continuous)
  ord.len <- length(ordinal)
  nom.len <- length(nominal)
  count.len <- length(count)
  
  ###### why need this ?????  # data.matrix translate date to some other value
  x.recode <- matrix(0,ncol=ncol(x.data),nrow=nrow(x.data))
  
  con.name <- c()
  if (con.len>0){
    x.recode[,1:con.len] <- x.data[,1:con.len]
    con.name <- colnames(x)[1:con.len] 
  }
  
  s <- con.len
  ord.name <- c()
  if(ord.len >0){
    x.recode[,(s+1):(s+ord.len)] <- x.data[,(s+1):(s+ord.len)]
    ord.name <- colnames(x)[(s+1):(s+ord.len)]
  }
    
  ord.levels <- list()
  if(ord.len>0){
    for(i in 1:ord.len){
      name <- colnames(x)[con.len+i]
      if(!is.null(order)){
        ord.levels[[name]] <- order[[i]]
      }else{
        ord.levels[[name]] <- levels(as.factor(x[,con.len+i]))
      }
        
      p <- length(ord.levels[[name]])
      for( j in 1:p){
        x.recode[as.factor(x[,con.len +i]) == ord.levels[[name]][j], con.len+i] <- j
      }
       
  }}
  
  s <- s + ord.len
  nom.levels <- list()
  ppdim <- numeric(0)
  if(nom.len>0){
    for(i in 1:nom.len){
      name <- colnames(x)[s+i]
      nom.levels[[name]] <- levels(as.factor(x[,s+i]))
      ppdim[i] <- length(nom.levels[[name]])
      for( j in 1:ppdim[i]) 
        x.recode[as.factor(x[, s+i]) == nom.levels[[name]][j], s+i] <- j
  }}
  
  s <- s + nom.len
  count.name <- c()
  if(count.len >0){
    x.recode[,(s+1):(s+count.len)] <- x.data[,(s+1):(s+count.len)]
    count.name <- colnames(x)[(s+1):(s + count.len)]
  }
  
  out <- list(data = x.recode, ppdim = ppdim, 
              con.name = con.name, ord.name = ord.name, count.name = count.name,
              ord.levels = ord.levels, nom.levels = nom.levels,
              con.len = con.len, ord.len = ord.len, nom.len = nom.len, count.len = count.len,
              ordata = x)
  out
}



##############
# function: profile
# Input:
#       mod: model, flexmix object
#       data: data object of data.recode 
# Output:
#       $dfPlt: data frame used in profile plot (without split of continuous&ordinal)
#       $dfPro: data frame of profile 
#       $dfPrM: data frame of probMean 
###############
profile <- function(mod, data){
  
  para <- parameters(mod)
  newdf <- cbind(data$ordata, cIdx =  mod@cluster)    # data with cluster index
  namep <- c("Cluster Size")               # track names used in profile table
  namel <- c("")
  nameplot <- c()
  dfPro <- data.frame()                    # dfPro used in profile table
  dfPlt <- data.frame()                    # dfPlt used in profile plot
  dfPrM <- data.frame()                    # dfPrM used in probMean 
  srow <- 0                                # track the start row in para
  scol <- 0                                # track the start column
  cSize <-  table(mod@cluster)/length(mod@cluster)
  dfPro <- rbind( dfPro, cSize )           # add cluster size 
  colnames( dfPro) <- colnames(para)
  dfPrM <- dfPro
  
  # continuous
  if(data$con.len >0){
    
    for(i in 1:data$con.len){
      # scaled mean
      minV <- min(data$data[,i])
      maxV <- max(data$data[,i])
      toAdd <- (para[i,] - minV) / (maxV - minV)
      dfPlt <- dfPlt %>% rbind(toAdd)
      dfPro <- dfPro %>% rbind(toAdd)
      dfPrM <- dfPrM %>% rbind(toAdd)
      colnames(dfPlt) <- colnames(para)
      
      # table 
      newdf[,i] <- cut(newdf[,i],5)
      t <- table(newdf[,c(i,ncol(newdf))])
      colnames(t) <- colnames(para)
     # t1 <- as.data.frame.matrix(t/colSums(t))
      t1 <-  as.data.frame.matrix(sweep(t,2,colSums(t),`/`))
      dfPro <- rbind(dfPro, t1)
      t2 <- as.data.frame.matrix(t/rowSums(t))
      dfPrM <- rbind(dfPrM, t2)
      namep <- namep %>% append(c(data$con.name[i], rep("",5)))
      namel <- namel %>% append(c("mean",rownames(t)))
    }
    nameplot <- append(nameplot,data$con.name)
    srow <- srow + data$con.len * (data$con.len + 1) 
    scol <- scol + data$con.len

  }
  
  
  ####################
  ## ordinal data
  ## a little bit confused here. Use mean to plot, but show the probability of each category in the profile
  if( data$ord.len >0){
    
    for(i in 1:data$ord.len){
      # mean
      minV <- min(data$data[,scol+i])  
      maxV <- max(data$data[,scol+i])
      toAdd <- (para[srow+i,] - minV) / (maxV - minV)
      dfPlt <- dfPlt %>% rbind(toAdd)
      nameplot <- nameplot %>% append(c(names(data$ord.levels[i])))
      
      #table
      t <- table(newdf[,c(scol+i,ncol(newdf))])
      colnames(t) <- colnames(para)
      #t1 <- as.data.frame.matrix(t/colSums(t))
      t1 <-  as.data.frame.matrix(sweep(t,2,colSums(t),`/`))
      t2 <- as.data.frame.matrix(t/rowSums(t))
      dfPro <- dfPro %>% rbind(t1)
      dfPrM <- dfPrM %>% rbind(t2)
      namel <- namel %>% append(rownames(t))
      namep <- namep %>% append(c(names(data$ord.levels[i]),
                                  rep("", length(data$ord.levels[[i]])-1)))
    }
    
    srow <- srow + data$ord.len * (data$ord.len +1)
    colnames(dfPlt) <- colnames(para)
  }
  
  # nominal & count
  if( data$nom.len >0 || data$count.len >0 ){
    toAdd <- as.data.frame( para[(srow+1):nrow(para), ])
    colnames(toAdd) <- colnames(para)
    dfPlt <- rbind(dfPlt, toAdd)
    dfPro <- rbind(dfPro, toAdd)
    dfPrM <- dfPrM %>% rbind(toAdd * cSize/ rowSums(toAdd * cSize) )
    if(data$nom.len >0 ){
      for(i in 1:data$nom.len){
        namel <- namel %>% append(data$nom.levels[[i]])
        namep <- namep %>% append(c(names(data$nom.levels[i]), 
                                    rep("", length(data$nom.levels[[i]])-1)))
      }
      nameplot <- nameplot %>% append(apply(melt(data$nom.levels)[,c(2,1)],1,paste,collapse = "-"))
    }
      
    if(data$count.len>0){
      namep <- namep %>% append(data$count.name)
      namel <- namel %>% append(rep("mean", data$count.len))
      nameplot <- nameplot %>% append(data$count.name)
    }
  }
  

  rownames(dfPlt) <- nameplot
  dfPro <- cbind(" " = namep,  "  " = namel, dfPro)
  rownames(dfPro) <-NULL
  dfPrM <- cbind(" " = namep,  "  " = namel, dfPrM)
  rownames(dfPrM) <-NULL
  return(list(dfPlt = dfPlt, dfPro = dfPro,dfPrM = dfPrM ))
}



#################
# step Flexmix Methods
#################

# show: define output summary of stepFlexmix
setMethod("show", "stepFlexmix",
          function(object){
          d <- data.frame(iter = sapply(object@models, function(x) x@iter),
                            converged = sapply(object@models, function(x) x@converged),
                            Npar = sapply(object@models, function(x) x@df),
                            k = sapply(object@models, function(x) x@k),
                            k0 = sapply(object@models, function(x) x@k0),
                            logLik = sapply(object@models, function(x) logLik(x)),
                            AIC = AIC(object),
                            BIC = BIC(object),
                            ICL = ICL(object))
          print(d)
          })



######
# Simutlate data
##################

##########
# Function: simuData
# Input: 
#      ngroup: number of groups
#      size: a vector with size of each group
#      cont/ord/nom/count: number of continuous/ordinal/nominal/count(poisson) variables
#      center: a matrix of means for each continuous variable in each group (ngroup * cont)
#      var: a matrix of variances for each continuous variable in each group (ngroup * cont)
#      centerOrd: a matrix of means for each ordinal variable in each group (ngroup * ord)
#      var: a matrix of variances for each ordinal variable in each group (ngroup * ord)
#      podim: vector of number of categories for each ordinal data
#      paraNom: a list of matrix. In each matrix, element (i,j) indicates the probabilities of each 

simuData <- function(ngroup, size,
                     cont = 0, ord = 0, nom = 0, count = 0,
                     center = NULL, var = NULL, 
                     centerOrd = NULL, varOrd = NULL, podim = NULL,
                     paraNom = NULL, ppdim = NULL,
                     lambda = NULL, seed = 123){
  set.seed(seed)
  
  numCol = cont + ord + nom + count 
  # g <- list()
  # for(i in 1:ngroup){
  #   g[[i]] <- matrix(NA, nrow = size[i], ncol = numCol)
  # }
  
  g <- matrix(NA, nrow = sum(size), ncol = numCol)
  g <- cbind(rep(1:ngroup, size),g)
  colnames(g) <- paste0("V", c("",1:numCol),"_", c( "Cluster", rep("Cont", cont), rep("Ord", ord), rep("Nom",nom), rep("Count", count)))
  stat <- data.frame(size = size )
  sc <-1
  # continuous 
  if(cont > 0){
    
    # center
    if(!is.null(center)){
      if(nrow(center) != ngroup || ncol(center) != cont){
        message("Input center of continous variables is not valid, use default")
        center = matrix(runif(ngroup*cont,5.0,15.0), nrow = cont)
      }else{
        center = center
      }
    }else{
      center = matrix(runif(ngroup*cont,5.0,15.0), nrow = ngroup )
    }
    
    # variance
    #if(!is.null(var) && length(var) == ngroup && all(sapply(var,dim) == c(cont,cont))){
    if(!is.null(var) && nrow(var) == ngroup && ncol(var) == cont){
      var = var
    }else{
      var = matrix(runif(ngroup*cont,1,3), nrow = ngroup )
    }
    
    s <- 0
    for(i in 1:ngroup){
      for(j in 1:cont){
        g[(s+1):(s+size[i]),sc+j] <- rnorm(size[i], mean = center[i,j],sd = var[i,j] )
      }
      s <- s+size[i]
    }
    
    stat <- stat %>% 
      cbind(center = center) %>%
      cbind(var = var)
    
  }
  
  sc = sc +cont 
  ### sample ordinal
  if(ord >0){
    # podim
    if(is.null(podim) || length(podim) != ord)
      podim = sample(2:6, ord, replace = TRUE)
    
    #center, var
    if(!is.null(centerOrd) && nrow(centerOrd) == ngroup && ncol(centerOrd) ==ord){
      centerOrd = centerOrd
    }else{
      centerOrd = matrix(runif(ngroup*ord,5,15), nrow = ngroup )
    }
    
    if(!is.null(varOrd) && nrow(varOrd) == ngroup && ncol(varOrd) == ord){
      varOrd = varOrd
    }else{
      varOrd = matrix(runif(ngroup*ord,0.5,1.1), nrow = ngroup )
    }
    
    for(j in 1:ord){
      s<-0
      for(i in 1:ngroup){
        g[(s+1):(s+size[i]),sc+j]  <- rnorm(size[i], mean = centerOrd[i,j], sd = varOrd[i,j] )
        s<- s+size[i]
      }
      q <- quantile(g[, sc+j], probs = seq(0,1,length.out =  podim[j]+1))[-c(1,podim[j]+1)] + rnorm(podim[j]-1,sd=2)
      g[,sc+j] <- cut(g[, sc+j], breaks = c( -Inf, q, Inf)) 
      
    }
    
    stat  <- stat %>% 
      cbind(centerOrd = centerOrd) %>%
      cbind(varOrd = varOrd)
  }
  
  sc = sc + ord
  
  ## nominal
  if(nom >0){
    # podim
    if(is.null(ppdim) || length(ppdim) != nom)
      ppdim <-  sample(2:6, nom, replace = TRUE)
    
    if(!is.null(paraNom) && length(paraNom) == nom && sapply(paraNom, dim ) == rbind(ngroup, ppdim))
      paraNom <- paraNom
    else{
      for(j in 1:nom){
        r <- matrix(runif(ppdim[j] *ngroup),nrow = ngroup)
        paraNom[[j]] <- r/rowSums(r)
      }
    }
    
    s <- 0
    for(i in 1:ngroup){
      for(j in 1:nom){
        g[(s+1):(s+size[i]), sc+j] <- sample(1:ppdim[j], size[i], prob = paraNom[[j]][i,], replace = TRUE) 
      }
      s <- s + size[i]
    }
    
    for(i in 1:nom){
      stat[[paste0("paraNom_",i)]] = paraNom[[i]]
    }
    
  }
  
  sc = sc + nom
  # count
  if(count >0 ){
    
    if(is.null(lambda) || dim(lambda) != c(ngroup, count))
      lambda <- matrix(sample(1:15, ngroup*count, replace = TRUE) , nrow = ngroup)
    
    s <- 0
    for(i in 1:ngroup){
      for(j in 1:count){
        g[(s+1):(s+size[i]), sc+j] <- rpois(size[i], lambda[i,j])
      }
      s <- s+size[i]
    }
    
    stat <- stat %>% cbind(lambda = lambda)
    
  }
  
  
  
  
  
  return(list(data = g, statistics =stat ))
  
} 



