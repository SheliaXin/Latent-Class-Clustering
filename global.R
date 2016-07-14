
# install & require packages
pkgs <- c("flexmix", "fpc", "MASS","mvtnorm", "reshape2",
          "shiny","ggplot2","shiny","plotly","xtable", "shinythemes","GGally")
newPkg <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newPkg))
  install.packages(newPkg, dependencies = TRUE)
sapply(pkgs, require, character.only = TRUE)


# different choices used in server
varChoices  = c("Continuous", "Nominal", "Ordinal", "Count_Poisson", "Count_BiNomial")
mIndex = c("nCluster","logLik", "AIC", "BIC", "ICL", "converged","iter")
dfIndex = c("nCluster","logLik", "AIC", "BIC", "ICL")

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
    # print("define Component")
    
    # loglikelihood
    logLik <- function (x, y) {
      # print("logLik")
      
      # continuous
      if (continuous!=0){
        out <- dmvnorm (as.matrix(y)[ , 1:continuous, drop = FALSE], 
                        mean = center, sigma = cov, log = TRUE )
        # print("continuous")
      }else{
        out <- 0
      }
      
      
      # ordinal
      if(ordinal != 0){
        for( k in 1:ordinal){
          perc.cut <- qnorm(c(0, cumsum(table(as.matrix(y)[ ,continuous+k])/nrow(y))))  # replicate 
          resp.probs <- pnorm(perc.cut[2:length(perc.cut)], 
                              mean = centerOrd[k], sd = diag(covOrd)[k]) 
          - pnorm(perc.cut[1:(length(perc.cut)-1)], 
                  mean = centerOrd[k], sd = diag(covOrd)[k])
          out <- out + log(resp.probs[as.matrix(y)[,continuous+k]])   ########### 
          # print("ordinal")
        }
      }
      
      # nominal 
      start = continuous + ordinal 
      if (nominal>0){
        for (k in 1:nominal)   
          out <- out + log(pp[[k]][y[ , start + k]])
        # print("nominal")
        # print(pp[[k]])
      }
      
      # poisson
      start = continuous + ordinal + nominal 
      if(count.poi >0){
        if(count.poi >1){
          yy <- as.matrix(y)[ ,(start +1):(start + count.poi)]
          out <- out + colSums(dpois(t(yy), lambda, log = TRUE))
        }else{
          yy <- as.matrix(y)[ ,(start +1)]
          out <- out + sum(dpois(y[ ,(start + 1)], lambda, log = TRUE))
        }
      }
      
      
      if (printlik){
        cat("LogLikelihood= ",sum(out), "\n")
        cat("pp= ",pp[[k]],"\n")
      }
      out
    }
    
    
    predict <- function (x) {
      print("predict")
      
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
        }
      }
      
      # poisson
      start = continuous + ordinal + nominal 
      if(count.poi >0){
        out[,( start + 1):(start + count.poi)] <- matrix(lambda, nrow = nrow(x), ncol = length(lambda), byrow = TRUE )
      }
      out
    }
    
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
    # print(sum(w[y[,3]==1]))
    # print(sw)
    
    # continuous 
    if (continuous!=0){
      para <- cov.wt(as.matrix(y)[, 1:continuous, drop=FALSE], wt = w)[c("center", "cov")]
    }else{
      para <- list(center=NULL, cov=NULL)
    }
    
    # ordinal
    if(ordinal != 0 ){
      para[c("centerOrd", "covOrd")] <- cov.wt(as.matrix(y)[, (continuous + 1): (continuous + ordinal), drop=FALSE], wt = w)[c("center", "cov")]
    }else{
      para[c("centerOrd", "covOrd")] <- list(centerOrd=NULL,covOrd=NULL)
    }
    
    
    # nominal
    para$pp <- list()
    start = continuous + ordinal 
    if (nominal>0){
      for (k in 1:nominal){
        para$pp[[k]] <- numeric(0) 
        #  print(table(as.matrix(y)[,continuous+k]))
        for (l in 1:ppdim[k])
          para$pp[[k]][l] <- sum(w*(as.matrix(y)[, start + k] == l))/sw  ## check!!!
      }
    }
    
    # poisson
    para$lambda <- list()
    start = continuous + nominal + ordinal 
    if(count.poi>1){
      para$lambda <- colSums(w * (as.matrix(y)[ ,(start +1):(start + count.poi)])/sw)
    }else if(count.poi == 1){
      para$lambda <- sum(w * (as.matrix(y)[ ,(start + 1)])/sw)
    }
    
    
    # degree of freedom 
    df <- (3 * continuous + continuous^2)/2
    if (continuous>0){
      if ( diagonal ) {
        if (ncol(para$cov)>1)
          para$cov <- diag ( diag ( para$cov ))   ## better to put it ahead 
        df <- 2 * continuous 
      }
    }
    if(ordinal >0 )
      df <- df + 2*ordinal # did not consider local dependence 
    if (nominal>0)
      df <- df + sum(ppdim-1)
    if(count.poi>0 ){
      df <- df + count.poi
    }
    # print(df)
    with (para , eval ( retval@defineComponent ))
  }
  retval
}


#########################
# Function: data.recode
# Description: Recodes discrete/categorical variables to standard numbering 
# Input: 
#      x: dataframe
#      continuous/ordinal/nominal/count.poi:  
#         vacter indicates the continuous/ordinal/nominal/count.poi variable columns
# Output: a list
#      $data: a numeric matrix
#      $ppdim: dims of each nominal variables
#      
#########################

data.recode <- function(x, continuous = NULL, ordinal =NULL, nominal= NULL, count = NULL){
  
  x.data <- data.matrix(x)
  x <- x[,c(continuous, ordinal, nominal, count)]
  x.data <- x.data[,c(continuous, ordinal, nominal, count)]
  con.len <- length(continuous)
  ord.len <- length(ordinal)
  nom.len <- length(nominal)
  count.len <- length(count)
  
  
  ###### why need this ?????  # data.matrix translate date to some other value
  x.recode <- matrix(0,ncol=ncol(x.data),nrow=nrow(x.data))
  
  con.name <- c()
  if (con.len>0){
    x.recode[,1:con.len] <- x.data[,1:con.len]
    con.name <- colnames(x[,1:con.len])
  }
    
  
  ord.levels <- list()
  if(ord.len>0){
    for(i in 1:ord.len){
      name <- names(x)[con.len+i]
      ord.levels[[name]] <- levels(as.factor(x[,con.len+i]))
      p <- length(ord.levels[[name]])
      print(p)
      for( j in 1:p)
        x.recode[as.factor(x[,con.len +i]) == ord.levels[[name]][j], con.len+i] <- j
    }
  }
  
  s <- con.len + ord.len
  nom.levels <- list()
  ppdim <- numeric(0)
  if(nom.len>0){
    for(i in 1:nom.len){
      name <- names(x)[s+i]
      nom.levels[[name]] <- levels(as.factor(x[,s+i]))
      ppdim[i] <- length(nom.levels[[name]])
      for( j in 1:ppdim[i]) 
        x.recode[as.factor(x[, s+i]) == nom.levels[[name]][j], s+i] <- j
    }
  }
  
  s <- s + nom.len
  count.name <- c()
  if(count.len >0){
    x.recode[,(s+1):(s+count.len)] <- x.data[,(s+1):(s+count.len)]
    count.name <- x[,(s+1):(s+count.len)]
  }
  
  out <- list(data = x.recode, ppdim = ppdim, 
              con.name = con.name, count.name = count.name,
              ord.levels = ord.levels, nom.levels = nom.levels,
              con.len = con.len, ord.len = ord.len, nom.len = nom.len, count.len = count.len)
  out
}



##############
# function: profile
# Input:
#       mod: model, flexmix object
#       data: data object of data.recode 
# Output:

###############


profile <- function(mod, data){
  
  # names the parameters
  namep <- c() 
  df <- data.frame()
  para <- parameters(mod)
  sRow <- 1
  
  if(data$con.len >0){
    
    if(data$con.len == 1){             # scale (0-1) mean
      minV <- min(data$data[,1])
      maxV <- max(data$data[,1])
      df <- df %>% rbind((para[1,] - minV) / (maxV - minV))
    }else{
      minV <- apply(data$data[,1:data$con.len],2,min)
      maxV <- apply(data$data[,1:data$con.len],2,max)
      df <-  df %>% rbind((para[1:data$con.len,] - minV) / (maxV - minV))
    }
    sRow <- sRow + data$con.len * (data$con.len + 1) 
    namep <- namep %>% append(data$con.name)
    # namep <- append(namep, names(datasetS()[,variableType()$Continuous]))
  }
  
  if( data$ord.len >0){
    
    if(data$ord.len == 1){          
      minV <- min(data$data[,sRow])  
      maxV <- max(data$data[,sRow])
      df <- df %>% rbind((para[sRow,] - minV) / (maxV - minV))
    }else{
      minV <- apply(data$data[,sRow:data$ord.len],2,min)
      maxV <- apply(data$data[,sRow:data$ord.len],2,max)
      df <-  df %>% rbind((para[1:data$ord.len,] - minV) / (maxV - minV))
    }
    sRow <- sRow + data$ord.len * (data$ord.len +1)
    namep <- namep %>% append(apply(melt(cc$ord.levels)[,c(2,1)],1,paste,collapse = "-"))
    # nameDis <- melt(lapply(datasetS(),levels)[variableType()$Ordinal])[,c(2,1)]
    # namep <- append(namep, apply(nameDis, 1, paste, collapse = "-"))
    
  }
  
  if( data$nom.len >0 || data$count.len >0 ){
    df <- rbind(df, para[sRow:nrow(para), ])
    if(data$nom.len >0 )
      namep <- namep %>% append(apply(melt(cc$nom.levels)[,c(2,1)],1,paste,collapse = "-"))
    if(data$count.len>0)
      namep <- namep %>% append(count.name)
    # nameDis <- melt(lapply(datasetS(),levels)[variableType()$Nominal])[,c(2,1)]
    # namep <- append(namep, apply(nameDis, 1, paste, collapse = "-"))
  }
  
  rownames(df) <- namep
  return(df)

  # datap <- cbind("id" = namep, datap)
  
}


