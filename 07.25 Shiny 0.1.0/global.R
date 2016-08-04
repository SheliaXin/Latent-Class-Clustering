# install & require packages
pkgs <- c("flexmix", "fpc", "MASS","mvtnorm", "reshape2", "devtools","roxygen2",
          "poLCA","mclust","DT",
          "shiny","ggplot2","shinyjs","plotly","xtable",
          "shinythemes","GGally","rhandsontable")
newPkg <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newPkg))
  install.packages(newPkg, dependencies = TRUE)
sapply(pkgs, require, character.only = TRUE)

install("fmclust")
library(fmclust)


# different choices used in server
varChoices  = c("Continuous", "Nominal", "Ordinal", "Count_Poisson")
mIndex = c("logLik", "AIC", "BIC", "ICL", "Npar", "converged","iter")
poIndex = c("logLik", "AIC", "BIC", "Npar", "df","Chisq")
dfIndex = c("logLik", "AIC", "BIC", "Npar")
ExampleDataset = c("Mixed Normal and Multinormial" = "Cars93")
Index = c("AIC", "BIC", "ICL")
cpkgs = c("flexmix","poLCA","mclust")


#########
# reconstruct the ouput of different packages
###########


fmSummary <- function(object){
  d <- data.frame(iter = sapply(object@models, function(x) x@iter),
                  converged = sapply(object@models, function(x) x@converged),
                  Npar = sapply(object@models, function(x) x@df),
                  k = sapply(object@models, function(x) x@k),
                  k0 = sapply(object@models, function(x) x@k0),
                  logLik = sapply(object@models, function(x) logLik(x)),
                  AIC = AIC(object),
                  BIC = BIC(object),
                  ICL = ICL(object))
  return(d)
}

### poLCA
poSummary <- function(obj){
  d <- data.frame(logLik = obj$llik,
                  Npar = obj$npar,
                  df = obj$resid.df,
                  AIC = obj$aic,
                  BIC = obj$bic,
                  Chisq = obj$Chisq)
  return(d)
}




# ! check nClust
poLCA.mdls <- function(nClust, data, covariate = NULL, nrep = 5){

  # formula
  if(!is.null(covariate)){
    f <- as.formula(paste("cbind(", paste(setdiff(names(data),names(covariate)), collapse = ","), ")~",
                          paste(names(covariate), collapse = "+")))
  }else{
    f <- as.formula(paste("cbind(", paste(names(data), collapse = ","), ")~1"))
  }

  # models
  L <- list()
  L$mdls <- list()

  for(i in 1:length(nClust)){
    L$mdls[[i]] <- poLCA(f, data, nclass = nClust[i], nrep)
  }

  L$nClust <- nClust

  # summary
  L$summary <- do.call(rbind.data.frame, lapply(L$mdls, poSummary))
  rownames(L$summary) <- nClust

  return(L)
}

poGetModel <- function(L, which){

  # summary
  nClust <- L$nClust
  s <- do.call(rbind.data.frame, lapply(L$mdls, poSummary))

  # get model
  if(which == "BIC"){
    idx <- which.min(s[,"BIC"])
  }
  if(which == "AIC"){
    idx <- which.min(s[,"AIC"])
  }
  if(which %in% as.character(nClust)){
    idx <- which(which == as.character(nClust))
  }
  mdl <- L$mdls[[idx]]

  # dataframe for plot
  res <- list()
  res$plot <- t(do.call(cbind.data.frame,mdl$probs))

  # profile
  len <- unlist(lapply(mdl$probs,ncol))
  lnames <- rep("",sum(len)+1)
  lnames[1] <- "Cluster Size"
  lnames[cumsum(len)-len+2] <- names(mdl$probs)
  names(mdl$probs) <- NULL
  pro <- t(do.call(cbind.data.frame, mdl$probs ))
  pro <- rbind(mdl$P, pro)
  name <- data.frame(Variable = lnames, Categories = row.names(pro) )
  res$profile <- cbind( name, data.frame( pro, row.names = NULL))

  # probsMeans
  prm <- lapply(mdl$probs, function(y)  apply(y, 2, function(x) x/sum(x)))
  prm <- t(do.call(cbind.data.frame, prm ))
  prm <- rbind(mdl$P, prm)
  res$probMeans <- cbind(name, data.frame( prm, row.names = NULL))

  # predict class
  res$predClust <- mdl$predclass

  res$mdl <- mdl
  return(res)
}


##############

### mclust
mcSummary <- function(obj){
  d <- data.frame(logLik = obj$loglik,
                  Npar = obj$df,
                  BIC = obj$bic)
  d
}

mclust.mdls <- function(data, nClust = 2:5){
  # models
  L <- list()
  L$mdls <- list()

  for(i in 1:length(nClust)){
    L$mdls[[i]] <- Mclust(data, G = nClust[i])
  }

  L$nClust <- nClust
  L$summary <- do.call(rbind.data.frame, lapply(L$mdls, mcSummary))
  rownames(L$summary) <- nClust
  return(L)
}

mcGetModel <- function(L, which){
  # summary
  nClust <- L$nClust
  s <- do.call(rbind.data.frame, lapply(L$mdls, mcSummary))
  # get model
  if(which == "BIC"){
    idx <- which.min(s[,"BIC"])
  }
  if(which %in% as.character(nClust)){
    idx <- which(which == as.character(nClust))
  }
  mdl <- L$mdls[[idx]]

  # dataframe for plot
  res <- list()
  res$plot <- t(do.call(cbind.data.frame, mdl$probs))

  # profile
  len <- unlist(lapply(mdl$probs,ncol))
  lnames <- rep("",sum(len)+1)
  lnames[1] <- "Cluster Size"
  lnames[cumsum(len)-len+2] <- names(mdl$probs)
  names(mdl$probs) <- NULL
  pro <- t(do.call(cbind.data.frame, mdl$probs ))
  pro <- rbind(mdl$P, pro)
  name <- data.frame(Variable = lnames, Categories = row.names(pro) )
  res$profile <- cbind( name, data.frame( pro, row.names = NULL))

  # probsMeans
  prm <- lapply(mdl$probs, function(y)  apply(y, 2, function(x) x/sum(x)))
  prm <- t(do.call(cbind.data.frame, prm ))
  prm <- rbind(mdl$P, prm)
  res$probMeans <- cbind(name, data.frame( prm, row.names = NULL))

  # predict class
  res$predClust <- mdl$predclass

  res$mdl <- mdl
  return(res)
}
