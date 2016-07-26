#' simuData
#'
#' Simulate data and related parameters if not defined.
#'
#' @param ngroup integer indicates the number of groups
#' @param size vector of sizes of each groups
#' @param cont/ord/nom/count number of continuous/ordinal/nominal/count(poisson) variables
#' @param center a matrix of means for each continuous variable in each group (ngroup * cont)
#' @param var a matrix of variances for each continuous variable in each group (ngroup * cont)
#' @param centerOrd a matrix of means for each ordinal variable in each group (ngroup * ord)
#' @param varOrd a matrix of variances for each ordinal variable in each group (ngroup * ord)
#' @param podim vector of number of categories for each ordinal data
#' @param paraNom: a list of matrix. In each matrix, element (i,j) indicates the probabilities of
#'
#' @return a list with component
#' \item{data}{simulated data}
#' \item{statistics}{parameters used in simulation}
#' @export

simuData <- function(ngroup, size,
                     cont = 0, ord = 0, nom = 0, count = 0,
                     center = NULL, var = NULL,
                     centerOrd = NULL, varOrd = NULL, podim = NULL,
                     paraNom = NULL, ppdim = NULL,
                     lambda = NULL, seed = 123){
  set.seed(seed)

  numCol = cont + ord + nom + count
  g <- matrix(NA, nrow = sum(size), ncol = numCol)
  g <- as.data.frame(cbind(rep(1:ngroup, size),g))
  colnames(g) <- paste0("V", c("",1:numCol),"_",
                        c( "Cluster", rep("Cont", cont), rep("Ord", ord),
                           rep("Nom",nom), rep("Count", count)))
  #g <- data.frame(Cluster = rep(1:ngroup, size))
  stat <- data.frame(size = size)
  sc <-1

  # continuous
  if(cont > 0){

    # center
    if(!is.null(center) && nrow(center) == ngroup && ncol(center) == cont){
      center = center
    }else
      center = matrix(runif(ngroup*cont,5.0,15.0), nrow = ngroup)

    # variance
    if(!is.null(var) && nrow(var) == ngroup && ncol(var) == cont){
      var = var
    }else
      var = matrix(runif(ngroup*cont,1,3), nrow = ngroup )

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
  sc = sc + cont

  # Ordinal
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
      q <- quantile(g[, sc+j], probs = seq(0,1, length.out =  podim[j]+1))[-c(1,podim[j]+1)] + rnorm(podim[j]-1,sd=2)
      g[,sc+j] <-as.numeric(cut(g[, sc+j], breaks = c( -Inf, q, Inf)))
    }

    stat <- stat %>%
      cbind(centerOrd = centerOrd) %>%
      cbind(varOrd = varOrd)
  }
  sc = sc + ord

  # nominal
  if(nom >0){
    # podim
    if(is.null(ppdim) || length(ppdim) != nom)
      ppdim <-  sample(2:6, nom, replace = TRUE)

    if(!is.null(paraNom) && length(paraNom) == nom && sapply(paraNom, dim ) == rbind(ngroup, ppdim)){
      paraNom <- paraNom
    }else{
      for(j in 1:nom){
        r <- matrix(runif(ppdim[j] *ngroup),nrow = ngroup)
        paraNom[[j]] <- r/rowSums(r)
      }
    }

    s <- 0
    for(i in 1:ngroup){
      for(j in 1:nom){
        g[(s+1):(s+size[i]), sc+j] <- sample(1:ppdim[j], size[i],
                                             prob = paraNom[[j]][i,], replace = TRUE)
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
        g[(s+1):(s+size[i]),sc+j] <- rpois(size[i], lambda[i,j])
      }
      s <- s+size[i]
    }
    stat <- stat %>% cbind(lambda = lambda)
  }

  g[(cont+2):(cont+ord+nom+1)] <- lapply(g[(cont+2):(cont+ord+nom+1)], as.factor)

  return(list(data = g, statistics = stat))
}
