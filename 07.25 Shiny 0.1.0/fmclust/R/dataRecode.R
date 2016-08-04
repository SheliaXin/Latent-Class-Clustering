#' data.recode
#'
#' Prepare data to be used in function \code{\link{mcmixed}}. This function
#' recodes discrete/categorical variables to standard number.
#'
#' @param x input data
#' @param continuous  a vecter indicates the columns of continuous variables.
#' @param ordinal  a vecter indicates the columns of ordinal variables.
#' @param nominal  a vecter indicates the columns of nominal variables.
#' @param count  a vecter indicates the columns of count variables.
#' @param order  a list indicates the order of each ordinal variable.
#' @return a list with components
#'      \item{data}{numeric data matrix with the order of continuous, ordinal,
#'      nominal, count variables}
#'      \item{ppdim}{vector of nominal variable-wise numbers of categories.}
#'      \item{con.name/ord.name/count.name}{name of continuous/ordinal/count variables}
#'      \item{ord.levels/nom.levels}{lists of levels for each ordinal/nominal variables}
#'      \item{con.len/ord.len/nom.len/count.len}{number of each kinds of variables}
#'      \item{ordata}{original data after exchange columns}
#' @export

data.recode <- function(x, continuous = NULL, ordinal = NULL, nominal = NULL,
                        count = NULL, order = NULL, concomitant = NULL){


  if(!is.null(concomitant)){
    covariates <- x[,concomitant, drop = FALSE]
  }else{
    covariates <- NULL
  }
  x.data <- data.matrix(x)
  x <- x[,c(continuous, ordinal, nominal, count), drop = FALSE]
  x.data <- x.data[,c(continuous, ordinal, nominal, count), drop = FALSE]
  con.len <- length(continuous)
  ord.len <- length(ordinal)
  nom.len <- length(nominal)
  count.len <- length(count)

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
      if(!is.null(order) && !is.null(order[[i]])){
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

  colnames(x.recode)  <- colnames(x)

  out <- list(data = x.recode, ppdim = ppdim,
              con.name = con.name, ord.name = ord.name, count.name = count.name,
              ord.levels = ord.levels, nom.levels = nom.levels,
              con.len = con.len, ord.len = ord.len, nom.len = nom.len, count.len = count.len,
              ordata = x, covariates = covariates)
  out
}
