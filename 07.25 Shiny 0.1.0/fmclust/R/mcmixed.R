#' mcmixed 
#' 
#' This is a M-step used to do clustering in flexmix, it could use to cluster mixed-mode (continuous/ordinal/nominal/count) data
#' @param formula use default when do clustering
#' @param continuous number of continuous variables
#' @param ordinal number of ordinal variables
#' @param nominal number of nominal variables
#' @param count.poi number of count variables
#' @param diagonal whether assume local independence. default TRUE (FALSE set full variance in continuous data)
#' @param printlik whether print the loglikelihood
#' @export


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
      
      # continuous
      if (continuous!=0){
        out <- dmvnorm (as.matrix(y)[ , 1:continuous, drop = FALSE], 
                        mean = center, sigma = cov, log = TRUE )
      }else{
        out <- 0}
      
      # ordinal
      if(ordinal != 0){
        ###   use cut probability   ### has problem here!
        for( k in 1:ordinal){
          d <- as.matrix(y)[ ,continuous+k]
          
          perc.cut <- qnorm(c(0, cumsum(table(d)/nrow(y))),
                            mean = mean(d), sd =  sqrt(var(d)))  # replicate
          resp.probs <- pnorm(perc.cut[2:length(perc.cut)],
                              mean = centerOrd[k], sd = sqrt(diag(covOrd)[k]))
          - pnorm(perc.cut[1:(length(perc.cut)-1)],
                  mean = centerOrd[k], sd = sqrt(diag(covOrd)[k]))
          out <- out + log(resp.probs[as.matrix(y)[,continuous+k]])   ###########
        }
        
      }
      
      
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
      
      # continuous
      if (!is.null(center))
        out <- matrix (center, nrow = nrow (x),
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