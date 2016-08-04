#' fmclust
#'
#' \code{fmclust} fits a finite mixture clustering model where some variables are continuous and
#' modelled within the mixture components by Gaussian distributions, some variables are nominal
#' and modelled by independent multinomial distributions, and some variables are count and
#' modelled by independent poisson distributions. The fit is by maximum likelihood estimation
#' computed with the EM algorithm, based on function \code{initFlexmix} from package \code{flexmix}
#'
#' @param x input dataframe
#' @param continuous  a vecter indicates the columns of continuous variables.
#' @param ordinal  a vecter indicates the columns of ordinal variables.
#' @param nominal  a vecter indicates the columns of nominal variables.
#' @param count  a vecter indicates the columns of count variables.
#' @param order  a list indicates the order of each ordinal variable.
#' @param nClust a vecter of number of clusters
#' @param diagonal if assume local independence between continuous variable. default TRUE
#' @return an object of class \code{stepFlexmix} containing the best models with respect to the
#' log likelihood for the different number of components in a slot if \code{length(k)>1},
#' else directly an object of class \code{flexmix}.
#'
#' @examples
#' # simulate data
#' gg <- simuData(3, size = c(100,100,100), cont = 3, ord = 2,  nom =3, count=3,
#'      center = center, var = var, centerOrd = centerOrd, varOrd = varOrd, podim = podim,
#'      paraNom = paraNom, ppdim = ppdim, lambda = lambda)
#'
#' # run models
#' mds <- fmclust(gg$data[,-1], continuous = 1:3, ordinal = 4:5, nominal = 6:8, count = 9:11)
#'
#' # get the best model by BIC
#' m <- getModel(mds, which="BIC")
#' m
#'
#' # compare with the real cluster
#' t <- table(gg$data[,1],m@cluster)
#' t
#'
#' @export

fmclust <- function(x, continuous = NULL, ordinal = NULL, nominal = NULL,
                    count = NULL, order = NULL, nClust = 2:5,
                    concomitant = NULL,
                    diagonal = TRUE){

  cc <- data.recode(x, continuous = continuous, ordinal = ordinal, nominal = nominal,
                    count = count, order = order, concomitant = concomitant)


  if(!is.null(concomitant)){
    fc <- as.formula(paste("~", paste(names(x)[concomitant], collapse = "+")))
    concom <- FLXPmultinom(fc)
    data <- data.frame(cc$data, cc$covariates)
  }else{
    concom = NULL
    data <- as.data.frame(cc$data)
  }

  f <- as.formula(paste( "cbind(", paste(colnames(cc$data) ,collapse = ","), ")~1"))

  mdls <- initFlexmix(f, k = nClust,
                      concomitant = concom,
                      data = data,
                      model = mcmixed(continuous = cc$con.len,
                                      ordinal = cc$ord.len,
                                      nominal = cc$nom.len,
                                      count.poi = cc$count.len,
                                      ppdim = cc$ppdim[1:cc$nom.len],
                                      diagonal = diagonal))
  res <- list()
  res$mdls <- mdls
  res$cc <- cc
  res$nClust <- nClust
  res$summary <- fmSummary(mdls)
  return(res)
}


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

