# show: define output summary of stepFlexmix
#' @export
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


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to package `fmclust`")
}
