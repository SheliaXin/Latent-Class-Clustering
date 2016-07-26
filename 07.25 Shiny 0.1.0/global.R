# install & require packages
pkgs <- c("flexmix", "fpc", "MASS","mvtnorm", "reshape2", "devtools","roxygen2",
          "shiny","ggplot2","shiny","plotly","xtable",
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
dfIndex = c("logLik", "AIC", "BIC", "ICL", "Npar")
ExampleDataset = c("Mixed Normal and Multinormial" = "Cars93")
Index = c("AIC", "BIC", "ICL")