#' profile
#'
#' @param mod object of class \code{flexmix}
#' @param data data obeject of \code{data.recode}
#' @return a list with component
#'    \item{dfPlt}{data frame used in profile plot (without split of continuous&ordinal)}
#'    \item{dfPro}{data frame of profile}
#'    \item{dfPrM}{data frame of probMean}
#' @export

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
      dfPro <- dfPro %>% rbind(para[i,])
      dfPrM <- dfPrM %>% rbind(para[i,])
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
    scol <- scol + data$ord.len
    colnames(dfPlt) <- colnames(para)
  }

  # nominal
  if(data$nom.len >0 ){
    toAdd <- as.data.frame( para[(srow+1):(srow + sum(data$ppdim)), ])
    colnames(toAdd) <- colnames(para)
    dfPlt <- rbind(dfPlt, toAdd)
    dfPro <- rbind(dfPro, toAdd)
    dfPrM <- dfPrM %>% rbind(toAdd * cSize/ rowSums(toAdd * cSize) )

    for(i in 1:data$nom.len){
      namel <- namel %>% append(data$nom.levels[[i]])
      namep <- namep %>% append(c(names(data$nom.levels[i]),
                                  rep("", length(data$nom.levels[[i]])-1)))
    }
    nameplot <- nameplot %>% append(apply(melt(data$nom.levels)[,c(2,1)],1,paste,collapse = "-"))
    srow <- srow + sum(data$ppdim)
    scol <- scol + data$nom.len
  }


  if(data$count.len>0){
    toAdd <- as.data.frame( para[(srow+1):nrow(para), ])
    colnames(toAdd) <- colnames(para)


    minV <- apply(data$data[ , (scol+1):(scol+data$count.len)],2,min)
    maxV <- apply(data$data[ , (scol+1):(scol+data$count.len)],2,max)
    dfPlt <- rbind(dfPlt, t((t(para[(srow+1):nrow(para),]) - minV) / (maxV - minV)))
    dfPro <- rbind(dfPro, toAdd)
    dfPrM <- dfPrM %>% rbind(toAdd * cSize/ rowSums(toAdd * cSize) )


    namep <- namep %>% append(data$count.name)
    namel <- namel %>% append(rep("mean", data$count.len))
    nameplot <- nameplot %>% append(data$count.name)
  }



  rownames(dfPlt) <- nameplot
  dfPro <- cbind(" " = namep,  "  " = namel, dfPro)
  rownames(dfPro) <-NULL
  dfPrM <- cbind(" " = namep,  "  " = namel, dfPrM)
  rownames(dfPrM) <-NULL
  return(list(dfPlt = dfPlt, dfPro = dfPro,dfPrM = dfPrM ))
}

