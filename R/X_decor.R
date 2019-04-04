#' Decorrelation of a matrix X, given a response variable Y.
#'
#' @param X the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates). X must have colnames and rownames.
#' @param Y the vector of the response, length n.
#' @param tree the dendrogram of the covariates (object obtained before by the function clustering). By default, tree=NULL.
#' @param nclust integer, the number of clusters in the covariates (1  by default).
#' @param maxnbfactors integer, the maximum number of factors in the clusters. By default: maxnbfactors=10.
#'
#' @return a matrix X.decorrele, with the same dimension, same rownames and same colnames than X.
#' @details The function X_decor applies the factor analysis method FAMT in the different clusters of variables. The clusters must have been defined before (with the function "clustering").
#' @import FAMT
#' @importFrom ClustOfVar cutreevar
#' @export
#'
#' @examples
#' toys.data
#' X<-toys.data$x
#' Y<-toys.data$Y
#' Tree <- clustering(X,plot=FALSE)
#' nclust <- Tree[[2]]
#' tree <- Tree[[1]]
#' library(ClustOfVar)
#' library(FAMT)
#' X.deco<- X_decor(X, Y, tree, nclust, maxnbfactors=10)
X_decor<-function(X, Y, tree=NULL, nclust=1, maxnbfactors=10){
  covariates<-covariables(X,Y)
  if(nclust!=1){
    P<-cutreevar(tree,nclust)
    cluster <- P$cluster
  }
  if(nclust==1){cluster<-rep(1,ncol(X))}
  X.decorrele<-c()
  for (k in 1:nclust){
    X_clust<-X[,which(cluster==k)]
    sink("a_jeter.txt")
    FAMT_data <- as.FAMTdata(expression=t(X_clust), covariates=as.data.frame(covariates), idcovar=1)

    ## choose number of factors
    nbf<-which.min(nbfactors(FAMT_data, x=2, test=2,  diagnostic.plot = FALSE, maxnbfactors = min(maxnbfactors,dim(t(X_clust))[1]-1))$criterion)
    nb_factors<-as.numeric(nbf)-1
    FAMT_model_manual<- modelFAMT(FAMT_data, x=2, test=2, nbf = nb_factors)
    sink()
    invisible(file.remove("a_jeter.txt"))
    X.decorrele<-cbind(X.decorrele,t(FAMT_model_manual$adjdata$expression))
  }

  order<-NULL
  for(i in 1:nrow(X))
  {
    order[i]<-which((covariates$ID[i]==rownames(X.decorrele))==TRUE)
  }
  X.decorrele<-X.decorrele[order,]

  order<-NULL
  for(i in 1:ncol(X))
  {
    order[i]<-which((colnames(X.decorrele)==colnames(X)[i])==TRUE)
  }
  X.decorrele<-X.decorrele[,order]
  rm(i,order)
  return(X.decorrele)
}
