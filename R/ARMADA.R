#' Scores of all the covariates present in X, given the vector Y of the response.
#'
#' @param X the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates). X must have rownames, which are the names of the n subjects (i.e. the user ID of the n subjects). X must have colnames, which are the names of the p covariates.
#' @param Y the vector of the response, length n.
#' @param nclust the number of clusters in the covariates dataset X.
#' @param clusterType to precise the type of cluster of the machine. Possible choices: "PSOCK", or "FORK" (for UNIX or MAC systems, but not for WINDOWS).
#' @param parallel =  TRUE if the calculus are made in parallel (default choice is FALSE).
#'
#' @return a 3-list with: "tree" which is the dendrogram of the data X, "nclust" which is a proposition of the number of clusters in the data X, "result" which is a data.frame with p rows and 2 columns, the first column gives the names of the covariates, the second column is the scores of the covariates.
#' @export
#' @import ClustOfVar
#' @import impute
#' @import FAMT
#' @import VSURF
#' @import glmnet
#' @import anapuce
#' @import qvalue
#' @import parallel
#' @import doParallel
#'
#' @examples
#' library(ClustOfVar)
#' library(impute)
#' library(FAMT)
#' library(VSURF)
#' library(glmnet)
#' library(anapuce)
#' library(qvalue)
#'
#' set.seed(1)
#' p <- 40
#' n <- 30
#' indexRow <- paste0("patient",1:n)
#' indexCol <- paste0("G",1:p)
#' X <- matrix(rnorm(p*n),ncol=p)
#' colnames(X) <- indexCol
#' rownames(X) <- indexRow
#' Y <- c(rep(-1,n/2), rep(1,n/2))
#' X[,1:4] <-  X[,1:4] + matrix(rnorm(n*4, mean=2*Y, sd=1), ncol=4)
#' Y<-as.factor(Y)
#' resultat <- ARMADA(X,Y, nclust=1)
#' \dontrun{
#' X<-toys.data$x
#' Y<-toys.data$Y
#' result<-ARMADA(X,Y, nclust=2)
#' }
ARMADA<-function(X,Y, nclust=NULL, clusterType=c("PSOCK","FORK"), parallel=FALSE){
  if(nclust!=1){
    #Research  of the clusters and of the number of clusters
    Tree <- clustering(X,plot=FALSE)
    if(is.null(nclust)){nclust <- Tree[[2]]}
    tree <- Tree[[1]]
    #P<-cutreevar(tree,nclust)
    #cluster <- P$cluster
    #table(cluster)

    #Decorrelation inside the clusters
    X.decorrele <- X_decor(X, Y, tree, nclust, maxnbfactors = 10)
  }
  if(nclust==1){
    tree<-NULL
    X.decorrele<- X_decor(X, Y, tree, nclust=1, maxnbfactors = 10)
    }

  #Application of the 8 selection methods
  if(is.factor(Y)&length(levels(Y))==2){
    if(min(summary(Y))<30){test <- "wilcox.test"}
    else
    {test <- "t.test"}
    type.measure_glmnet <- "class"
    family_glmnet <- "binomial"
  }
  if(is.factor(Y)&length(levels(Y))>2){
    if(min(summary(Y))<30){test<- "kruskal.test"}
    else
    {test <- "anova"}
    type.measure_glmnet <- "class"
    family_glmnet <- "multinomial"
  }
  if(is.numeric(Y)){test <- "cor.test";  type.measure_glmnet <- "deviance"; family_glmnet <- "gaussian"}

  res <- ARMADA.select(X, X.decorrele, Y, test=test, type.cor.test=NULL,type.measure_glmnet=type.measure_glmnet, family_glmnet=family_glmnet, clusterType = clusterType, parallel=parallel )

  #Results
  result <- ARMADA.summary(X,res)
  return(list(tree,nclust, result))
}
