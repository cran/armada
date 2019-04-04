 #' Heatmap of the selected covariates.
#'
#' @param X the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates). X must have rownames, which are the names of the n subjects (i.e. the user ID of the n subjects). X must have colnames, which are the names of the p covariates.
#' @param Y the vector of the response, length n.
#' @param res.ARMADA.summary the result of the function ARMADA, or output of the function ARMADA.summary.
#' @param threshold an integer between 0 and 8: the selected covariates are those which have a score greater or equal to "threshold."
#'
#' @return the plot of the heatmap, and a data.frame of the selected covariables.
#' @details This function plots the heatmap of the covariates which have a score higher than some threshold chosen by the user, with respect to the values of Y.
#' @export
#' @import ComplexHeatmap
#' @import circlize
#'
#' @examples
#' library(ClustOfVar)
#' library(impute)
#' library(FAMT)
#' library(VSURF)
#' library(glmnet)
#' library(anapuce)
#' library(qvalue)
#' library(ComplexHeatmap)
#' library(circlize)
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
#' tracer <- ARMADA.heatmap(X, Y, resultat[[3]], threshold=5)
#' \dontrun{
#' X<-toys.data$x
#' Y<-toys.data$Y
#' result<-ARMADA(X,Y, nclust=2)
#' select<-ARMADA.heatmap(X, Y,  result[[3]], threshold=5)
#' }
ARMADA.heatmap<-function(X, Y,  res.ARMADA.summary, threshold=5){
  indexHeatmap <- (res.ARMADA.summary[,1]>=threshold)
  myMatrix <- X[,indexHeatmap]

  if(is.numeric(Y)){
    ha <- HeatmapAnnotation(df = data.frame(Y = Y),
                            col = list(Y = colorRamp2(c(min(Y), max(Y)), c("white", "blue"))))
  }
  if(is.factor(Y)){
    ha <- HeatmapAnnotation(df = data.frame(Y= Y))
  }
  draw(Heatmap(t(myMatrix), name = "heatmap", top_annotation = ha))

  out=myMatrix
  return(out)
}
