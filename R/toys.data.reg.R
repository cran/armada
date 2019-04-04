#' Toys data in regression case

"toys.data.reg"
#' @description toys.data.reg is a simple simulated dataset of a regression problem.
#' @format The format is a list of 2 component.
#' @details
#' \itemize{
#'   \item $Y: output variable;
#'   \item $x  A data-frame containing input variables: with 30 obs. of 50 variables.
#' }
#' The data-frame x is composed by 2 independant clusters, each cluster contains 25 correlated variables. There is only 5 true variables, that are in the first cluster : Y =50*(x[,1]+x[,2]+x[,3]+x[,4]+x[,5]). The other variables are noise.
#' @docType data
#' @keywords datasets
#' @name toys.data.reg
#' @examples
#' library(ClustOfVar)
#' library(impute)
#' library(FAMT)
#' library(VSURF)
#' library(glmnet)
#' library(anapuce)
#' library(qvalue)
#' X<-toys.data.reg$x
#' Y<-toys.data.reg$Y
#' scoreX<-data.frame(c(rep(8,5),rep(0,45)))
#' rownames(scoreX)<-colnames(X)
#' select<-ARMADA.heatmap(X, Y,  scoreX, threshold=1)
#' \dontrun{
#' result<-ARMADA(X,Y, nclust=2)
#' select<-ARMADA.heatmap(X, Y,  result[[3]], threshold=5)
#' }
NULL
