#' Toys data in multinomial case

"toys.data.multi"
#' @description toys.data.multi is a simple simulated dataset of a multinomial classification problem.
#' @format The format is a list of 2 component.
#' @details
#' \itemize{
#'   \item $Y: output variable: a factor with 3 levels "-1", "0", and "2";
#'   \item $x  A data-frame containing input variables: with 60 obs. of 50 variables.
#' }
#' The data-frame x is composed by 2 independant clusters, each cluster contains 25 correlated variables. It is an equiprobable three class problem, Y belongs to -1,0,1. There is only 6 true variables, that are in the first cluster, the others being noise. The simulation model is defined through the conditional distribution of the X^j for Y=y. In the first cluster, the X^j are simulated in the following way:
#' \itemize{
#'   \item X^j ~ N(2*y,2) for j=1,2,3,4,5,6;
#'   \item the other variables are noise, X^j ~ N(0,1) for j=7,. . . ,25.
#'   }
#' The second cluster of 25 variables contains only noise variables.
#' @docType data
#' @keywords datasets
#' @name toys.data.multi
#' @examples
#' library(ClustOfVar)
#' library(impute)
#' library(FAMT)
#' library(VSURF)
#' library(glmnet)
#' library(anapuce)
#' library(qvalue)
#' X<-toys.data.multi$x
#' Y<-toys.data.multi$Y
#' scoreX<-data.frame(c(rep(8,6),rep(0,44)))
#' rownames(scoreX)<-colnames(X)
#' select<-ARMADA.heatmap(X, Y,  scoreX, threshold=1)
#' \dontrun{
#' result<-ARMADA(X,Y, nclust=2)
#' select<-ARMADA.heatmap(X, Y,  result[[3]], threshold=5)
#' }
NULL
