#' Toys data

"toys.data"
#' @description toys.data is a simple simulated dataset of a binary classification problem, introduced by Weston et.al..
#' @format The format is a list of 2 component.
#' @details
#' \itemize{
#'   \item $Y: output variable: a factor with 2 levels "-1" and "1";
#'   \item $x  A data-frame containing input variables: with 30 obs. of 50 variables.
#' }
#' The data-frame x is composed by 2 independant clusters, each cluster contains 25 correlated variables. It is an equiprobable two class problem, Y belongs to -1,1, with 12 true variables (6 true variables in each cluster), the others being noise. The simulation model is defined through the conditional distribution of the X^j for Y=y. In the first cluster, the X^j are simulated in the following way:
#' \itemize{
#'   \item with probability 0.7, X^j ~N(y,2) for j=1,2,3, and X^j ~ N(0,2) for j=4,5,6 ;
#'   \item  with probability 0.3, X^j ~ N(0,2) for j=1,2,3, and X^j ~ N(y(j-3),2) for j=4,5,6 ;
#'   \item the other variables are noise, X^j ~ N(0,1) for j=7,. . . ,25.
#'   }
#' The second cluster of 25 variables is simulated in a similar way.
#' @docType data
#' @keywords datasets
#' @name toys.data
#' @source  Weston, J., Elisseff, A., Schoelkopf, B., Tipping, M. (2003), Use of the zero norm with linear models and Kernel methods, J. Machine Learn. Res. 3, 1439-14611
#' @examples
#' library(ClustOfVar)
#' library(impute)
#' library(FAMT)
#' library(VSURF)
#' library(glmnet)
#' library(anapuce)
#' library(qvalue)
#' X<-toys.data$x
#' Y<-toys.data$Y
#' scoreX<-data.frame(c(rep(8,6),rep(0,19),rep(8,6),rep(0,19)))
#' rownames(scoreX)<-colnames(X)
#' select<-ARMADA.heatmap(X, Y,  scoreX, threshold=1)
#'  \dontrun{
#' result<-ARMADA(X,Y, nclust=2)
#' select<-ARMADA.heatmap(X, Y,  result[[3]], threshold=5)
#' }
NULL
