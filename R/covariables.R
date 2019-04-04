#'concatenation of the rownames of X and of the response vector Y.
#'
#' @param X  the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates). X must have rownames, which are the names of the n subjects (i.e. the user ID of the n subjects).
#' @param Y the vector of the response, length n.
#'
#' @return a data.frame with dimension n*2: the first column gives the names of the subjects, and the second column is Y.
#' @details internal function. Concatenation of the rownames of X (X is the matrix n*p of the covariates), and of the response vector Y. X must have rownames, which are the names of the n subjects (i.e. the user ID of the n subjects).
#' @export


#' @examples
#' X<-matrix(rnorm(50),nrow=10)
#' rownames(X)<-letters[1:10]
#' covariables(X, 1:10)


covariables  <- function(X,Y){
  mat <- cbind(rownames(X),as.data.frame(Y))
  colnames(mat)<-c("ID", "Y")
  return(mat)
}

