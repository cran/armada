#' Scores of the covariates X
#'
#' @param X the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates). X must have colnames.
#' @param resultat.ARMADA.select  the output of the ARMADA.select function: a list  with 8 vectors, called: genes_rf_thres, genes_rf_interp, genes_lasso, genes_bonferroni, genes_BH, genes_qvalues, genes_localfdr, genes_FAMT. The 8 vectors are the selected covariates by the corresponding selection methods.
#'
#' @return gene_list: data.frame with p rows and 2 columns, the first column gives the names of the covariates, the second column is the scores of the covariates.
#' @details The function ARMADA.summary gives the scores of all the covariates. The score of a variable is an integer between 0 and 8, and represents the number of selections of this variable by the 8 selection methods.
#'
#' @export

ARMADA.summary <- function(X, resultat.ARMADA.select){
  a<-matrix(0, ncol=1, nrow=ncol(X))
  rownames(a)<-colnames(X)
  for (i in 1:ncol(X))
  {
    for (j in 1:8){
      a[i,] <- a[i,]+rownames(a)[i]%in% (resultat.ARMADA.select[[j]])
    }
  }
  return(a)
}
