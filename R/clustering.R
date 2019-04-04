#' To obtain the dendrogram of the covariates contained in the data.frame X, and a proposition for the number of clusters of covariates in X.

#'
#' @param X the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates).
#' @param plot if plot = TRUE" (default value): it gives the dendrogram and the plot of the height versus the number of clusters, for the 30 first clusters.
#'
#' @return a 2-list composed by: "tree" (the dendrogram of X), and "nclust" which is a proposition of the number of clusters. The proposed number of clusters is calculated as following: in the graph of the decreasing height versus the number of clusters, we define variation_height = (height[1:29]-height[2:30])/height[2:30], and our proposition is nclust=min(which(variation_height<0.05)). It is preferable that the user chooses its own number of clusters. Warning: nclust must be not too high. Indeed, if nclust is too high, the clusters contain a small number of covariates, and it is then possible that all the covariates of one or several cluster(s) are included in H0. In that case, the FAMT procedure will have a dysfunction.

#' @export
#' @import ClustOfVar
#' @import "graphics"

#' @examples
#' toys.data
#' X<-toys.data$x
#' clustering(X)
#'
clustering <- function(X,  plot=TRUE){
  tree <- hclustvar(X)
  if(plot==TRUE){
    par(mfcol=c(2,1))
    plot(tree,type="tree")
    plot(rev(tree$height)[1:30], xlab="number of clusters", ylab="Height")
  }
  height <-  rev(tree$height)[1:30]
  variation_height <- (height[1:29]-height[2:30])/height[2:30]
  nclust<-min(which(variation_height<0.05))
  return(list(tree, nclust))
}
