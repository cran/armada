#' Covariates selection via 8 selection methods
#'
#' @param X the matrix (or data.frame) of covariates, dimension n*p (n is the sample size, p the number of covariates). X must have rownames and colnames.
#' @param X.decorrele the matrix of decorrelated covariates, dimension n*p (n is the sample size, p the number of covariates). X.decorrele has been obtained by the function X_decor.
#' @param Y the vector of the response, length n.
#' @param test the type of test to apply ("wilox.test" or "t.test" if Y is a binary variable; "kruskal.test" or "anova" if Y is a factor with more than 2 levels; "cor.test" if Y is a continuous variable).
#' @param type.cor.test if test="cor.test", precise the type of test (possible choices: "pearson","kendall", "spearman"). Default value is NULL, which corresponds to "pearson".
#' @param type.measure_glmnet  argument for the lasso regression. The lasso regression is done with the function cv.glmnet (package glmnet), and you can precise the type of data in cv.glmnet. Possible choices for type.measure_glmnet: "deviance" (for gaussian models, logistic, regression and Cox), "class" (for binomial or multinomial regression).
#' @param family_glmnet argument for the lasso regression. The lasso regression is done with the function glmnet. Possible choices for family_glmnet: "gaussian" (if Y is quantitative), "binomial" (if Y is a factor with two levels), "multinomial" (if Y is a factor with more than two levels).
#' @param clusterType to precise the type of cluster of the machine. Possible choices: "PSOCK" or "FORK" (for UNIX or MAC systems, but not for WINDOWS).
#' @param parallel TRUE if the calculus are made in parallel.
#'
#' @details The function ARMADA.select applies 8 selection methods on the decorrelated covariates (named X.decorrele), given the variable of interest Y. It resturns a list of 8 vectors of the selected covariates, each vector correspond to one selection method. The methods are (in the order): Random forest (threshold step), Random forest (interpretation step), Lasso, multiple testing with Bonferroni, multiple testing with Benjamini-Hochberg, multiple testing with qvalues, multiple testing with localfdr, FAMT.

#' @return a list  with 8 vectors, called: genes_rf_thres, genes_rf_interp, genes_lasso, genes_bonferroni, genes_BH, genes_qvalues, genes_localfdr, genes_FAMT. The 8 vectors are the selected covariates by the corresponding selection methods.
#'
#' @export
#'
#' @import VSURF
#' @import glmnet
#' @import FAMT
#' @import impute
#' @import anapuce
#' @import qvalue
#' @import parallel
#' @import doParallel
#' @import stats
ARMADA.select<-function(X,X.decorrele,Y, test, type.cor.test=NULL,type.measure_glmnet=c("deviance","class"), family_glmnet=c("gaussian","binomial", "multinomial"), clusterType=c("PSOCK","FORK"), parallel=c(FALSE,TRUE)){
  covariates<-covariables(X,Y)

  if(is.null(test)){
    if(is.factor(Y)&length(levels(Y))==2){
      if(min(summary(Y))<30){test<-"wilcox.test"}
      else
      {test<-"t.test"}
    }
    if(is.factor(Y)&length(levels(Y))>2){
      if(min(summary(Y))<30){test<-"kruskal.test"}
      else
      {test<-"anova"}
    }
    if(is.numeric(Y)){test<-"cor.test"}
  }
  if(is.null(family_glmnet)){
    if(is.factor(Y)&length(levels(Y))==2){family_glmnet<-"binomial"}
    if(is.factor(Y)&length(levels(Y))>2){family_glmnet<-"multinomial"}
    if(is.numeric(Y)){family_glmnet<-"gaussian"}
  }

  # rf
  vsurf<- VSURF(x = X.decorrele, y = Y, parallel = parallel, clusterType=clusterType)
  genes_rf_thres_index<-vsurf$varselect.thres
  genes_rf_thres<-colnames(X.decorrele)[genes_rf_thres_index]
  genes_rf_interp_index<-vsurf$varselect.interp
  genes_rf_interp<-colnames(X.decorrele)[genes_rf_interp_index]

  # lasso
  lasso_data <- cbind(Y,X.decorrele)
  cvfit <- cv.glmnet(lasso_data[,-1],lasso_data[,1], family = family_glmnet,  type.measure = type.measure_glmnet, nfolds = min(10,length(lasso_data[,1])/3), parallel=parallel)
  if(family_glmnet!="multinomial"){lasso_index<- (coef(cvfit, s = "lambda.min"))@i}
  if(family_glmnet=="multinomial"){nY<-length(levels(Y))
  lasso_index<-NULL
  for (j in 1:nY){lasso_index<-c(lasso_index, (coef(cvfit, s = "lambda.min"))[[j]]@i)}
  lasso_index<-unique(lasso_index)}
  lasso_index<-lasso_index[lasso_index!=0]
  genes_lasso<-colnames(X.decorrele)[lasso_index]

  #pvalues
  Test <- get(test)
  pvalue<-NULL
  if(test=="wilcox.test" | test == "t.test"){
    labels<-levels(Y)
    for (i in 1:ncol(X.decorrele)){
      pvalue[i]<-Test(X.decorrele[which(Y==labels[1]),i],
                      X.decorrele[which(Y==labels[2]),i], paired=FALSE)$p.value
    }
  }
  if(test=="kruskal.test"){
    for (i in 1:ncol(X.decorrele)){pvalue[i]<-Test(X.decorrele[,i]~Y)$p.value}
  }
  if(test=="anova"){
    for (i in 1:ncol(X.decorrele)){pvalue[i]<-Test(lm(X.decorrele[,i]~Y))[1,5]}
  }
  if (test=="cor.test"){
    for (i in 1:ncol(X.decorrele)){
      pvalue[i]<-Test(X.decorrele[,i],Y,method=type.cor.test)$p.value
    }
  }

  # Bonferroni
  p_values_Bonferroni<-p.adjust(pvalue,method = "bonferroni")
  genes_bonferroni<-colnames(X.decorrele)[which(p_values_Bonferroni<=0.05)]

  # BH
  p_values_BH<-p.adjust(pvalue,method = "BH")
  genes_BH<-colnames(X.decorrele)[which(p_values_BH<=0.05)]

  # qvalue
  qobj <- qvalue(p = pvalue)
  q_values<-qobj$qvalues
  genes_qvalues<-colnames(X.decorrele)[which(q_values<=0.05)]

  # local FDR
  dataf<-cbind(1:ncol(X.decorrele),pvalue)
  dataf<-dataf[order(pvalue) ,]
  fdr_local_Aubert<-LocalFDR(dataf = dataf, graph = FALSE)
  fdr_local_Aubert_index<-fdr_local_Aubert[,1][which(fdr_local_Aubert[,8]<=0.05)]
  genes_localfdr<-colnames(X.decorrele)[as.numeric(fdr_local_Aubert_index)]
  invisible(file.remove("LocalFDRFile.txt"))

  #FAMT
  sink("a_jeter.txt")
  FAMT_data <- as.FAMTdata(expression=t(X.decorrele), covariates=as.data.frame(covariates), idcovar=1)
  sink()
  invisible(file.remove("a_jeter.txt"))
  FAMT_model<- modelFAMT(FAMT_data, x=2, test=2, nbf = 0)
  genes_FAMT<-as.character(summaryFAMT(FAMT_model, alpha = 0.05)$DE)

  return(list(genes_rf_thres, genes_rf_interp, genes_lasso, genes_bonferroni, genes_BH, genes_qvalues, genes_localfdr, genes_FAMT))
}
