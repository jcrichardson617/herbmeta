require("Matrix")
require("corpcor")
require("MASS")

force_Positive_Definiteness <- function(aMatrixOrList, method = c("Matrix", "corpcor")) {
	method <- match.arg(method)
	if(class(aMatrixOrList) == "list") {
		return(lapply(aMatrixOrList, function(x) ifelse(!is.positive.definite(x), return(force_Matrix_into_PosDef(x,method)), return(x))))
	} else {
		ifelse(!is.positive.definite(aMatrixOrList), return(force_Matrix_into_PosDef(aMatrixOrList,method)), return(aMatrixOrList))
	}
}


force_Matrix_into_PosDef <- function(aMatrix, method = c("Matrix", "corpcor")) {
	method <- match.arg(method)
	## determine whether each V is positive definite, if not, then force positive definiteness following Higham (2002)
	## Higham, N. (2002) Computing the nearest correlation matrix - a problem from finance. IMA Journal of Numerical Analysis 22, 329–343. 
	if(method == "Matrix") return(as.matrix(nearPD(aMatrix, keepDiag = TRUE, corr = FALSE)$mat))
	if(method == "corpcor") return(make.positive.definite(aMatrix))
}