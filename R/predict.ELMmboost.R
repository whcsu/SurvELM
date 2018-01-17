##' Predicting from an Extreme Learning Machine Cox  Model with Gradient Based Boosting
##' @title SurvELM predict.ELMmboost
##' @param object  An object that inherits from class ELMmboost.
##' @param testx  A data frame in which to look for variables with which to predict. 
##' @param ... Additional arguments for  mboost.
##' @return produces a vector of predictions or a matrix of predictions
##' @seealso \code{\link{mboost}}
##' @author Hong Wang
##' @examples
##' set.seed(123)
##' require(SurvELM)
##' require(survival)
##' #Lung DATA
##' data(lung)
##' lung=na.omit(lung)
##' lung[,3]=lung[,3]-1
##' n=dim(lung)[1]
##' L=sample(1:n,ceiling(n*0.5))
##' trset<-lung[L,]
##' teset<-lung[-L,]
##' rii=c(2,3)
##' elmsurvmodel=ELMmboost(x=trset[,-rii],y=Surv(trset[,rii[1]], trset[,rii[2]]))
##' #THE predicted linear predictor
##' testpre=predict(elmsurvmodel,teset[,-c(rii)])
##' @export
predict.ELMmboost <- function(object, testx,...) {
  Kernel_para = object$Kernel_para
  kerneltype = object$kerneltype
  trainx = object$trainx

  H = kernmat(trainx,kerneltype, Kernel_para,testx)
  #elmcoxpre = survFit(object$mbelm_cox,as.matrix(H))
  elmcoxpre = predict(object$elmglmboost,as.matrix(H),...)
  return(elmcoxpre)
}





