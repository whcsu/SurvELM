##' Predicting from A Regularized Cox  Extreme Learning Machine Model
##' @title SurvELM predict.ELMCox
##' @param object  An object that inherits from class ELMCox.
##' @param testx  A data frame in which to look for variables with which to predict. 
##' @param ... Additional arguments for  \code{glmnet}.
##' @return produces a vector of predictions or a matrix of predictions
##' @seealso \code{predict.glmnet}
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
##' # Default with lasso penalty
##' elmsurvmodel=ELMCox(x=trset[,-rii],y=Surv(trset[,rii[1]], trset[,rii[2]]))
##' # with ridge penalty and RBF kernel, alpha has the same meaning as in glmnet
##' elmsurvmodel=ELMCox(x=trset[,-rii],y=Surv(trset[,rii[1]], 
##' trset[,rii[2]]),Kernel_type="RBF_kernel",Kernel_para=c(2,1),alpha=0)
##' # with elastic net penalty
##' elmsurvmodel=ELMCox(x=trset[,-rii],y=Surv(trset[,rii[1]], trset[,rii[2]]),alpha=0.5)
##' #The predicted linear predictor
##' testprelin=predict(elmsurvmodel,teset[,-c(rii)],type="link")
##' #The predicted  relative-risk
##' testpreres=predict(elmsurvmodel,teset[,-c(rii)],type="response")
##' @export
predict.ELMCox<- function(object, testx,...) {
  Kernel_para = object$Kernel_para
  kerneltype = object$kerneltype
  trainx = object$trainx

  H = kernmat(trainx,kerneltype, Kernel_para,testx)
  #message(paste("Dimesion of H",dim(H)[1]))
  elmcoxpre = predict(object$elmcox,as.matrix(H),...)

  return(elmcoxpre)
}





