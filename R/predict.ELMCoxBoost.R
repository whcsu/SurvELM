##' Predicting from An Extreme Learning Machine Cox  Model with Likelihood Based Boosting
##' @title SurvELM predict.ELMCoxBoost
##' @param object  An object that inherits from class ELMCoxBoost.
##' @param testx  A data frame in which to look for variables with which to predict. 
##' @param ... Additional arguments for  \code{CoxBoost}.
##' @return produces a vector of predictions or a matrix of predictions
##' @seealso \code{predict.CoxBoost}
##' @author Hong Wang
##' @examples
##' set.seed(123)
##' library(SurvELM)
##' library(survival)
##' #Lung DATA
##' data(lung)
##' lung=na.omit(lung)
##' lung[,3]=lung[,3]-1
##' n=dim(lung)[1]
##' L=sample(1:n,ceiling(n*0.5))
##' trset<-lung[L,]
##' teset<-lung[-L,]
##' rii=c(2,3)
##' elmsurvmodel=ELMCoxBoost(x=trset[,-rii],y=Surv(trset[,rii[1]], trset[,rii[2]]))
##' #THE predicted linear predictor
##' testpre=predict(elmsurvmodel,teset[,-c(rii)])
##' #The predicted cumulative incidence function
##' testprecif=predict(elmsurvmodel,teset[,-c(rii)],type="CIF")
##' # The predicted partial log-likelihood
##' testprellk=predict(elmsurvmodel,teset[,-c(rii)],newtime=teset[,rii[1]],
##' newstatus=teset[,rii[2]],type="logplik")
##' uniquetimes=sort(unique(trset$time))
##' # The predicted probability of not yet having had the event at the time points given in times
##' testprerisk=predict(elmsurvmodel,teset[,-c(rii)],times=uniquetimes,type="risk")
##' @export
predict.ELMCoxBoost<- function(object, testx,...) {
  Kernel_para = object$Kernel_para
  kerneltype = object$kerneltype
  trainx = object$trainx
  H = kernmat(trainx,kerneltype, Kernel_para,testx)
  #elmcoxpre = survFit(mbelmcox$mbelm_cox,as.matrix(H))
  elmcoxpre = predict(object$elmcoxboost,as.matrix(H),...)
  
  return(elmcoxpre)
}





