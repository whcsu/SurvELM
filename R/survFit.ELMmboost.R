##' An Extreme Learning Machine Cox  Model with Gradient Based Boosting
##' @title SurvELM SurvFit.ELMmboost
##' @param object  An ELMmboost object.
##' @param newdata  An optional data frame in which to look for variables with which to predict the survivor function.
##' @param ... Additional arguments for  mboost.
##' @return Object of class \code{survFit} with elements
##'   \tabular{ll}{
##'       \code{surv}    \tab  Estimated survival probabilities at the given time points\cr
##'       \code{time} \tab  The evaluated given time points. \cr
##'       \code{n.event} \tab  Number of observed events at given at time points. \cr
##'   }
##' @seealso \code{survFit} 
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
##' sfit=survFit(elmsurvmodel)
##' # plot the survival probability
##' plot(sfit, xlab = "Time", ylab = "Survival Probability")
##' @export
survFit.ELMmboost <- function(object,newdata = NULL, ...) {
if (class(object) == "ELMmboost"){
   survFit(object$elmglmboost, ...)
  }
else{
stop("SurvFit only works for ELMmboost!")
}
}