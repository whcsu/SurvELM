##' Predicting from A Kernel Extreme Learning Machine Using the Buckley-James estimator
##' @title SurvELM ELMBJ
##' @param object  An object that inherits from class ELMBJEN.
##' @param testx  A data frame in which to look for variables with which to predict. 
##' @param ... Additional arguments.
##' @return List of returned values
##'   \tabular{ll}{
##'       \code{trainMSE}    \tab  Mean Square Error(MSE) on training data. \cr
##'       \code{newy} \tab Esitmated survival times of training data by the Buckley-James estimator. \cr
##'       \code{outputWeight} \tab Weights of the output layer in ELM. \cr
##'       \code{testpre} \tab The estimated survival times for \code{testx} data. \cr
##'   }
##' @seealso \code{\link{ELMBJEN}}
##' @author Hong Wang
##' @references
##' \itemize{
##'   \item Hong Wang et al (2017). A Survival Ensemble of Extreme Learning Machine. Applied Intelligence, DOI:10.1007/s10489-017-1063-4.
##'  }
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
##' #A kernel ELM base model
##' kerelmsurv=ELMBJ(trset[,-rii],Surv(trset[,rii[1]],trset[,rii[2]]))
##' testpre=predict(kerelmsurv,teset[,-c(rii)])
##' @export
predict.ELMBJ <- function(object, testx, ...) 
{ 
  Kernel_para = object$Kernel_para
  kerneltype = object$kerneltype
  trainx = object$trainx
  outputWeight = object$outputWeight
  
  H = kernmat(trainx,kerneltype, Kernel_para,testx)
    
  #Calculate the output of testing input   
 
  elmpre = H %*% outputWeight
  
  return(elmpre)
  
}
