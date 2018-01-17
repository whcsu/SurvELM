##' An Extreme Learning Machine Cox  Model with Likelihood Based Boosting
##' @title SurvELM ELMCoxBoost
##' @param x  The covariates(predictor variables) of training data.
##' @param y  Survival time and censored status of training data. Must be a Surv  \code{survival} object
##' @param Kernel_type Type of kernel matrix. Currently four options avaibable. "RBF_kernel",a RBF kernel;"lin_kernel" , a linear kernel;poly_kernel ,a polynomial kernel;sigmoid_kernel, a sigmoid kernel. Default is "lin_kernel".
##' @param Kernel_para Parameters for different types of kernels. A single value for RBF and linear kernels. A vector for polynomial and sigmoid kernels and progam stops if only a single value is supplied. However, if the vector of values is supplied in the cases of RBF and liner kernels, only the first value will be used. Default is a vector value "c(2,1)".
##' @param ... Additional arguments for CoxBoost.
##' @return Object of class \code{ELMCoxBoost} with elements
##'   \tabular{ll}{
##'       \code{elmcoxboost}    \tab  A CoxBoost model. See \code{CoxBoost} for details. \cr
##'       \code{trainx} \tab  Training data covariates. \cr
##'          \code{kerneltype} \tab  Type of kernel matrix used in training. kerneltype=1,a RBF kernel;kerneltype=2 , a linear kernel;kerneltype=3 ,a polynomial kernel;kerneltype=4, a sigmoid kernel. \cr
 ##'   \code{Kernel_para} \tab  Parameters used in training. A single value for kerneltype=1 or 2. A vector for kerneltype=3 or 4. \cr
##'   }
##' @seealso \code{\link{CoxBoost}}
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
ELMCoxBoost <- function(x,y, Kernel_type="lin_kernel",Kernel_para=c(2,1),...) {

  if (!inherits(y, "Surv"))
    stop("Response must be a 'survival' object - use the 'Surv()' function")

  kplen=length(Kernel_para)

  if(Kernel_type=="RBF_kernel"){
    kerneltype=1
    if(kplen==0||kplen<1){
      stop("Error: Kernel Parameter for RBF_kernel Error!")
    }
  }else if(Kernel_type=="lin_kernel"){
    kerneltype=2
    if(kplen==0||kplen<1){
      stop("Error: Kernel Parameter for lin_kernel Error!")
    }
  }  else if(Kernel_type=="poly_kernel"){
    kerneltype=3
    if(kplen==0||kplen<2){
      stop("Error: Kernel Parameter for poly_kernel Error!")
    }
  }  else if(Kernel_type=="sigmoid_kernel"){
    kerneltype=4
    if(kplen==0||kplen<2){
      stop("Error: Kernel Parameter for sigmoid_kernel Error!")
    }
  }else{
    stop("Error:Unknow kernel types!")
  }

  H = kernmat(x,kerneltype, Kernel_para,NULL)
  ystatus <- y[, 2]
  survtime = y[, 1L]
  elmcoxboost = CoxBoost(time=survtime,status=ystatus,as.matrix(H),...)

	fit <- list()
    fit$elmcoxboost=elmcoxboost
	fit$trainx=x
	fit$kerneltype=kerneltype
	fit$Kernel_para=Kernel_para
    class(fit) <- "ELMCoxBoost"
    fit

}





