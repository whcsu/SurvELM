##' An Ensemble of Regularized Cox  Extreme Learning Machine  Model
##' @title SurvELM ELMCoxEN
##' @param x  The covariates(predictor variables) of training data.
##' @param y  Survival time and censored status of training data. Must be a Surv  \code{survival} object
##' @param mtry Number of covariates within the subset tp build a base ELMCox model.
##' @param enlen Number of base models within the ensemble. Default is 100.
##' @param Kernel_type Type of kernel matrix. Currently four options avaibable. "RBF_kernel",a RBF kernel;"lin_kernel" , a linear kernel;poly_kernel ,a polynomial kernel;sigmoid_kernel, a sigmoid kernel. Default is "lin_kernel".
##' @param Kernel_para Parameters for different types of kernels. A single value for RBF and linear kernels. A vector for polynomial and sigmoid kernels and progam stops if only a single value is supplied. However, if the vector of values is supplied in the cases of RBF and liner kernels, only the first value will be used. Default is a vector value "c(2,1)".
##' @param ... Additional arguments for  glmnet.
##' @return Object of class \code{ELMmboost} with elements
##'   \tabular{ll}{
##'       \code{elmcoxfit}    \tab   A list of base \code{ELMCox} models of of size \code{enlen}.To retrieve a particular base model: use  elmcoxfit[[i]], where i takes values between 1 and \code{enlen}\cr
##'       \code{trainx} \tab  Training data covariates. \cr
##'          \code{kerneltype} \tab  Type of kernel matrix used in training. kerneltype=1,a RBF kernel;kerneltype=2 , a linear kernel;kerneltype=3 ,a polynomial kernel;kerneltype=4, a sigmoid kernel. \cr
 ##'   \code{Kernel_para} \tab  Parameters used in training. A single value for kerneltype=1 or 2. A vector for kerneltype=3 or 4. \cr
##'   }
##' @seealso \code{\link{ELMCox}}
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
##' # with ridge penalty and RBF kernel, alpha has the same meaning as in glmnet
##' elmsurvmodel=ELMCoxEN(x=trset[,-rii],y=Surv(trset[,rii[1]],trset[,rii[2]]),
##' enlen=10,Kernel_type="RBF_kernel",Kernel_para=c(2,1),alpha=0)
##' #The second base model
##' fit2=elmsurvmodel$elmcoxfit[[2]]
##' #The predicted linear predictor
##' testprelin=predict(elmsurvmodel,teset[,-c(rii)],type="link")
##' #The predicted  relative-risk
##' testpreres=predict(elmsurvmodel,teset[,-c(rii)],type="response")
##' @export
ELMCoxEN <-function(x,y,mtry=floor(sqrt(ncol(x))),enlen=100,
                     Kernel_type="lin_kernel",Kernel_para=c(2,1),...)
{


  if (!inherits(y, "Surv"))
    stop("Response must be a 'survival' object - use the 'Surv()' function")

  ny <- ncol(y)
  n <- nrow(y)

  status <- y[, ny]
  survtime=y[, 1L]

  if (any(survtime <= 0)) stop("Observation time must be > 0")
  if (all(status == 0)) stop("No deaths in training data set")



  precitedtime<-NULL

    rii=c(1,2)
    elmcoxfit <- vector(mode = "list", length = enlen)
    colindexes <- vector(mode = "list", length = enlen)

   for(i in 1:enlen){

    colindex=sample(c(1:ncol(x)),size=mtry)
    colindexes[[i]]=colindex
    train_x=x[,colindex]

    newbagdata=data.frame(y[,1],y[,2],train_x)

    trainbag=newbagdata[sample(nrow(newbagdata),replace=T),]

    elmcoxfit[[i]] = ELMCox(x=as.matrix(trainbag[,-rii]),y=Surv(trainbag[,1],trainbag[,2]), Kernel_type,Kernel_para,...)

   }

   
   	fit <- list()
    fit$elmcoxfit=elmcoxfit
	fit$colindexes=colindexes
	fit$enlen=enlen
    class(fit) <- "ELMCoxEN"
    fit    
}
