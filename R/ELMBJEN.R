##' A Survival Ensemble of Extreme Learning Machine Using the Buckley-James estimator
##' @title SurvELM ELMBJEN
##' @param x  The covariates(predictor variables) of training data.
##' @param y  Survival time and censored status of training data. Must be a Surv  \code{survival} object
##' @param mtry   The number of covariates(predictor variables) used in each base ELM model. Default is the square root of the number of all avaibable covariates.
##' @param trlength  The ensemle size (the number of base ELM survival models). Default is 100.
##' @param Regularization_coefficient  Ridge or Tikhonov regularization parameter. Default is 10000. Also known as \eqn{C} in the ELM paper.
##' @param Kernel_type Type of kernel matrix. Currently four options avaibable. "RBF_kernel",a RBF kernel;"lin_kernel" , a linear kernel;"poly_kernel" ,a polynomial kernel;"sigmoid_kernel", a sigmoid kernel. Default is "lin_kernel".
##' @param Kernel_para Parameters for different types of kernels. A single value for RBF and linear kernels. A vector for polynomial and sigmoid kernels and progam stops if only a single value is supplied. However, if the vector of values is supplied in the cases of RBF and liner kernels, only the first value will be used. Default is a vector value "c(2,1)"
##' @return Object of class \code{ELMSurvEN} with elements
##'   \tabular{ll}{
##'       \code{elmsurvfit}    \tab  A list of base models \code{ELMBJ} of size \code{trlength}. To retrieve a particular base model: use  elmsurvfit[[i]], where i takes values between 1 and \code{trlength} \cr
##'       \code{colindexes} \tab Covaraite subspace index. \cr
##'       \code{trlength} \tab Number of bases models trained. \cr
##'   }
##' @seealso \code{\link{ELMBJ}}
##' @author Hong Wang
##' @references
##' \itemize{
##'   \item Hong Wang et al (2017). A Survival Ensemble of Extreme Learning Machine. Applied Intelligence, DOI:10.1007/s10489-017-1063-4.
##'  }
##' @examples
##' set.seed(123)
##' require(SurvELM)
##' require(survival)
##' ## Survival Ensemble of ELM  with default settings
##' #Lung DATA
##' data(lung)
##' lung=na.omit(lung)
##' lung[,3]=lung[,3]-1
##' n=dim(lung)[1]
##' L=sample(1:n,ceiling(n*0.5))
##' trset<-lung[L,]
##' teset<-lung[-L,]
##' rii=c(2,3)
##' elmsurvmodel=ELMBJEN(x=trset[,-rii],y=Surv(trset[,rii[1]], trset[,rii[2]]))
##' # Get the 1th base model
##' firstbasemodel=elmsurvmodel$elmsurvfit[[1]]
##' @export
ELMBJEN <-function(x,y,mtry=floor(sqrt(ncol(x))),trlength=100, Regularization_coefficient=10000,
                     Kernel_type="lin_kernel",Kernel_para=c(2,1))
  { 
    
  
  if (!inherits(y, "Surv"))
    stop("Response must be a 'survival' object - use the 'Surv()' function")
  
  ny <- ncol(y)
  n <- nrow(y)
  
  status <- y[, ny]
  survtime=y[, 1L]
  
  if (any(survtime <= 0)) stop("Observation time must be > 0")
  if (all(status == 0)) stop("No deaths in training data set")
  
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
  
    
    rii=c(1,2)
    elmsurvfit <- vector(mode = "list", length = trlength)
    colindexes <- vector(mode = "list", length = trlength)
    
   for(i in 1:trlength){
     
    colindex=sample(c(1:ncol(x)),size=mtry)
	colindexes[[i]]=colindex
    train_x=x[,colindex]
    
    newbagdata=data.frame(y[,1],y[,2],train_x)
    
    trainbag=newbagdata[sample(nrow(newbagdata),replace=T),] 
    
    elmsurvfit[[i]]=ELMBJ(trainbag[,-rii],Surv(trainbag[,1],trainbag[,2]),
                          Regularization_coefficient,kerneltype,Kernel_para)    
   }
    
    fit <- list()
    fit$elmsurvfit=elmsurvfit
	fit$colindexes=colindexes
	fit$trlength=trlength
    
	
    class(fit) <- "ELMBJEN"

    fit
    
    
}

