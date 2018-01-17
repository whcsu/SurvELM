##' Predicting from A Kernel Extreme Learning Machine Ensemble Using the Buckley-James estimator
##' @title SurvELM predict.ELMBJEN
##' @param object  An object that inherits from class ELMBJEN.
##' @param testx  A data frame in which to look for variables with which to predict. 
##' @param trlength  Number of based models used for prediction, shouble be less than and equal to the number for training.
##' @param ... Additional arguments.
##' @return produces a vector of predictions or a matrix of predictions
##' @seealso \code{predict.ELMBJ}
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
##' # with RBF kernel
##' elmsurvmodel=ELMBJEN(x=trset[,-rii],y=Surv(trset[,rii[1]],trset[,rii[2]]),
##' trlength=10,Kernel_type="RBF_kernel",Kernel_para=c(2,1))
##' #The second base model
##' fit2=elmsurvmodel$elmcoxfit[[2]]
##' #The predicted survival
##' testprelin=predict(elmsurvmodel,teset[,-c(rii)])
##' @export
predict.ELMBJEN <-function(object, testx,trlength,...)
  {

 
    elmsurvfit=object$elmsurvfit
    colindexes=object$colindexes
	
	
	
  if(missing(trlength))  trlength=object$trlength
  if (trlength>object$trlength) trlength=object$trlength
  
   # classify the test data
    testpre<-NULL
    for (i in 1:trlength) {
        #if (oobacc[i]<=avroobacc)
        {
           
            # preparing for testing
            testdata=testx[,colindexes[[i]]]
            testdata=as.matrix(testdata)            
            predicts<-predict(elmsurvfit[[i]],testdata)
            
            testpre<-cbind(predicts,testpre)
        }
    }
    
    ensemble_predictions<-rowMeans(testpre)    
    return(ensemble_predictions) 

  
}
