##' Predicting from an Ensemble of Regularized Cox  Extreme Learning Machine  Model
##' @title SurvELM predict.ELMCoxEN
##' @param object  An object that inherits from class ELMCoxEN.
##' @param testx  A data frame in which to look for variables with which to predict. 
##' @param enlen  Number of based models used for prediction, shouble be less than and equal to the number for training.
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
predict.ELMCoxEN <-function(object, testx,enlen,...)
  {

    elmcoxfit=object$elmcoxfit
    colindexes=object$colindexes
	
	
	
  if(missing(enlen))  enlen=object$enlen
  if (enlen>object$enlen) enlen=object$enlen
  
   # classify the test data
    testpre<-NULL
    for (i in 1:enlen) {
        #if (oobacc[i]<=avroobacc)
        {
           
            # preparing for testing
            testdata=testx[,colindexes[[i]]]
            testdata=as.matrix(testdata)            
            predicts<-predict(elmcoxfit[[i]],testdata,...)
            
            testpre<-cbind(predicts,testpre)
        }
    }
    
    ensemble_predictions<-rowMeans(testpre)    
    return(ensemble_predictions) 

  
}
