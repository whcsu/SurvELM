# SurvELM: an R package for high dimensional survival analysis with extreme learning machine
### Introduction
Due to its fast learning speed, simplicity of code implementation and effectiveness in prediction, extreme learning machine(ELM) for single hidden layer feedforward neural networks (SLFNs) has received considerable attentions recently. However, few researchers consider its possible applications in high dimensional survival analysis.  In SurvELM, we present a set of six survival analysis models to model high dimensional right-censored survival data by combining kernel ELMs with the Buckley-James estimator, regularized Cox model, random forests and boosting, namely, ELM with Buckley-James estimator(ELMBJ) and its ensemble (ELMBJEN), ELM with penalized Cox regression(ELMCox) and its ensemble (ELMCoxEN), ELM with graident boosting(ELMmboost) and ELM with likelihood boosting(ELMCoxBoost)
### Installation 
R version >= 3.1 and the latest new Rtools toolchain need to be installed to compile the package. With the "devtools" package, it is easy to install the latest SurvELM R package from Github:
```R
library(devtools)
install_github("whcsu/SurvELM")
```
### An Example
ELMBJ function
## Description
A Kernel Extreme Learning Machine Using the Buckley-James estimator
#### Usage
ELMBJ(x, y, Regularization_coefficient, kerneltype = 2, Kernel_para = c(2,  1))
#### Arguments
x: The covariates(predictor variables) of training data.

y: Survival time and censored status of training data. Must be a Surv survival object.

Regularization_coefficient: Ridge or Tikhonov regularization parameter. Default value for ELMBJEN is 10000. It need be set by the user here when using a single base ELM survival model. Also known as C in the ELM paper.

kerneltype: Type of kernel matrix. kerneltype=1,a RBF kernel;kerneltype=2 , a linear kernel;kerneltype=3 ,a polynomial kernel;kerneltype=4, a sigmoid kernel.

Kernel_para: Parameters for different types of kernels. A single value for kerneltype=1 or 2. A vector for kerneltype=3 or 4.

#### List of returned values
trainMSE:  Mean Square Error(MSE) on training data.

newy:  Esitmated survival times of training data by the Buckley-James estimator.

outputWeight:  Weights of the output layer in ELM.

#### Sample R code

```R
set.seed(123)
require(SurvELM)
require(survival)
#Lung DATAdata(lung)
lung=na.omit(lung)
lung[,3]=lung[,3]-1
n=dim(lung)[1]
# Divide the original dataset into training and test datasets
# In this sample code, 50% of the original data are used as training data 
# while the remaining 50% are used as test data
L=sample(1:n,ceiling(n*0.5))
trset<-lung[L,]
teset<-lung[-L,]
#Specify the indexes of survival times and censoring status
rii=c(2,3)
#A kernel ELM base model
kerelmsurv=ELMBJ(trset[,-rii],Surv(trset[,rii[1]],trset[,rii[2]]))
#The traing MSE
tr_mse=kerelmsurv$trainMSE
#New survival times imputed for training data
y_impute=kerelmsurv$newy
```

### Online Demo
In addition to a traditional R package "SurvELM", we also provide a simple and interactive web-based version using Shiny. The R Package is available at https://github.com/whcsu/SurvELM and its Shiny version is available at 
https://whcsu.shinyapps.io/SurvELM/

### References

Hong Wang and Lifeng Zhou. SurvELM: an R package for high dimensional survival analysis with extreme learning machine. Knowledge-Based Systems, 2018.(Accepted)
