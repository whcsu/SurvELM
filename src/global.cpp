#include <vector>
#include <algorithm>
#include <cmath>
#include <iterator>
#include <numeric>
#include <string>
#include <iostream>
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat mm_mult(NumericMatrix x,NumericMatrix y)
{
	arma::mat lhs =as <arma::mat>(x);
	arma::mat rhs =as <arma::mat>(y);
	
  return lhs * rhs;
}

arma::mat mm_transpose(NumericMatrix x,NumericMatrix y)
{
	arma::mat lhs =as <arma::mat>(x);
	arma::mat rhs =as <arma::mat>(y);
	
	
  return lhs * rhs.t();
}

arma::mat mv_mult(NumericMatrix x,NumericVector y)
{
	arma::mat lhs =as <arma::mat>(x);
	arma::vec rhs =as <arma::vec>(y);
	
  return lhs * rhs;
}

//[[Rcpp::export]]
Rcpp::NumericMatrix testDFtoNM(DataFrame x) {
  int nRows=x.nrows();  
  NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=NumericVector(x[i]);
  }  
  return y;
}

//[[Rcpp::export]]
Rcpp::List scaleNM(NumericMatrix x1) {
  NumericMatrix x=clone(x1);
  int nRows=x.nrow();  
  int nCols=x.ncol();
  NumericVector xmean(nCols);
  
  for (int j=0; j<nCols;j++) {
       xmean[j]=mean(x(_,j));
  }  
  
   for (int i=0; i<nRows;i++) 
    for (int j=0; j<nCols;j++) 
	{
       x(i,j)=x(i,j)-xmean(j);
	}  
  
  Rcpp::List ret;
 
  ret["meanx"] = xmean;
  ret["newx"] = x;
  
  return (ret);
}
