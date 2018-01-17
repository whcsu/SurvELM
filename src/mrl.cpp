#include "mrl.h"

using namespace Rcpp;



Rcpp::NumericVector  mrl( NumericVector y, NumericVector cen) {
  //vector for Kaplan-MeierEstimator
  std::vector<double> time=as<std::vector<double> > (y);
  std::vector<int> status=as <std::vector<int> > (cen);
  size_t num=time.size();
  
  // Order by time for calacuate km survival
  std::vector<size_t> indices = order(time, false);
  
  //get sorted time & status 
  std::vector<double> newtime;
  newtime.reserve(num);
  std::vector<double> newstatus;
  newstatus.reserve(num);
  
  for (size_t i = 0; i < num; ++i) {
    newtime.push_back(time[indices[i]]);
    newstatus.push_back(status[indices[i]]);
  }
  
  // Create unique timepoints
  std::set<double> unique_timepoint_set;
  for (size_t i = 0; i < num; ++i) {
    unique_timepoint_set.insert(newtime[i]);
    }
  std::vector<double> unique_timepoints;
  unique_timepoints.reserve(unique_timepoint_set.size());
  for (auto& t : unique_timepoint_set) {
    unique_timepoints.push_back(t);
  }
  
  //get number of unique time points
  size_t num_timepoints = unique_timepoints.size();
  
  Rcpp::NumericVector num_deaths(num_timepoints);
  Rcpp::NumericVector num_samples_at_risk(num_timepoints);
  
  
  // Initialize
  for (size_t i = 0; i < num_timepoints; ++i) {
    num_deaths[i] = 0;
    num_samples_at_risk[i] = 0;
  }
  
 for (size_t i = 0; i < num; ++i){
  
  double survival_time = newtime[i];
  
  size_t t = 0;
  while (t < num_timepoints && (unique_timepoints[t] < survival_time)) {
    ++num_samples_at_risk[t];
    ++t;
  }
  
  // Now t is the survival time, add to at risk and to death if death
  if (t < num_timepoints) {
    ++num_samples_at_risk[t];
    if ( newstatus[i]== 1) {
      ++num_deaths[t];
    }
  }
  } 
  
  
  
  // calculate km alues 
  Rcpp::NumericVector km_temp (num_timepoints);
  
  double status_double = (double) newstatus[0];
  double num_samp_at_risk = (double) num;  
  
  km_temp[0]=1-status_double/num_samp_at_risk;
  
  
  for( size_t i=1; i<num_timepoints; i++){
    status_double = (double) newstatus[i]; 
    km_temp[i]=km_temp[i-1]*((num_samples_at_risk[i]-num_deaths[i])/num_samples_at_risk[i]);
  }
  
   //calacuate integral over time
   Rcpp::NumericVector integ (num_timepoints-1);
   for( size_t i=0; i<num_timepoints-1; i++){
      integ[i] = km_temp[i]*(unique_timepoints[i+1]-unique_timepoints[i]);
   }

  // std::cout<<"numtime="<<num_timepoints<<"\n";
  Rcpp::NumericVector ystar (num);
  // if event, ystar=time, otherwise impute
  for( size_t i=0; i<num; i++)   {
	if(status[i]==1) {
		ystar[i]=time[i];
		}
    else{
	//all timepoints upto time[i], find the index of time[i] in unique_timepoints	
	  size_t timepointID = find(unique_timepoints.begin(), unique_timepoints.end(), time[i]) - unique_timepoints.begin();
      // std::cout<<"id="<<timepointID<<"\n";	  
	  if (timepointID+1==num_timepoints)  {ystar[i] = time[i];}
      else {
		double residual =std::accumulate(integ.begin()+timepointID,integ.end(),0.0) / km_temp[timepointID];
	//	std::cout<<"res="<<residual;
		ystar[i] = time[i] + residual;
		}	
		
	}	
	  
  }

  
  
  /*Rcpp::List ret;
  ret["surv"] = km_temp;
  ret["time"] = unique_timepoints; 
  */
 return (ystar);
}




