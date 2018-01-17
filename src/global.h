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


// better put template into header files

/**
 * Get indices of sorted values
 * @param values Values to sort
 * @param decreasing Order decreasing
 * @return Indices of sorted values
 */
template<typename T>
std::vector<size_t> order(std::vector<T>& values, bool decreasing) {
  // Create index vector
  std::vector<size_t> indices(values.size());
  std::iota(indices.begin(), indices.end(), 0);
  
  // Sort index vector based on value vector
  if (decreasing) {
    std::sort(std::begin(indices), std::end(indices), [&](size_t i1, size_t i2) {return values[i1] > values[i2];});
  } else {
    std::sort(std::begin(indices), std::end(indices), [&](size_t i1, size_t i2) {return values[i1] < values[i2];});
  }
  return indices;
}
/*A common purpose of using log-domain computations is to increase accuracy and avoid underflow and overflow problems when very small or very large numbers are represented directly (i.e. in a linear domain) using a limited-precision, floating point numbers.
*/

template <typename Iter>
typename  std::iterator_traits <Iter>::value_type log_sum_exp(Iter begin, Iter end)
{
  using VT = typename std::iterator_traits<Iter>::value_type;
  if (begin==end) return VT{};
  using std::exp;
  using std::log;
  auto max_elem = *std::max_element(begin, end);
  auto sum = std::accumulate(begin, end, VT{}, 
     [max_elem](VT a, VT b) { return a + exp(b - max_elem); });
  return max_elem + log(sum);
}

arma::mat mm_mult(NumericMatrix x,NumericMatrix y);
arma::mat mm_transpose(NumericMatrix x,NumericMatrix y);

arma::mat mv_mult(NumericMatrix x,NumericVector y);

Rcpp::NumericMatrix testDFtoNM(DataFrame x);

Rcpp::List scaleNM(NumericMatrix x1);
