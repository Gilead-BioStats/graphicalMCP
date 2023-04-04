#include <cpp11.hpp>
#include <numeric>
#include <algorithm>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
double p_adjust_simes_ordered_cpp(doubles weights, doubles p) {
  int size = weights.size();
  std::vector<double> y(size), z(size);

  // cumsum weights, store in y
  std::partial_sum(weights.cbegin(), weights.cend(), y.begin());

  // divide p by y, store in z
  std::transform(p.begin(), p.end(), y.begin(), z.begin(), std::divides<double>());

  // std::vector<double>::iterator z_min = ;

  return *std::min_element(z.begin(), z.end());
}

[[cpp11::register]]
double p_adjust_simes_ordered_cpp2(doubles weights, doubles p) {
  int size = weights.size();
  double adj_p = std::numeric_limits<double>::infinity();
  writable::doubles y(size), z(size);

  auto cumsum = cpp11::package("base")["cumsum"];

  // writable::doubles intermediate = as_doubles(cumsum(weights));

  y = as_doubles(cumsum(weights));

  // cumsum weights, store in y
  // for (int j = 0; j < size; j++) {
  //   y[j] = intermediate[j];
  // }


  // divide p by y, store in z
  for (int i = 0; i < size; i++) {
    // z[i] = p[i] / y[i];
    adj_p = std::min(adj_p, p[i] / y[i]);
  }

  // std::vector<double>::iterator z_min = ;

  // return *std::min_element(z.begin(), z.end());
  return adj_p;
}

[[cpp11::register]]
sexp my_cumsum(doubles x, doubles p) {
  return p;
}
