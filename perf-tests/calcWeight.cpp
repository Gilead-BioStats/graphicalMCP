#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List calcWeightCpp(NumericVector w, NumericMatrix g, IntegerVector h) {
  NumericVector wCpp = clone(w);
  NumericMatrix gCpp = clone(g);
  int n_rej = sum(h == 0);
  if (n_rej > 0) {
    IntegerVector zero(wCpp.size());
    IntegerVector loc = seq(0, wCpp.size() - 1);
    IntegerVector rej = loc[h == 0];
    for (int i = 0; i < n_rej; ++i) {
      int ind = rej[i];
      NumericMatrix g1(wCpp.size(), wCpp.size());
      for (int j = 0; j < wCpp.size(); ++j) {
        wCpp[j] = wCpp[j] + wCpp[ind] * gCpp(ind, j);
        NumericVector temp = (gCpp(j, _) + gCpp(j, ind) * gCpp(ind, _)) / (1 - gCpp(j, ind) * gCpp(ind, j));
        temp[j] = 0;
        temp[is_nan(temp)] = 0;
        g1(j, _) = temp;
      }
      wCpp[ind] = 0;
      gCpp = g1;
      gCpp(ind, _) = zero;
      gCpp(_, ind) = zero;
    }
  }
  return List::create(wCpp, gCpp);
}
