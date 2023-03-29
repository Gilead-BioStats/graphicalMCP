#include <cpp11.hpp>
#include <algorithm>
#include <iostream>
#include <vector>
using namespace cpp11;

// // Used for indexing into 2d weights/alphas array
// int ind(int row, int col, int nrow){
//   // returns index of a matrix stored
//   return nrow*col+row;
// }
//
// // I think layer in here is which graph you're in when there's multiple graphs?
// // Used for indexing into 3d transitions array
// int aind(int row, int col, int layer, int nrow){
//   // returns index of an array
//   return layer*nrow*nrow + nrow*col+row;
// }

[[cpp11::register]]
doubles temp1(writable::doubles h, int index) {
  writable::doubles h1;

  return h1;
}

[[cpp11::register]]
doubles graphproc_temp2(writable::doubles h, writable::doubles vals) {

  // h.clear();
  int g_size = vals.size();
  // h.reserve(g_size);


  for (int i; i < g_size; i++) {
    // h.push_back(vals[i]);
    h[i] = vals[i] + 0;
    // std::cout << h[i] << '\n';
  }

  return h;
}
