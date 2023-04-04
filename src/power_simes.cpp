#include <cpp11.hpp>
#include <algorithm>
#include <iostream>

using namespace cpp11;

[[cpp11::register]]
double p_adjust_simes_cpp(writable::doubles p, writable::doubles weights) {
  int i, j, group_size = weights.size();
  double cur_p, adj_p = std::numeric_limits<double>::infinity();

  writable::doubles adj_p_vec(group_size), new_weights = weights;

  for (i = 0; i < group_size; i++) {
    for (j = 0; j < group_size; j++) {
      if ((p[j] <= p[i]) & (j != i)) {
        new_weights[i] += weights[j];
      }
    }

    adj_p_vec[i] = p[i] / new_weights[i];
    cur_p = adj_p_vec[i];
    adj_p = std::min(adj_p, cur_p);
  }

  return adj_p;
}

[[cpp11::register]]
double p_adjust_simes_ord_simple_cpp(writable::doubles weights, writable::doubles p) {
  int i, j, group_size = weights.size();
  double adj_p = std::numeric_limits<double>::infinity();

  writable::doubles new_weights = weights;

  for (i = 0; i < group_size; i++) {
    for (j = 0; j < i; j++) {
      new_weights[i] += weights[j];
    }

    // adj_p_vec[i] = p[i] / new_weights[i];
    // cur_p = adj_p_vec[i];
    adj_p = std::min(adj_p, p[i] / new_weights[i]);
  }

  return adj_p;
}

[[cpp11::register]]
doubles test_graph_fast_simes_cpp(
  doubles hypotheses,
  doubles transitions,
  doubles p
)
{
  writable::doubles adjusted_p;

  return adjusted_p;
}

// test_graph_fast_simes <- function(graph,
//                                   p,
//                                   alpha = .05,
//                                   groups = list(seq_along(graph$hypotheses)),
//                                   test_types = c("bonferroni"),
//                                   corr = NULL,
//                                   intersections = generate_weights(graph),
//                                   graph_size = length(graph$hypotheses),
//                                   gw_size = 2 ^ graph_size - 1,
//                                   num_groups = length(groups))
