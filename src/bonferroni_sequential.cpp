#include <cpp11.hpp>
#include <vector>
#include <functional> // std::divides
#include <algorithm> // std::transform
#include <iostream>
using namespace cpp11;

[[cpp11::register]]
list bonferroni_sequential_cpp(list graph, doubles p, double alpha) {
  writable::list new_graph(graph);

  writable::doubles hypotheses = graph["hypotheses"];
  writable::doubles_matrix<> transitions = graph["transitions"];

  double adj_p_global_max = 0;
  double adj_p_subgraph_min;
  int min_index = 0;
  int graph_size = hypotheses.size();
  // writable::doubles adj_p_subgraph[graph_size];
  writable::doubles adjusted_p(p);
  // writable::logicals rejected[graph_size];

  for (int i = 0; i < graph_size; i++) {

    // initialize subgraph p at 1
    adj_p_subgraph_min = 1.0;

    // loop through the graph, replacing the min each time a smaller is found
    // also store the index
    for (int j = 0; j < graph_size; j++) {
      if ((hypotheses[j] > 0) & ((p[j] / hypotheses[j]) < adj_p_subgraph_min)) {
        min_index = j;
        adj_p_subgraph_min = p[j] / hypotheses[j];
      }
    }

    // update the global max if this sub-graph's min is larger than prior
    if (adj_p_subgraph_min > adj_p_global_max) {
      adj_p_global_max = adj_p_subgraph_min;
    }

    // hypothesis @ min_index gets largest adj-p seen so far
    adjusted_p[min_index] = adj_p_global_max;
    // rejected[min_index] = (adj_p_global_max <= alpha);
    // std::cout << i << " | " << adj_p_subgraph_min << '\n';

    // update hypotheses & transitions -----------------------------------------

    // init storage for new graph elts
    writable::doubles new_weights(graph_size);
    writable::doubles_matrix<> new_transitions(graph_size, graph_size);

    // calculate new weights & transitions
    for (int hyp_num = 0; hyp_num < graph_size; ++hyp_num) {
      if (hyp_num == min_index) {
        new_weights[hyp_num] = 0;
      } else {
        new_weights[hyp_num] =
          hypotheses[hyp_num] + hypotheses[min_index] * transitions(min_index, hyp_num);
      }
      for (int end_num = 0; end_num < graph_size; ++end_num) {
        if (hyp_num == end_num || hyp_num == min_index || end_num == min_index) {
          new_transitions(hyp_num, end_num) = 0;
        } else {
          double numerator =
            transitions(hyp_num, end_num) +
            transitions(hyp_num, min_index) * transitions(min_index, end_num);
          double denominator =
            1 - transitions(hyp_num, min_index) * transitions(min_index, hyp_num);

          new_transitions(hyp_num, end_num) = numerator / denominator;
        }
      }
    }

    hypotheses = new_weights;
    writable::doubles_matrix<> transitions = new_transitions;

    // update graph end --------------------------------------------------------
  }

  return list({
    "p_adj"_nm = adjusted_p,
    "rejected"_nm = transitions
  });

  // return new_weights;
}
