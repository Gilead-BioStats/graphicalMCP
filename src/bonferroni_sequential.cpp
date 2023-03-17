#include <cpp11.hpp>
#include <algorithm>
#include <iostream>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
std::vector<double> bonferroni_sequential_cpp(std::vector<double> hypotheses,
                                              std::vector<double> transitions,
                                              std::vector<double> p,
                                              double alpha) {
  double adj_p_global_max = 0, numerator, denominator;
  std::vector<double>::iterator adj_p_subgraph_min;
  int min_index = 0, graph_size = hypotheses.size();
  std::vector<double> adj_p_subgraph, adjusted_p(graph_size);
  std::vector<int> rejected(graph_size);

  std::vector<double> new_weights(hypotheses), new_transitions(transitions);

  for (int i = 0; i < graph_size; i++) {
    // divide p by hypotheses and store in adj_p_subgraph
    std::transform(
      p.begin(), p.end(),
      hypotheses.begin(),
      std::back_inserter(adj_p_subgraph),
      std::divides<double>()
    );

    // get pointer to smallest element - bare variable is iterator, * gets value
    adj_p_subgraph_min = std::min_element(
      adj_p_subgraph.begin(),
      adj_p_subgraph.end()
    );

    min_index = std::distance(adj_p_subgraph.begin(), adj_p_subgraph_min);

    // reset adj_p_subgraph, as it's not needed any more this loop
    adj_p_subgraph.clear();

    // update the global max if this sub-graph's min is larger than prior
    adj_p_global_max = std::max(adj_p_global_max, *adj_p_subgraph_min);

    // hypothesis at min_index gets largest adj-p seen so far
    adjusted_p[min_index] = adj_p_global_max;
    rejected[min_index] = (adj_p_global_max <= alpha);

    // delete min_index & update hypotheses & transitions ----------------------
    for (int hyp_num = 0; hyp_num < graph_size; ++hyp_num) {
      if (hyp_num == min_index) {
        new_weights[hyp_num] = 0;
      } else {
        new_weights[hyp_num] =
          hypotheses[hyp_num] +
          hypotheses[min_index] * transitions[hyp_num * graph_size + min_index];
      }
      for (int end_num = 0; end_num < graph_size; ++end_num) {
        if (hyp_num == end_num || hyp_num == min_index || end_num == min_index) {
          new_transitions[end_num * graph_size + hyp_num] = 0;
        } else {
          numerator =
            transitions[end_num * graph_size + hyp_num] +
            transitions[min_index * graph_size + hyp_num] *
            transitions[end_num * graph_size + min_index];
          denominator =
            1 - transitions[min_index * graph_size + hyp_num] *
            transitions[hyp_num * graph_size + min_index];

          new_transitions[end_num * graph_size + hyp_num] =
            numerator / denominator;
        }
      }
    }

    hypotheses = new_weights;
    transitions = new_transitions;
    // update hypotheses & transitions end -------------------------------------
  }

  return adjusted_p;
}

[[cpp11::register]]
std::vector<int> bs_fast(
    std::vector<double> hypotheses,
    std::vector<double> transitions,
    std::vector<double> p,
    double alpha,
    int graph_size
) {
  // init vars -----------------------------------------------------------------
  int reject, hyp_num, end_num, i;
  double numerator, denominator;

  std::vector<int> rejected(graph_size);
  std::vector<double> new_weights;
  std::vector<double> new_transitions;

  // number of rejections unknown ----------------------------------------------
  while (1) {
    // find a rejection --------------------------------------------------------
    for (i = 0; i < graph_size; i++) {
      reject = (p[i] <= hypotheses[i] * alpha);
      // std::cout << i << reject << '\n';
      if (reject) {
        rejected[i * reject] = 1;
        break;
      }
    }

    if (reject) {
      reject *= i;
      // update hypotheses & transitions
      for (hyp_num = 0; hyp_num < graph_size; ++hyp_num) {

        // update hypotheses
        if (hyp_num == reject) {
          new_weights[hyp_num] = 0;
        } else {
          new_weights[hyp_num] =
            hypotheses[hyp_num] +
            hypotheses[reject] * transitions[hyp_num * graph_size + reject];
        }

        // update transitions
        for (end_num = 0; end_num < graph_size; ++end_num) {
          if (hyp_num == end_num || hyp_num == reject || end_num == reject) {
            new_transitions[end_num * graph_size + hyp_num] = 0.0;
          } else {
            numerator =
              transitions[end_num * graph_size + hyp_num] +
              transitions[reject * graph_size + hyp_num] *
              transitions[end_num * graph_size + reject];
            denominator =
              1 - transitions[reject * graph_size + hyp_num] *
              transitions[hyp_num * graph_size + reject];
            new_transitions[end_num * graph_size + hyp_num] =
              numerator / denominator;
          }
        }
      }

      hypotheses = new_weights;
      transitions = new_transitions;
    } else {
      break;
    }
  }

  return rejected;
}
