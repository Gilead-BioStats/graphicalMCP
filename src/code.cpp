#include <cpp11.hpp>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
list zero_node_cpp(list graph, int remove) {
  // Convert to 0-indexing
  remove -= 1;

  // extract graph components
  doubles weights = graph["hypotheses"];
  doubles_matrix<> transitions = graph["transitions"];

  int size = weights.size();
  writable::doubles new_weights(size);
  writable::doubles_matrix<> new_transitions(size, size);

  // calculate new weights & transitions
  for (int hyp_num = 0; hyp_num < size; ++hyp_num) {
    if (hyp_num == remove) {
      new_weights[hyp_num] = 0;
    } else {
      new_weights[hyp_num] =
        weights[hyp_num] + weights[remove] * transitions(remove, hyp_num);
    }
    for (int end_num = 0; end_num < size; ++end_num) {
      if (hyp_num == end_num || hyp_num == remove || end_num == remove) {
        new_transitions(hyp_num, end_num) = 0;
      } else {
        double numerator =
          transitions(hyp_num, end_num) +
          transitions(hyp_num, remove) * transitions(remove, end_num);
        double denominator =
          1 - transitions(hyp_num, remove) * transitions(remove, hyp_num);

        new_transitions(hyp_num, end_num) = numerator / denominator;
      }
    }
  }

  return list({
    "hypotheses"_nm = new_weights,
    "transitions"_nm = new_transitions
  });

  // return new_weights;
}
