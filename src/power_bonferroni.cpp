#include <cpp11.hpp>
#include <iostream>

using namespace cpp11;

// void print_vec(doubles vec) {
//   int size = vec.size();
//
//   std::cout << "[";
//
//   for (int iter = 0; iter < size; iter++) {
//     std::cout << vec[iter] << ", ";
//   }
//
//   std::cout << "]\n";
// }
//
// void print_mat(doubles_matrix<> mat) {
//   int size = mat.nrow();
//
//   std::cout << "[\n";
//
//   for (int iter1 = 0; iter1 < size; iter1++) {
//     std::cout << "[";
//     for (int iter2 = 0; iter2 < size; iter2++) {
//       std::cout << mat(iter1, iter2) << ", ";
//     }
//     std::cout << "]\n";
//   }
//
//   std::cout << "]\n";
// }

[[cpp11::register]]
writable::integers test_graph_shortcut_cpp_(
  writable::doubles hypotheses,
  writable::doubles_matrix<> transitions,
  doubles p,
  double alpha
)
{
  int rej_num, hyp_num, end_num, reject, hyp_match, cum_rej = 0,
    graph_size = hypotheses.size();
  double numerator, denominator, intermediate;

  writable::integers rejected(graph_size);
  writable::doubles new_hypotheses = hypotheses;

  writable::doubles_matrix<> new_transitions = transitions;

  // init rejected to all 0
  for (int i = 0; i < graph_size; i++) {
    rejected[i] = 0;
  }

  while (1) {

    reject = 0;

    // find a hypothesis that can be rejected
    // reject ends as 0 if none are rejected
    for (rej_num = 0; rej_num < graph_size; rej_num++) {
      // this line has the actual test
      reject = p[rej_num] < hypotheses[rej_num] * alpha;
      // increase count of rejections and break for loop
      if (reject) {
        rejected[rej_num] = 1;
        cum_rej++;
        break;
      }
    }

    // check for no rejections, or all rejected
    if (!reject | (cum_rej == graph_size)) {
      break;
    } else {

      for (hyp_num = 0; hyp_num < graph_size; hyp_num++) {

        if (hyp_num == rej_num) {
          new_hypotheses[hyp_num] = 0;
        } else {
          new_hypotheses[hyp_num] =
            hypotheses[hyp_num] +
            hypotheses[rej_num] * transitions(rej_num, hyp_num);
        }

        for (end_num = 0; end_num < graph_size; end_num++) {

          if (hyp_num == end_num) {
            new_transitions(hyp_num, end_num) = 0;
          } else {
            numerator = transitions(hyp_num, end_num) +
              transitions(hyp_num, rej_num) *
              transitions(rej_num, end_num);

            denominator = 1 -
              transitions(hyp_num, rej_num) * transitions(rej_num, hyp_num);

            hyp_match = (hyp_num == end_num) |
              (hyp_num == rej_num) |
              (end_num == rej_num);

            if ((denominator <= 0) | hyp_match) {
              new_transitions(hyp_num, end_num) = 0;
            } else {
              new_transitions(hyp_num, end_num) = numerator / denominator;
            }
          }
        }
      }

      for (hyp_num = 0; hyp_num < graph_size; hyp_num++) {
        intermediate = new_hypotheses[hyp_num];
        hypotheses[hyp_num] = intermediate;
      }

      for (hyp_num = 0; hyp_num < graph_size * graph_size; hyp_num++) {
        intermediate = new_transitions(hyp_num, 0);
        transitions(hyp_num, 0) = intermediate;
      }

    }

  }

  return rejected;
}

[[cpp11::register]]
integers_matrix<> power_shortcut_cpp(
    writable::doubles hypotheses,
    writable::doubles_matrix<> transitions,
    doubles_matrix<> p_mat,
    double alpha
)
{
  int g_size = hypotheses.size();
  int p_mat_rows = p_mat.nrow();
  int col;
  int row;
  int int_intermediate;
  double dbl_intermediate;

  writable::integers rejected(g_size);
  writable::doubles p(g_size);

  writable::integers_matrix<> rejected_mat(p_mat_rows, g_size);

  for (row = 0; row < p_mat_rows; row++) {

    // get a single row of p-values
    for (col = 0; col < g_size; col++) {
      dbl_intermediate = p_mat(row, col);
      p[col] = dbl_intermediate;
    }

    // test that row
    rejected = test_graph_shortcut_cpp_(hypotheses, transitions, p, alpha);

    // insert results into results matrix
    for (col = 0; col < g_size; col++) {
      int_intermediate = rejected[col];
      rejected_mat(row, col) = int_intermediate;
    }

  }

  return rejected_mat;
}
