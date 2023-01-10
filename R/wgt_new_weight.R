wgt_new_weight <- function(l, j, w, g) {
  if (l == j) {
    0
  } else {
    w[[l]] + w[[j]] * g[[j, l]]
  }
}
