wgt_new_transition <- function(l, k, j, w, g) {
  if (k == l | g[[l, j]] * g[[j, l]] >= 1) {
    0
  } else {
    (g[[l, k]] + g[[l, j]] * g[[j, k]]) /
      (1 - g[[l, j]] * g[[j, l]])
  }
}
