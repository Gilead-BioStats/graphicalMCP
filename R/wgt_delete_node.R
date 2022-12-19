wgt_delete_node <- function(j, w, g, I) {
	w_updated <- w
	g_updated <- g

	for (l in I) {
		# if (l == 4) browser()
		w_updated[[l]] <- wgt_new_weight(l, j, w, g)
		for (k in I) {
			g_updated[[l, k]] <- wgt_new_transition(l, k, j, w, g)
		}
	}
	w_updated[[j]] <- 0
	g_updated[j, ] <- 0
	g_updated[, j] <- 0

	list(w = w_updated, g = g_updated)
}
