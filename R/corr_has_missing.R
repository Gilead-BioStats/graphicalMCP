#' Check if a correlation matrix has any missing values in a given subset
#'
#' @param corr A numeric matrix
#' @param indices A numeric vector indicating which rows and columns to check in
#'   for missing values
#'
#' @return A logical value indicating whether the correlation matrix has missing
#'   values within the given indices
#'
#' @export
#'
#' @examples
#' corr <- matrix(nrow = 4, ncol = 4)
#' corr[3:4, 3:4] <- .5
#' diag(corr) <- 1
#'
#' corr_has_missing(corr, 2:3)
#' # TRUE
#'
#' corr_has_missing(corr, 3:4)
#' # FALSE
corr_has_missing <- function(corr, indices) {
  any(is.na(corr[indices, indices]))
}
