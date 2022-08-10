#' Get mode
#'
#' @param x a numerical vector
#'
#' @return the mode
#' @export
#'
#' @examples
#' get_mode(c(1, 2, 2, 3, 4, 4))
get_mode <- function(x) {
  if (!is.numeric(x) && !is.logical(x)) {
    stop("The input object should be be either numeric vector or logical!")
  }
  as.numeric(names(table(x)))[table(x) == max(table(x))]
}


#' Round function that mimic the rounding rule used in daily life
#' Round to 0 if the number at specified digit is less than 5, and to 1 otherwise
#'
#' @param x a numerical scalar object
#' @param digits number of digits to keep
#'
#' @return a rounded scalar object
#' @export
#'
#' @examples
#' round_off5(0.5)
round_off5 <- function(x, digits = 0) {
  trunc(abs(x)*10^digits + 0.5)*sign(x)/10^digits
}
