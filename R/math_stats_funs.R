#' Compute the cofactor matrix
#'
#' @param input The input matrix, should be an numerical square matrix.
#'
#' @return The cofactor matrix
#' @export
#'
#' @examples
cofactor <- function(input) {
  if (!is.matrix(input)) {
    stop("The argument must be a matrix.")
  }
  if (!is.numeric(input)) {
    stop("The argument must be a numerical matrix.")
  }
  if (nrow(input) != ncol(input)) {
    stop("The argument must be a square matrix.")
  }
  order_input <- nrow(input)
  num_elements_input <- length(input)
  cofactor_vec <- sapply(1:num_elements_input, function(x){
    remove_id_column <- ifelse(x < order_input, 1, ceiling(x/order_input))
    remove_id_row <- ifelse(x <= order_input,
                            x,
                            (x - floor(x/order_input)*order_input))
    if (remove_id_row == 0) {
      remove_id_row <- order_input
    }
    left_matrix <- input[-remove_id_row, -remove_id_column]
    return(ifelse(length(left_matrix) == 1,
                  ((-1)^(remove_id_column + remove_id_row))*left_matrix,
                  ((-1)^(remove_id_column + remove_id_row))*det(left_matrix)))
  })
  return(matrix(cofactor_vec, order_input, order_input))
}


#' Convert correlation matrix into covariance matrix with given sds
#'
#' @param R Correlation matrix
#' @param sds Standard deviation vector
#'
#' @return Covariance matrix
#' @export
#'
#' @examples
#' R <- matrix(rep(0.6, 9), 3, 3)
#' diag(R) <- 1
#' cor2cov(R, c(10, 15, 12))
cor2cov <- function(R, sds){
  sd <- diag(sds)
  sd %*% R %*% sd
}


#' Compute the Mahalanobis distance of each row vector
#'
#' @param x The original data matrix
#' @param Sigma The covariance matrix of x
#'
#' @return The Mahalanobis distance vector.
#' @export
#'
#' @examples
#' data <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1, 0.6, 0.6, 1), 2, 2))
#' mahalanobis_yujun(data)
mahalanobis_yujun <- function(x, Sigma = cov(x)) {
  x_centered <- x - matrix(colMeans(x), 1)[rep(1, nrow(x)), ]
  rowSums(x_centered %*% solve(Sigma) * x_centered)
}