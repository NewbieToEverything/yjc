#' Pattern Matching
#'
#' Horizontally collapse a survey data matrix or data.frame into a character vector, and search for matches to argument `pattern` in each element of the resultant vector.
#'
#' @author Yujun Li
#'
#' @param x a character vector where matches are sought.
#' @param pattern character string containing either regular expressions or permutation pattern specification, see details for more information.
#' @param scale_point integer, the scale point, use the Likert-5 as default.
#' @param rm_empty logical, indicating whether to remove the elements of resulting list with length 0.
#' @param repeats.allowed logical, indicating whether repeated response pattern is allowed when generating using `gtools::permutation`.
#' @param simplify logical, indicating whether results should be simplified if possible.
#' @param method character, indicating the method used to locate recursive response.
#'
#' @return a list of the same length as argument `pattern` (regular expression) or resultant permutations (permutation pattern specification), of which the element housing the indices of the elements of `x` that yielded a match of corresponding `pattern`.
#' @export
#'
#' @details
#'
#' `Pattern` accepts either
#'
#' 1). regular expressions, each leads to a element containing the indices of matched element of `x`.
#'
#' 2). permutations pattern specification, a character scalar, has to be in the form of "p:n1,r1|n2,r2", start with the prefix "p:", where "n" corresponding to the number of element to draw from all possible response categories, "r" indicating the number of times that the permutations of previously drawn elements being repeated consecutively. The permutations are generated using the `permutations` function of the package `gtools`. For example, "p:2,5" means that any two of all the response categories occurs 5 straight times will be detected, the resultant permutations for `scale_point = 5` are "1212121212", "1313131313", "1414141414", "1515151515", "2121212121" and etc.
#'
#' @examples
#' df <- data.frame(
#'   v1 = c(1, 2, 2, 4, 1),
#'   v2 = c(1, 3, 2, 5, 2),
#'   v3 = c(1, 2, 2, 3, 3),
#'   v4 = c(1, 4, 3, 4, 5)
#' )
#' df
#' # character
#' find_recursive_res(df, pattern = c("111", "222", "123"))
#' # regular expressions
#' find_recursive_res(df, pattern = c("1{3}", "2{3}", "123{1}"))
#' # permutation pattern specification
#' find_recursive_res(df, pattern = "p:1,3|3,1")
find_recursive_res <- function(x,
                               pattern = NULL,
                               scale_point = 5,
                               rm_empty = TRUE,
                               repeats.allowed = TRUE,
                               simplify = FALSE,
                               method = "grep") {
  # permutation match, generate all permutation as character vector
  if (method == "grep") {
    if (all(startsWith(pattern, "p:")) | all(startsWith(pattern, "P:"))) {
      pattern <- strsplit(strsplit(pattern, ":")[[1]][2], "\\|")[[1]]
      pattern <- lapply(pattern, \(y) {
        pattern_split <- strsplit(y, ",")[[1]]
        perm_all <-
          gtools::permutations(
            scale_point,
            as.numeric(pattern_split[1]),
            repeats.allowed = repeats.allowed
          )
        perm_all <- perm_all[, rep(1:ncol(perm_all), as.numeric(pattern_split[2]))]
        perm_all <- apply(perm_all, 1, \(z) paste(z, collapse = ""))
      })
      pattern <- unlist(pattern)
    }
    # Horizontally collapse x into a char vector and match pattern
    x_char <- sapply(1:nrow(x), \(y) paste(x[y, ], collapse = ""))
    recursive_res <- lapply(pattern, \(x) y <- grep(x, x_char))

    if (rm_empty) {
      length_recursive_res <- sapply(recursive_res, length)
      recursive_res <- recursive_res[length_recursive_res != 0]
      names(recursive_res) <- pattern[length_recursive_res != 0]
    } else {
      names(recursive_res) <- pattern
    }
    if (simplify) {
      recursive_res <- sort(unique(unlist(recursive_res)))
    }
  }
  if (method == "rle") {

  }
  return(recursive_res)
}
