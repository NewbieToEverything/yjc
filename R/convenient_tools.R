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


#' Convert int scalar to bit
#'
#' @param x An int scalar object.
#' @param signed If true (default), x is treated as a signed int.
#' @param nbits number of bit to convert, default is 8.
#'
#' @return The converted bit vector.
#' @export
#'
#' @examples
#' intToBits_yujun(127)
#' intToBits_yujun(255, FALSE)
intToBits_yujun <- function(x, signed = TRUE, nbits = 8L) {
  if (!is.integer(x) & !is.numeric(x)) stop("x should at least be an number.")
  sign_x <- sign(x)
  if (!is.integer(x) & is.double(x)) {
    x <- trunc(abs(x))
    warning("x is not an integer object, and has been truncated.")
  }

  if (x == 0L) return(x)

  upper <- ifelse(signed, 2L^nbits/2L - 1L, 2L^nbits - 1L)
  if (x > upper | x < (-1L * (upper + 1L))) stop("overflow occurred")

  bits <- vector(mode = "integer", nbits)

  count <- 1L
  while (x != 0L) {
    bits[nbits + 1L - count] <- x %% 2L
    x <- x %/% 2L
    count <- count + 1L
  }
  return(paste(bits[-(1L:(nbits - count + 1L))], collapse = ""))
}


#' Convert float scalar into bits
#'
#' @param x A float scalar.
#' @param nbits Number of bits to convert.
#'
#' @return Converted bits object.
#' @export
#'
#' @examples
#' floatToBits_yujun(0.1)
floatToBits_yujun <- function(x, nbits = 53L) {
  part_int <- as.integer(trunc(x))
  if (x == part_int) {
    return(intToBits_yujun(part_int))
  } else {
    part_decimal <- x - part_int
    bits <- vector(mode = "integer", nbits)
    for (i in 1:nbits) {
      part_decimal <- part_decimal * 2
      bits[i] <- trunc(part_decimal)
      part_decimal <- part_decimal - bits[i]
      if (part_decimal == 0) break
    }
    return(paste(intToBits_yujun(part_int),
                 paste(bits[1:i], collapse = ""),
                 sep = "."))
  }
}
