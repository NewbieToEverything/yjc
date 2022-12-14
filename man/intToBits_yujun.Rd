% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my functions.R
\name{intToBits_yujun}
\alias{intToBits_yujun}
\title{Convert int object to bit vector}
\usage{
intToBits_yujun(x, signed = TRUE, nbits = 8L)
}
\arguments{
\item{x}{An int scalar object.}

\item{signed}{If true (default), x is treated as a signed int.}

\item{nbits}{number of bit to convert, default is 8.}
}
\value{
The converted bit vector.
}
\description{
Convert int object to bit vector
}
\examples{
intToBits_yujun(127)
intToBits_yujun(255, FALSE)
}
