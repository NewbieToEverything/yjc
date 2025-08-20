#' EFA using fit indices, a wrapper of 'stats::factanal'
#'
#' @param data_input input data, should be either a matrix or dataframe
#' @param nfactors number of factors to be tested in EFA
#' @param fit if 'TRUE' (default), model fit information will be extracted
#' @param IC if 'TRUE' (default is 'FALSE'), information criterion will be provided, i.e. AIC, BIC, EBIC
#' @param ... Further arguments passed to or from other methods.
#'
#' @return a regular factanal return list with fit indice added
#' @importFrom stats factanal
#' @export
#'
#' @examples
#' library(MASS)
#' test <- factanal_fit(mvrnorm(200, rep(0, 6), diag(6)), 1)
#' test$fit_indices
#' test <- factanal_fit(mvrnorm(200, rep(0, 6), diag(6)), 1, TRUE, TRUE)
#' test$fit_indices
factanal_fit <- function(data_input, nfactors = 1, fit = TRUE, IC = FALSE, ...) {
  factanal_results <- factanal(data_input, nfactors, ...)
  if (fit) {
    m <- factanal_results$factors
    p <- length(factanal_results$uniquenesses)
    n <- factanal_results$n.obs
    q <- p + p*m - m*(m - 1)/2
    df <- ((p - m)^2 - (p + m))/2
    tml <- factanal_results$STATISTIC
    tml_0 <- -(n - 1)*log(det(factanal_results$correlation))
    df_0 <- p*(p + 1)/2 - p

    AIC <- tml + 2*q/n
    BIC <- tml + log(n)*q
    EBIC <- df + tml + log(n)*q
    CFI <- 1 - (tml - df)/(tml_0 - df_0)
    TLI <- 1 - (df_0/df)*(tml - df)/(tml_0 - df_0)
    RMSEA <- sqrt(max((tml - df)/(df*(n - 1)), 0))
    SRMR <- SRMR_factanal(factanal_results)
    # RMSEA_0 <- sqrt((tml_0 - df_0)/(df_0*(n - 1)))
    fit_indices <- round(data.frame(AIC, BIC, EBIC, CFI, TLI, RMSEA, SRMR),
                         digits = 3)
    if (!IC) {
      fit_indices <- fit_indices[,-(1:3)]
    }
    factanal_results$fit_indices <- fit_indices
  }

  return(factanal_results)
}


#' compute SRMR using factanal output
#'
#' @param factanal_results output list object provided by 'stats::factanal'
#'
#' @return SRMR
#' @export
#'
#' @examples
#' library(MASS)
#' library(stats)
#' data <- mvrnorm(200, rep(0, 6), diag(6))
#' SRMR_factanal(factanal(data, 1))
SRMR_factanal <- function(factanal_results) {
  p <- length(factanal_results$uniquenesses)
  cor_implied <-  factanal_results$loadings %*% t(factanal_results$loadings)
  diag(cor_implied) <- 1
  cor_sample <- factanal_results$correlation
  sqrt(sum((cor_sample - cor_implied)^2)/(p*(p + 1)/2))
}


#' Estimate the number of factors using 11 factor retention methods
#'
#' The 11 retention methods including chi square test (Bartlett test statistics),
#' CFI, TLI, RMSEA, CD, EKC, CAF, HULL-CFI, HULL-RMSEA, KGC, PA.
#'
#' @param data_input input data, should be either a matrix or dataframe
#'
#' @return a vector, containing the number of factors suggested by 11 retention methods
#' @importFrom stats qchisq
#' @importFrom EFAtools N_FACTORS
#' @export
#'
#' @examples
#' library(MASS)
#' nfactors(mvrnorm(200, rep(0, 6), diag(6)))
#` NULL
nfactors <- function(data_input) {
  # initialization
  EFA_results <- list()  # the results returned by factanal for all fitted models
  p <- ncol(data_input)
  max_m <- floor(p/2) - 3  # the maximum number of factors yet to test
  max_m <- ifelse(max_m == 0, 1, max_m)
  # the collection of chi2_B, CFI, TLI and RMSEA for all fitted models
  results <- matrix(0, nrow = max_m, ncol = 4)
  # the cut_off values of chi_square test for all fitted models
  cut_off_chisquare <- matrix(0, nrow = max_m, ncol = 1)
  nfactors <- matrix(0, 4)  # the number of factors determined by chi2_B, CFI, TLI and RMSEA

  # determine number of factors
  # using the "N_FACTORS" function of "EFAtools" package to determine nfactors
  # methods used are comparison data, Hull method, Kaiser-Guttman criterion,
  #   parallel analysis.
  EFAtools_results <- N_FACTORS(data_input,
                                method = "ULS",
                                eigen_type_HULL = "PCA",
                                eigen_type_other = "PCA",
                                criteria = c("CD",
                                             "EKC",
                                             "HULL",
                                             "KGC",
                                             "PARALLEL"))
  # remove unwanted results
  EFAtools_results$n_factors <- EFAtools_results$n_factors[-c(7,
                                                              8,
                                                              10,
                                                              11,
                                                              12,
                                                              13,
                                                              14)]
  # sequential chi-square model test
  for (m in 1:max_m) {
    q <- p + p*m - m*(m - 1)/2
    df <- ((p - m)^2 - (p + m))/2
    EFA_results[[m]] <- factanal_fit(data_input, m, rotation = "none")
    if (EFA_results[[m]]$converged == TRUE) {
      results[m, 1] <- EFA_results[[m]]$STATISTIC[1]
      results[m, 2] <- EFA_results[[m]]$fit_indices$CFI
      results[m, 3] <- EFA_results[[m]]$fit_indices$TLI
      results[m, 4] <- EFA_results[[m]]$fit_indices$RMSEA
      cut_off_chisquare[m] <- qchisq(0.95, df)
    } else {
      break
    }
  }
  # filter the converged solution and their corresponding cut-off value
  if (m - 1 > 1) {
    results <- results[1:(m - 1), ]
  }
  cut_off_chisquare <- cut_off_chisquare[1:(m - 1), ]
  nfactors[1] <- min(which((results[, 1] < cut_off_chisquare) == TRUE))  # SMT
  nfactors[2] <- min(which(results[, 2] > 0.95))  # CFI
  nfactors[3] <- min(which(results[, 3] > 0.95))  # TLI
  nfactors[4] <- min(which(results[, 4] < 0.05))  # RMSEA
  nfactors[is.infinite(nfactors)] <- NA
  all_nfactors <- as.data.frame(t(as.matrix(c(nfactors, EFAtools_results$n_factors))))
  names(all_nfactors) <- c("chi_square_Bartlett", "CFI", "TLI", "RMSEA",
                           "CD", "EKC", "CAF", "HULL_CFI", "HULL_RMSEA",
                           "KGC", "PA")
  return(all_nfactors)
}
