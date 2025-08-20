#' Generate contaminated survey data.
#'
#' Contamination include missing, attention check failure, repeated response, recurrent response, and speedness. Data are generated using multivariate normal distribution with given \mu and \Sigma correlation matrix.
#'
#' @param is_att_check a logical value (`TRUE`, default) to indicate whether there is attention check in generated data.
#' @param is_factor_structure a logical value (`TRUE`, default) to indicate whether there is underlying factor structure.
#' @param is_missing a logical value (`TRUE`, default) to indicate whether there is missing data in generated data.
#' @param is_outlier a logical value (`TRUE`, default) to indicate whether there is outlier.
#' @param is_recode a logical value (`TRUE`, default) to indicate whether there is variable that need to be recoded.
#' @param is_res_repeated a logical value (`TRUE`, default) to indicate whether there is repeated data in generated data.
#' @param is_res_recur a logical value (`TRUE`, default) to indicate whether there is recurrent data in generated data.
#' @param is_speedness a logical value (`TRUE`, default) to indicate whether there is speedness in generated data.
#' @param is_time_res_total a logical value (`TRUE`, default) to indicate whether the response time is summed.
#' @param mu a positive vector used to generate response data from multivariate normal distribution, the default is \boldsymboal(0).
#' @param mu_time_used_in_sec a positive integer to indicate the average time (in second) used to answer all the questions, the default is `round(n_var/3)*60`, assuming that on average, one examinee can answer 3 variables in 1 minute.
#' @param n_examinee a positive integer to indicate how many examinees in total, the default is 1000.
#' @param n_var a positive integer to indicate how many variables in total, the default is 80.
#' @param n_fac a positive integer to indicate how many factors in total, the default is 5.
#' @param n_straight_missing_max a positive integer to indicate the maximum number of missing for one examinee, the default is 10.
#' @param n_res_repeated_lower a positive integer to indicate the lower bound of the number of repeated response for one examinee, the default is 8.
#' @param n_res_repeated_upper a positive integer to indicate the upper bound of the number of repeated response for one examinee, the default is 15.
#' @param pattern_recur_res a positive integer vector to indicate the pattern of recurrent response, the default is `c(2, 3, 4)` repeated 4 times.
#' @param prop_att_check a positive double to indicate the proportion of variables in the attention check, the default is 0.01.
#' @param prop_examinee_fail_att_check a positive double to indicate the proportion of examinees who fail in the attention check, the default is 0.05.
#' @param prop_examinee_with_missing a positive double to indicate the proportion of examinees that have missing data, the default is 0.1.
#' @param prop_examinee_with_repeated_res a positive double to indicate the proportion of examinees with repeated response, the default is 0.1.
#' @param prop_examinee_with_recur_res a positive double to indicate the proportion of examinee with recurrent response, the default is 0.1.
#' @param prop_examinee_with_speedness a positive double to indicate the proportion of examinee with speedness, the default is 0.1.
#' @param prop_outlier a positive double to indicate the proportion of outlier, the default is 0.05.
#' @param prop_recode a positive double to indicate the proportion of variables that need to be recoded.
#' @param range_age a vector specify the lower bound and upper bound of the age to simulate.
#' @param scale_res the scale of response, the default is 5 level Likert.
#' @param seed a positive integer used in `set.seed()`, the default is 123456.
#' @param Sigma a correlation matrix used to generate response data from multivariate normal distribution, the defaults correlation matrix has identical intervariable correlation = 0.5.
#' @param tag_missing missing label (`NA`, default).
#' @param time_speedness_upper a positive integer to indicate the upper bound of the time that examinee with speedness used to answer all questions, the default is 1/3 of `mu_time_used_in_sec`.
#' @details This function can simulate five types of data contamination. When generating different types of contaminated data, the examinees are sampled separately, implying that the response pattern of one single examinee can contain all four types.
#'
#' For attention check, the validity variables present in the 2nd half of all variables.
#'
#' For missing data, less missing point is more likely to appear than more missing point.
#'
#' For repeated data, the number of repeated data point for one examinee is randomly selected from a given range.
#'
#' For recurrent data, the pattern is fixed.
#'
#' For speedness, response time that is closer to the prespecified upper bound enjoys higher probability when sampling.
#'
#'
#' @return a list containing generated data and all contamination information.
#' @export
#'
#' @examples simu_dirty_survey_data()
simu_dirty_survey_data <- function(
    is_att_check = TRUE,
    is_factor_structure = TRUE,
    is_missing = TRUE,
    is_outlier = TRUE,
    is_recode = TRUE,
    is_res_repeated = TRUE,
    is_res_recur = TRUE,
    is_speedness = TRUE,
    is_time_res_total = TRUE,
    mu = rep(0, n_var),
    mu_time_used_in_sec = round(n_var/3)*60,
    n_examinee = 1000,
    n_var = 80,
    n_fac = 5,
    n_straight_missing_max = 10,
    n_res_repeated_lower = 8,
    n_res_repeated_upper = 15,
    pattern_recur_res = rep(c(2, 3, 4), length.out = 12),
    prop_att_check = 0.03,
    prop_examinee_fail_att_check = 0.05,
    prop_examinee_with_missing = 0.1,
    prop_examinee_with_repeated_res = 0.1,
    prop_examinee_with_recur_res = 0.1,
    prop_examinee_with_speedness = 0.1,
    prop_outlier = 0.05,
    prop_recode = 0.03,
    range_age = c(20, 30),
    scale_res = 5,
    seed = 123456,
    Sigma = matrix(
      rep(c(1, rep(0.5, n_var)), length.out = n_var^2),
      n_var, n_var
    ),
    tag_missing = NA,
    time_speedness_upper = round(mu_time_used_in_sec/3)
  ) {

  if (!is.null(seed)) {
    set.seed(seed)
  }
  name_var <- paste(sample(c(letters, LETTERS), 1), 1:n_var, sep = "")
  category_res <- 1:scale_res
  if (is_factor_structure) {
    n_var_per_fac <- ceiling(n_var/n_fac)
    id_primary_loading <- lapply(1:n_fac, \(x){
      cbind(1:n_var_per_fac + (x - 1)*n_var_per_fac, x)
    })
    id_primary_loading <- Reduce(rbind, id_primary_loading)
    id_primary_loading <- id_primary_loading[1:n_var, ]
    name_var <- paste(
      rep(
        sample(c(letters, LETTERS), n_fac),
        each = n_var_per_fac,
        length.out = n_var
      ),
      1:n_var_per_fac,
      sep = ""
    )
    id_primary_loading <- id_primary_loading[, 1] +
      (id_primary_loading[, 2] - 1)*n_var
    factor_loadings <- matrix(0, n_var, n_fac)
    factor_loadings[id_primary_loading] <- 0.8
    factor_correlation <- matrix(0.3, n_fac, n_fac)
    diag(factor_correlation) <- 1
    Sigma <- factor_loadings %*% factor_correlation %*% t(factor_loadings)
    diag(Sigma) <- 1
  }
  data_simu <- MASS::mvrnorm(n_examinee,
                             mu = rep(0, n_var),
                             Sigma = Sigma)
  cut_off <- stats::qnorm(seq(0.1, 0.9, length.out = scale_res - 1))
  data_simu[data_simu > cut_off[scale_res - 1]] <- category_res[scale_res]
  data_simu[data_simu < cut_off[1]] <- 1
  for (i in 2:(scale_res - 1)) {
    data_simu[data_simu > cut_off[i - 1] & data_simu < cut_off[i]] <- i
  }
  colnames(data_simu) <- name_var

  # outlier
  if (is_outlier) {
    n_outlier <- ceiling(prop_outlier * n_var)
    id_outlier <- order(rowSums(data_simu), decreasing = T)[1:n_outlier]
    for (r in seq_along(id_outlier)) {
      posi <- sample(1:n_var, round(0.6 * n_var))
      data_simu[id_outlier[r], posi] <- sample(
        c(4, 5), length(posi), replace = TRUE
      )
    }
  }

  # attention check
  if (is_att_check) {
    n_att_check <- ceiling(prop_att_check * n_var)
    id_att_check <- sample(round(n_var/2), n_att_check)
    tag_att_check <- sample(scale_res, n_att_check, replace = TRUE)
    n_examinee_fail_att_check <-
      ceiling(prop_examinee_fail_att_check * n_examinee)
    id_fail_att_check <- sample(n_examinee, n_examinee_fail_att_check)
    for (r in 1:n_att_check) {
      data_simu[, id_att_check[r]] <-
        ifelse(
          1:n_examinee %in% id_fail_att_check,
          sample(
           category_res[!(category_res %in% tag_att_check[r])],
           n_examinee,
           replace = TRUE
          ),
          tag_att_check[r]
        )
    }
    colnames(data_simu)[id_att_check] <-
      paste(colnames(data_simu)[id_att_check], "_att", sep = "")
  }

  # recode
  if (is_recode) {
    id_recode <-
      ifelse(
        is_att_check,
        sample(
          (1:n_var)[!1:n_var %in% id_att_check],
          ceiling(prop_recode*n_var)
        ),
        sample(1:n_var, ceiling(prop_recode*n_var))
      )
    for (r in seq_along(id_recode)) {
      data_simu[, id_recode[r]] <- scale_res + 1 - data_simu[, id_recode[r]]
    }
    colnames(data_simu)[id_recode] <-
      paste(colnames(data_simu)[id_recode], "_recode", sep = "")
  }

  # missing
  if (is_missing) {
    n_examinee_with_missing <- round(prop_examinee_with_missing * n_examinee)
    id_examinee_with_missing <- sample(n_examinee, n_examinee_with_missing)
    for (r in id_examinee_with_missing) {
      data_simu[r, ][
        sample(
          n_var,
          sample(
            n_straight_missing_max:1,
            1,
            prob = (1:n_straight_missing_max)/sum(1:n_straight_missing_max)
          )
        )
      ] <- tag_missing
    }
  }

  # repeated response
  if (is_res_repeated) {
    n_examinee_with_repeated_res <-
      round(prop_examinee_with_missing * n_examinee)
    id_examinee_with_repeated_res <- sample(
      n_examinee,
      n_examinee_with_repeated_res
    )
    for (r in id_examinee_with_repeated_res) {
      n_repeated_res <- sample(n_res_repeated_lower:n_res_repeated_upper, 1)
      data_simu[r, n_var:(n_var - n_repeated_res + 1)] <-
        sample(category_res, 1)
    }
  }

  # recurrent response
  if (is_res_recur) {
    n_examinee_with_recur_res <-
      round(prop_examinee_with_recur_res * n_examinee)
    id_examinee_with_recur_res <- sample(n_examinee, n_examinee_with_recur_res)
    for (r in id_examinee_with_recur_res) {
      data_simu[r, (n_var - length(pattern_recur_res) + 1):n_var] <-
        pattern_recur_res
    }
  }

  # speedness
  if (is_time_res_total) {
    data_simu <-
      cbind(
        data_simu,
        round(stats::rnorm(
          n_examinee,
          mu_time_used_in_sec,
          sqrt(mu_time_used_in_sec)
        ))
      )
    colnames(data_simu)[ncol(data_simu)] <- "time"
  }

  if (is_speedness) {
    n_examinee_with_speed_res <-
      round(prop_examinee_with_speedness * n_examinee)
    id_examinee_with_speed_res <- sample(n_examinee, n_examinee_with_speed_res)
    for (r in id_examinee_with_speed_res) {
      data_simu[r, "time"] <-
        sample(
          1:time_speedness_upper,
          1,
          prob = (1:time_speedness_upper)/sum(1:time_speedness_upper)
        )
    }
  }

  # generate random names and other demographics
  data_simu <-
    cbind(
      as.data.frame(unclass(randomNames::randomNames(
        n_examinee,
        return.complete.data = TRUE
      ))),
      age = sample(range_age[1]:range_age[2], n_examinee, replace = TRUE),
      data_simu
    )

  output <- list(data = data_simu)
  if (is_recode) output$id_item_recode <- id_recode
  if (is_att_check) {
    output$id_item_att_check <- id_att_check
    output$tag_att_check <- tag_att_check
  }
  if (is_outlier) output$id_examinee_outlier <- id_outlier
  if (is_missing) output$id_examinee_missing <- id_examinee_with_missing
  if (is_res_repeated) output$id_examinee_repeat <- id_examinee_with_repeated_res
  if (is_res_recur) output$id_examinee_recurrent <- id_examinee_with_recur_res
  if (is_speedness) output$id_examinee_speedness <- id_examinee_with_speed_res

  return(output)
}
