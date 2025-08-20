# author: Wenzheng Lin
# collaborator: Yujun Li
recode_vars <- function(data, max = 5, min = 1, vars = names(data)){
  rule <- paste(min:max, max:min, sep = "=", collapse = ";")
  if (!is.data.frame(data) & is.vector(data)) {
    # recode single variable
    output <- car::recode(data, rule)
    return(output)
  }
  output <- as.data.frame(apply(data[vars], 2, \(x) car::recode(x, rule)))
  # recode some variables
  if (ncol(data) != length(vars)) {
    vars_rest <- !names(data) %in% vars
    output <- data.frame(data[, vars_rest], output)
    names(output)[1:(ncol(output) - length(vars))] <- names(data)[vars_rest]
    output <- output[names(data)]
  }
  return(output)
}
