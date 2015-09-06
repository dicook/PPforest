#' Random selection of variables
#' 
#' Index id for variables set, sample variables without replacement.
#' @param x is a data frame with explanatory variables. 
#' @param size.p proportion of variables used in each split
#' @return a vector giving the names of the selected variables 
#' @export
#' @examples
#' variables <- var_select(x = leukemia[, -1], size.p = 0.9)
#' variables
var_select <- function(x, size.p) {
    nam <- colnames(x)
    var.num <- 1:length(nam)
    index <- sample(var.num, floor(length(var.num) * size.p))
    return(sort(index))
} 
