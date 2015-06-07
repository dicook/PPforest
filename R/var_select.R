#' Random selection of variables
#' 
#' Index id for variables set, sample variables without replacement.
#' @param data is a data frame without the class variable. 
#' @param size.p proportion of variables used in each split
#' @return a vector giving the names of the selected variables 
#' @export
#' @examples
#' variables <- var_select(data=iris[,-5], size.p=0.9)
#' variables
var_select <- function(data, size.p) {
    nam <- colnames(data)
    var.num <- 1:length(nam)
    index <- sample(var.num, floor(length(var.num) * size.p))
    return(sort(index))
} 
