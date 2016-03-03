#' Random selection of variables
#' 
#' Index id for variables set, sample variables without replacement.
#' @param data Data frame with the complete data set.
#' @param size.p Sample size proportion of variables randomly sampled as candidates at each split.
#' @return a vector giving the id of the randomly sampled variables. 
#' @export
#' @examples
#' variables <- var_select(data = leukemia,  size.p = 0.9)
#' variables
var_select <- function(data, size.p) {
  id.var <- NULL
  n <- dplyr::data_frame(id.var = 1:(ncol(data) - 1))
  
  sampl <- n %>% 
    dplyr::sample_frac(size.p) %>%
    dplyr::arrange(id.var)
  
  return(sampl$id.var)

} 



