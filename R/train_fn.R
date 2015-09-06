#' Obtain stratified sample of the data 
#' 
#' Index id for training set, sample in each class with the same proportion.
#' @param y input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector  with the sample indexes for training data
#' @export
#' @importFrom magrittr %>%
#' @examples
#' training.id <- train_fn(y = leukemia[,1], size.p = 0.9)
#' training.id
train_fn <- function(y, size.p = 0.9) {
    id <- NULL
    n <- length(y)
    class.id <- dplyr::data_frame(id = 1:n, class = y)
    class.id %>% 
      dplyr::group_by(class) %>% 
      dplyr::sample_frac(size.p) %>% 
      dplyr::arrange(id) %>%
      dplyr::ungroup() %>% 
      dplyr::select(id)
} 
