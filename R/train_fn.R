#' Obtain stratified sample of the data 
#' 
#' Index id for training set, sample in each class with the same proportion.
#' @param class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector  with the sample indexes for training data
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(iris)
#' training <- train_fn(class=iris[,5], size.p=0.9)
#' training
train_fn <- function(class = class, size.p = 0.9) {
    id <- NULL
    n <- length(class)
    class.id <- data.frame(id = 1:n, class = class)
    class.id %>% dplyr::group_by(class) %>% dplyr::sample_frac(size.p) %>% dplyr::arrange(id) %>% dplyr::ungroup() %>% 
        dplyr::select(id)
} 
