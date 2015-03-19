#' Index id for training set, sample in each class with the same proportion.
#' @param class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector giving number with the sample indexes for training
#' @export
#' @examples
#' set.seed(3)
#' training<-train_fn2(class=iris[,5],size.p=0.9)
#' training
train_fn2 <-function(class=class,size.p=.9){
  n <- length(class)
  class.id <- data.frame(id=1:n,class=class)
  dplyr::class.id%>%
  dplyr::group_by(class) %>% 
  dplyr::sample_frac(size.p)  %>%
  dplyr::arrange(id) %>% 
  dplyr::ungroup() %>%
  dplyr::select(id)
}