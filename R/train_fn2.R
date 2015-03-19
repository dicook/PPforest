#' Index id for training set, sample in each class with the same proportion.
#' @param class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector giving number with the sample indexes for training
#' @export
#' @examples
#' training<-train_fn2(class=iris[,5],size.p=0.9)
#' training
train_fn2 <-function(class=class,size.p=.9){
  n <- length(class)
  class.id <- data.frame(id=1:n,class=class)
  class.id %>%
    group_by(class) %>% 
    sample_frac(size.p)  %>%
    arrange(id) %>% 
    ungroup() %>%
    select(id)
}