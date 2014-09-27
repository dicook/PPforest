#' Index id for training set.
#'
#' @param i.class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector with the sample indexes for training
#' @examples
#' train_fn(iris[,5],.9)
train_fn <- function(i.class,size.p){
  n <- length(i.class)
  class.id <- data.frame(id=1:n,class=i.class)
  index  <- unlist(dlply(aux,.(i.class),function(x) sample(x$id,round(length(x$class)*size.p,1))))
  names(index) <- NULL
  return(sort(index))
}