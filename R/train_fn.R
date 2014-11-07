#' Index id for training set, sample in each class with the same proportion.
#'
#' @param class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector giving number with the sample indexes for training
#' @export
#' @examples
#' training<-train_fn(iris[,5],.9)
#' training
train_fn <- function(class, size.p) {
  n <- length(class)
  class.id <- data.frame(id=1:n,class=class)
  index  <- unlist(plyr:: dlply(class.id, .(class), function(x) 
    sample(x$id, round(length(x$class)*size.p, 1))))
  names(index) <- NULL
  return(sort(index))
}


