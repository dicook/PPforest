#' Index id for training set, sample in each class with the same proportion.
#'
#' @param class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector giving number with the sample indexes for training
#' @export
#' @examples
#' training<-train_fn(class=iris[,5],size.p=0.9)
#' training
train_fn <-  function(class, size.p) {
  n <- length(class)
  class.id <- data.frame(id=1:n,class=class)
  index  <- unlist(plyr::dlply(class.id,plyr::.(class), function(x) 
  sample(x$id, floor(length(x$class)*size.p))))
  names(index) <- NULL
  return(sort(index))
}
