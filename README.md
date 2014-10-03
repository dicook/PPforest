PPforest
========
This is going to be an R package that extends PPtree to incorporate ideas of random forests 
=======

Sample index function
--------

```{r} 
#' Index id for training set.
#'
#' @param i.class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector giving number with the sample indexes for training
#' @examples
#' train_fn(iris[,5],.9)
train_fn <- function(class,size.p){
  n <- length(class)
  class.id <- data.frame(id=1:n,class=class)
  index  <- unlist(dlply(class.id,.(class),function(x) sample(x$id,round(length(x$class)*size.p,1))))
  names(index) <- NULL
  return(sort(index))
}

```