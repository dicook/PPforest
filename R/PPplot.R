#' MDS
#'
#' @param ppfo a PPforest object
#' @param train is a data frame with the training data with class in the first column
#' @param k number of dimensions of the MDS layout 
#' @return MDS plot  
#' @export
#' @examples
#' tr.index <- train_fn(iris[, 5], 2/3)
#' te.index <- as.vector(1:length(iris[, 5]))[!(1:length(iris[, 5]) %in% (sort(tr.index$id)))]
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppfr.iris <- PPforest( train = train, testap = TRUE, test = test,
#'  m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' PPplot(ppfr.iris, train, k = 2)
#' PPplot(ppfr.iris, train, k = 3)
PPplot <- function(ppfo, train, k) {
  value <- NULL
  Var1 <- NULL
  Var2 <- NULL
  MDS1 <- NULL
  MDS2 <- NULL
  fac <- NULL

  id <- diag(dim(train)[1])
  id[lower.tri(id, diag = TRUE)] <- ppfo[[9]]$proxi
  id[upper.tri(id)] <-  t(id)[upper.tri(id)]
  
  rf.mds <- stats::cmdscale( 1 - id, eig = TRUE, k = k  )
  colnames(rf.mds$points) <- paste( "MDS", 1:k, sep = '')
  nlevs <- nlevels(train[,1])
  
  if (k == 2) {
    df <- data.frame( fac = train[,1], rf.mds$points )
    ggplot2::ggplot( data = df) + 
      ggplot2::geom_jitter(ggplot2::aes( x = MDS1, y = MDS2, color = fac) ) +
      ggplot2::theme(aspect.ratio = 1) + 
      ggplot2::scale_colour_discrete( name = "Class" )  
      
  }
  else {
    df <- data.frame( fac = train[,1], rf.mds$points)
    GGally::ggpairs(data = df, columns =2:ncol(df), colour = "fac"  )
  }
  
}



