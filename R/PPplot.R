#' MDS
#'
#' @param ppfo  a PPforest object
#' @param train is a data frame with the training data with class in the first column
#' @param k number of MDS dimensions to plot
#' @return proximity matrix plot and MDS plot  
#' @export
#' @importFrom magrittr %>%
#' @examples
#' tr.index <- train_fn(iris[, 5], 2/3)
#' te.index <- as.vector(1:length(iris[, 5]))[!(1:length(iris[, 5]) %in% (sort(tr.index$id)))]
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppfr.iris <- PPforest( train = train, testap = TRUE, test = test, m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' PPplot(ppfr.iris, train, k = 2)
#' PPplot(ppfr.iris, train, k = 3)
PPplot <- function(ppfo, train, k) {
  id <- diag(dim(train)[1])
  id[lower.tri(id, diag = TRUE)] <- ppfo[[9]]$proxi
  id[upper.tri(id)] <-  t(id)[upper.tri(id)]
  ggplot2::ggplot( reshape2::melt(id), ggplot2::aes( Var1,Var2, fill = value ) ) + ggplot2::geom_raster()
  
  rf.mds <- stats::cmdscale( 1 - id, eig = TRUE, k = k  )
  colnames(rf.mds$points) <- paste( "Dim", 1:k, sep = '')
  nlevs <- nlevels(train[,1])
  
  if (k <= 2) {
    df <- data.frame( fac = train[,1], rf.mds$points )
    ggplot2::ggplot( data = df, ggplot2::aes( x = Dim1, y = Dim2,color = train[,1] ) ) +ggplot2::scale_colour_discrete( name = "Class" )  + ggplot2::geom_jitter()
  }
  else {
    df <- data.frame( fac = train[,1], rf.mds$points)
    GGally::ggpairs(data = df[,-1], colours = as.factor( train[, 1] ) )
  }
  
}



