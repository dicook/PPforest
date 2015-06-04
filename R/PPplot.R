#' MDS
#'
#' @param ppfo a PPforest object
#' @param train is a data frame with the training data with class in the first column
#' @param k number of dimensions of the MDS layout 
#' @return MDS plot  
#' @export
#' @examples
#' tr.index <- train_fn(iris[, 5], 2/3)
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppfr.iris <- PPforest( train = train, testap = TRUE, test = test,
#'  m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' PPplot(ppfr.iris, train, k = 2)
#' PPplot(ppfr.iris, train, k = 3)
#' tr.index <- train_fn(lymphoma[, 1], 2/3)
#' train <- lymphoma[sort(tr.index$id),  ]
#' test <- lymphoma[-tr.index$id, ]
#' ppfr.lymphoma <- PPforest( train = train, testap = TRUE, test = test,
#'  m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' PPplot(ppfr.iris, train, k = 2)

PPplot <- function(ppfo, train, k) {
  value <- NULL
  Var1 <- NULL
  Var2 <- NULL
  MDS1 <- NULL
  MDS2 <- NULL
  fac <- NULL

  id <- diag(dim(train)[1])
  id[lower.tri(id, diag = TRUE)] <- 1-ppfo[[9]]$proxi  
  rf.mds <- stats::cmdscale( as.dist(id), eig = TRUE, k = k  )
  colnames(rf.mds$points) <- paste( "MDS", 1:k, sep = '')
  nlevs <- nlevels(train[,1])
  
  if (k == 2) {
    df <- data.frame( fac = train[,1], rf.mds$points )
    ggplot2::ggplot( data = df) + 
      ggplot2::geom_point(ggplot2::aes( x = MDS1, y = MDS2, color = fac) ) +
      ggplot2::theme(aspect.ratio = 1) + 
      ggplot2::scale_colour_discrete( name = "Class" )  
      
  }
  else {
    df <- data.frame( fac = train[,1], rf.mds$points)
    GGally::ggpairs(data = df, columns =2:ncol(df), colour = "fac"  )
  }
  
}



