#' Proximity matrix visualization 
#'
#' MDS plot using proximity matrix inforation.
#' @param ppfo a PPforest object
#' @param k number of dimensions of the MDS layout 
#' @return MDS plot  
#' @export
#' @examples
#' ppfr.iris <- PPforest(data = iris[,5:1], size.tr = 2/3, m = 500, size.p = .9, 
#'  PPmethod = 'LDA', strata = TRUE)
#'  PPplot(ppfr.iris, k = 2)
#'  PPplot(ppfr.iris, k = 3)
PPplot <- function(ppfo, k) {
  value <- NULL
  Var1 <- NULL
  Var2 <- NULL
  MDS1 <- NULL
  MDS2 <- NULL
  fac <- NULL

  id <- diag(dim(ppfo$train)[1])
  id[lower.tri(id, diag = TRUE)] <- 1-ppfo[[9]]$proxi  
  rf.mds <- stats::cmdscale( as.dist(id), eig = TRUE, k = k  )
  colnames(rf.mds$points) <- paste( "MDS", 1:k, sep = '')
  nlevs <- nlevels(ppfo$train[,1])
  
  if (k == 2) {
    df <- data.frame( fac = ppfo$train[,1], rf.mds$points )
    ggplot2::ggplot( data = df) + 
      ggplot2::geom_point(ggplot2::aes( x = MDS1, y = MDS2, color = fac) ) +
      ggplot2::theme(aspect.ratio = 1) + 
      ggplot2::scale_colour_discrete( name = "Class" )  
      
  }
  else {
    df <- data.frame( fac = ppfo$train[,1], rf.mds$points)
    GGally::ggpairs(data = df, columns =2:ncol(df), colour = "fac"  )
  }
  
}



