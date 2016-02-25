#' Ternary plot
#'
#'\code{pp.ternary.plot} implements a ternary plot .
#' @usage pp.ternary.plot(x, y, z, pprf, data)
#' @param x is a numeric value which identifies the selected class to plot in the left corner.
#' @param y is a numeric value which identifies the selected class to plot in the right corner.
#' @param z is a numeric value which identifies the selected class to plot in the top corner.
#' @param pprf a PPforest object
#' @param data a data frame with the complete data set
#' @export
#' @examples
#'  #crab data set without test
#' pprf.crab2 <- PPforest(y = crab[, 1], x = crab[, -1], size.tr = 1, 
#' m = 100, size.p = .8, PPmethod = 'LDA', strata = TRUE)
#' pp.ternary.plot(1, 2, 3, pprf.crab2, crab)
pp.ternary.plot <- function(x, y, z, pprf, data){
  X1 <- NULL
  X2 <- NULL
  tri <- ggplot2::ggplot(data.frame(x = c(0, 1, 1/2), y = c(0,  0,sqrt(3)/2))) + ggplot2::geom_polygon(ggplot2::aes(x = x, 
                                                                                         y = y), colour = "black", fill = NA)
  f1 <- function(x) c((1/2) * (2 * x[2] + x[3])/(sum(x)), (sqrt(3)/2) * x[3]/sum(x))
  
  if(length(levels(data[, 1])) == 4){
    aux <- pprf$votes[, c(x, y, z)] + pprf$votes[, -c(x, y, z)]/3
  }else{
    aux <- pprf$votes[, c(x, y, z)] +  apply(pprf$votes[, -c(x, y, z)], 1, sum)/3
    
  }
  sp <- apply(pprf$votes[, c(x, y, z)], 1, sum)
  pr <- data.frame(t(apply(aux, 1, f1)), class = pprf$train[, 1], sp)
  
  colnames(pr)[1:2] <- c("X1", "X2")
  
  p <- tri + ggplot2::geom_point(data = pr, ggplot2::aes(X1, X2, color = class, size = sp)) +  
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 6)) 
  plotly::ggplotly(p)
}
