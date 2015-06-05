#' Proximity matrix heat map plot
#'
#' @param ppfo a PPforest object
#' @return proximity matrix plot  
#' @export
#' @examples
#' ppfr.iris <- PPforest(data = iris[,5:1], size.tr=2/3, testap = TRUE, m = 500, size.p = .9, 
#' PPmethod = 'LDA', std = TRUE, strata = TRUE)
#' pproxy_plot(ppfr.iris)
pproxy_plot <- function(ppfo){
  value <- NULL
  Var1 <- NULL
  Var2 <- NULL
  
  id <- diag(dim(ppfo$train)[1])
  id[lower.tri(id, diag = TRUE)] <- ppfo[[9]]$proxi
  id[upper.tri(id)] <-  t(id)[upper.tri(id)]
  m.prox <-  reshape2::melt(id)
  
  ggplot2::ggplot(m.prox, ggplot2::aes( Var1,Var2, fill = value ) ) + 
    ggplot2::scale_y_reverse() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::scale_fill_gradient( high="#132B43", low="#56B1F7",name = "Proximity")
}
