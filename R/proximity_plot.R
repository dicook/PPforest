#' Proximity matrix heat map plot
#'
#' @param ppfo a PPforest object
#' @param train is a data frame with the training data with class in the first column
#' @return proximity matrix plot  
#' @export
#' @examples
#' tr.index <- train_fn(iris[, 5], 2/3)
#' te.index <- as.vector(1:length(iris[, 5]))[!(1:length(iris[, 5]) %in% (sort(tr.index$id)))]
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppfr.iris <- PPforest( train = train, testap = TRUE, test = test,
#'  m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' pproxy_plot(ppfr.iris, train)
pproxy_plot <- function(ppfo, train){
  value <- NULL
  Var1 <- NULL
  Var2 <- NULL
  
  id <- diag(dim(train)[1])
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
