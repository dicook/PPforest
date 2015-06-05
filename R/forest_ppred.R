#' Vector with predicted values from a PPforest.
#'
#' @param newdata are the complete data without the class variable we want to predict
#' @param output.tree trees classifiers from trees_pp function or PPforest object
#' @param ... arguments to be passed to methods
#' @return predicted values form PPforest  
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(iris)
#' training <- train_fn(class = iris[, 5], size.p = 2/3)
#' iris.b <- bootstrap( iris[training$id, 5:1], 500) 
#' output <- trees_pp(iris.b, size.p = 0.9, PPmethod ="LDA") 
#' pr <- forest_ppred( iris[-training$id, 1:4] , output)
forest_ppred <- function(newdata, output.tree, ...){
  . <- NULL
aux <- "PPforest" %in%class(output.tree)
if(aux)  {
  output.tree <- output.tree[[8]]
  
}
votes <- output.tree %>% 
              dplyr::do(tr = PPtreeViz::PP.classify(test.data = newdata, Tree.result = .$tr, Rule = 1)) 
    
  out <- votes %>%  dplyr::do(pred = .$tr[[2]] )
  
  vote.mat <- matrix(unlist(out$pred), ncol = dim(newdata)[[1]], byrow = T)
  max.vote <- apply(vote.mat, 2, function(x) {
    t1 <- table(x)
    names(t1)[which.max(t1)]
  }
  )


  return(list(out, vote.mat, max.vote))
}
