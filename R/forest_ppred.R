#' Vector with predicted values from a PPforest.
#'
#' @param data are the data without class we want to predict
#' @param output.tree trees classifiers from trees_pp function
#' @param ... arguments to be passed to methods
#' @return predicted values form PPforest  
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(iris)
#'iris.sc <- data.frame(Class = iris[, 5], scale(iris[, 1:4]))
#'training <- train_fn(class = iris[, 5], size.p = 2/3)
#'iris.b <- bootstrap( iris.sc[training$id, ], 500) 
#'output <- trees_pp(iris.b, size.p = 0.9, PPmethod ="LDA") 
#'pr <- forest_ppred( iris.sc[-training$id, 2:5] , output)
forest_ppred <- function(data, output.tree, ...){
  . <- NULL
    votes <- output.tree %>% 
              dplyr::do(tr = PPtreeViz::PP.classify(test.data = data, Tree.result = .$tr, Rule = 1)) 
    
  out <- votes %>%  dplyr::do(pred = .$tr[[2]] )
  
  vote.mat <- matrix(unlist(out$pred), ncol = dim(data)[[1]], byrow = T)
  max.vote <- apply(vote.mat, 2, function(x) {
    t1 <- table(x)
    names(t1)[which.max(t1)]
  }
  )


  return(list(out, vote.mat, max.vote))
}
