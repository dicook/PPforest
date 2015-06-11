#' Obtain predicted class for new data using PPforest 
#' 
#' Vector with predicted values from a PPforest.
#' @param object an object of class PPforest
#' @param ... arguments to be passed to methods
#' @return predicted values form PPforest  
#' @export
#' @importFrom magrittr %>%
#' @examples
#' pprf.iris <- PPforest(data = iris[,5:1], size.tr = .9, m = 500, size.p = .9, 
#' PPmethod = 'PDA', strata = TRUE)
#' pr <- predict( object = pprf.iris, test.data = pprf.iris$test)
#' pr
predict.PPforest <- function( object, ...){
  . <- NULL

votes <- object[[8]] %>% 
              dplyr::do(tr = PPtreeViz::PP.classify( test.data = object$test, Tree.result = .$tr, Rule = 1)) 
    
  out <- votes %>%  dplyr::do(pred = .$tr[[2]] )
  
  vote.mat <- matrix(unlist(out$pred), ncol = length(votes[[1]][[1]][[2]]), byrow = T)
  max.vote <- apply(vote.mat, 2, function(x) {
    t1 <- table(x)
    names(t1)[which.max(t1)]
  }
  )

 max.vote
}
