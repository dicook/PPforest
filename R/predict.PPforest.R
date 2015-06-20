#' Obtain predicted class for new data using PPforest 
#' 
#' Vector with predicted values from a PPforest.
#' @param object an object of class PPforest
#' @param test.data the test dataset
#' @param Rule split rule \itemize{ \item{1: mean of two group means} \item{ 2: weighted mean of two group means - weight with group size} \item{3: weighted mean of two group means - weight with group sd} \item{ 4: weighted mean of two group means - weight with group se} \item{5: mean of two group medians} \item{ 6: weighted mean of two group medians - weight with group size} \item{ 7: weighted mean of two group median - weight with group IQR} \item{8: weighted mean of two group median - weight with group IQR and size} }
#' @param true.class true class of test dataset if available
#' @param ... arguments to be passed to methods
#' @return predicted values form PPforest  
#' @export
#' @importFrom magrittr %>%
#' @examples
#' pprf.iris <- PPforest(data = iris[,5:1], size.tr = .9, m = 500, size.p = .9, 
#' PPmethod = 'PDA', strata = TRUE)
#' pr <- predict( object = pprf.iris, test.data = pprf.iris$test)
#' pr
predict.PPforest <- function(object, test.data, Rule, true.class = NULL, ...) {
    . <- NULL
    
    votes <- object[[8]] %>% dplyr::do(tr = PPtreeViz::PP.classify(test.data = test.data, Tree.result = .$tr, Rule = 1, 
        ...))
    
    out <- votes %>% dplyr::do(pred = .$tr[[2]])
    
    vote.mat <- matrix(unlist(out$pred), ncol = length(votes[[1]][[1]][[2]]), byrow = T)
    max.vote <- apply(vote.mat, 2, function(x) {
        t1 <- table(x)
        names(t1)[which.max(t1)]
    })
    
    max.vote
} 
