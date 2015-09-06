#' Obtain predicted class for new data using PPforest 
#' 
#' Vector with predicted values from a PPforest.
#' @param xnew data frame with explicative variables used to get new predicted values.
#' @param output.tree trees classifiers from trees_pp function or PPforest object
#' @param ... arguments to be passed to methods
#' @return predicted values form PPforest  
#' @export
#' @importFrom magrittr %>%
#' @examples 
#' training.id <- train_fn(y = leukemia[,1], size.p = 0.9)
#' leukemia.b <- ppf_bootstrap(y = leukemia[, 1], df = leukemia, m= 200) 
#' leukemia.trees <- trees_pp(data.b = leukemia.b, size.p = .9, PPmethod = 'PDA', lambda = .1)
#' pr <- tree_ppred( xnew = leukemia[-training.id$id, -1] , leukemia.trees)
#' pr
tree_ppred <- function(xnew, output.tree, ...) {
    . <- NULL
    
    votes <- output.tree %>% dplyr::do(tr = PPtreeViz::PP.classify(test.data = xnew, Tree.result = .$tr, Rule = 1))
    
    out <- votes %>% dplyr::do(pred = .$tr[[2]])
    
    vote.mat <- matrix(unlist(out$pred), ncol = dim(xnew)[[1]], byrow = T)
    
    
    max.vote <- apply(vote.mat, 2, function(x) {
        t1 <- table(x)
        names(t1)[which.max(t1)]
    })
    
    
    return(list(out, vote.mat, max.vote))
} 
