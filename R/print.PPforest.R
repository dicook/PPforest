#' Print PPforest object
#'
#' @param ppf is a PPforest class object
#' @param ... additional parameter
#' @return printed results for PPforest object
#' @export
#' 
print.PPforest <- function(ppf, ...) {
    cat("\nCall:\n", deparse(ppf$call), "\n")
    cat("               Type of random forest: ", ppf$type, "\n", sep = "")
    cat("                     Number of trees: ", ppf$n.tree, "\n", sep = "")
    cat("No. of variables tried at each split: ", ppf$n.var, "\n\n", sep = "")
    
    cat("        OOB estimate of  error rate: ", round(ppf$oob.error.forest * 100, digits = 2), "%\n", sep = "")
    cat("Confusion matrix:\n")
    print(x$confusion)
    
} 
