#'   Projection persuit trees for bootstrap samples.
#'
#' @param data.b are the bootstrap samples. Class factor in the first column
#' @param size.p proportion of random sample variables in each split.
#' @param PPmethod to run the PPtree_plit function, options LDA or PDA, by default it is LDA.
#' @param lambda a parameter for PDA index
#' @param ... arguments to be passed to methods
#' @return data frame with tree_pp output for all the bootstraps samples.
#' @export
#' @examples
#' training <- train_fn2(iris[,5], .9)
#' data1 <- iris[training$id, 5:1]
#' iris.b <- bootstrap(data1, 500) 
#' output1 <- trees_pp(iris.b, size.p=.9,  PPmethod = "LDA") 
#' output2 <- trees_pp(iris.b, size.p=.9, PPmethod ='PDA', lambda=.1) 
#' 
trees_pp <- function(data.b, size.p = 0.9, PPmethod = "LDA", lambda, ...) {
    names(data.b)[1] <- "class"
    if (PPmethod == "LDA") {
        trees <- data.b %>% dplyr::do(tr = PPtree_split(as.formula("class~."), PPmethod = "LDA", data = ., size.p = size.p,...))
        
    } else {
        trees <- data.b %>% dplyr::do(tr = PPtree_split(as.formula("class~."), PPmethod = "PDA", data = ., size.p = size.p, lambda, 
            ...) )
    }
    trees
}






 
