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
#' training <- train_fn(iris[,5], .9)
#' data1 <- iris[training$id, 5:1]
#' iris.b <- bootstrap(data1, 500) 
#' output1 <- trees_pp(iris.b, size.p=.9,  PPmethod = "LDA") 
#' output2 <- trees_pp(iris.b, size.p=.9, PPmethod ='PDA', lambda=.1) 
#' tr.index2 <- train_fn(NCI60[,1], 2/3)
#' dat.b<-  bootstrap(NCI60[tr.index2$id,], 50) 
#'op <- trees_pp(dat.b, size.p=.9,  PPmethod = "LDA", lambda=.1) 
#'ppfr2 <- PPforest( train = leukemia [tr.index2$id,], testap = TRUE, test = leukemia[-tr.index2$id,], m = 50, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' pr <- forest_ppred( parkinson[-tr.index$id, -1] , op)
trees_pp <- function(data.b, size.p = 0.9, PPmethod = "LDA", lambda=.1, ...) {
    names(data.b)[1] <- "class"
    if (PPmethod == "LDA") {
        trees <- data.b %>% dplyr::do(tr = PPtree_split(as.formula("class~."), PPmethod = "LDA", data = ., size.p = size.p,...))
        
    } else {
        trees <- data.b %>% dplyr::do(tr = PPtree_split(as.formula("class~."), PPmethod = "PDA", data = ., size.p = size.p, lambda, 
            ...) )
    }
    trees
}






 
