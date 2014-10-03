#' List of PP.Tree objects for bootstrap samples.
#'
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param aux indices of the training data set
#' @param ntree number of bootstrap samples or individual classifiers (trees).
#' @param size.p proportion of sample in each class group.
#' @param index PP index to run the PP.Tree, options LDA, Lp or PDA.
#' @return list with the PP.Tree output for all the bootstraps samples and the index for the initial 
#'   training set.
#' @export
#' @examples
#' data<-iris[,5:1]
#' output<-bootstrap_pp(data,ntree=100,size.p=.9,index="LDA")  
bootstrap_pp <- function(data, aux, ntree, size.p, index='LDA', ...){
  out <- mlply(data.frame(tr=1:ntree), function(tr) {
    boot <- sort(sample(aux, replace = TRUE))
    pp.tree <- PP.Tree(PPmethod=index, i.data=data[boot,-1], i.class=data[boot,1], ...) 
    list(boot, pp.tree)
  })
  return(list(out, aux))        
}


