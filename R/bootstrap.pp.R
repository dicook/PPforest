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
#' output<-bootstrap_pp(data,aux,ntree=50,index="LDA")  
bootstrap_pp <- function(data, aux, ntree, index='LDA', ...){
  names(data)[1]<-"class"
  out <- mlply(data.frame(tr=1:ntree), function(tr) {
    n <- length(aux)
    class.id <- data.frame(id=1:n,class=data[aux,"class"])
    index.boot  <- unlist(dlply(class.id, .(class), function(x) 
      sample(x$id, replace=TRUE)))
    names(index.boot) <- NULL
    index.boot<-sort(index.boot)
    pp.tree <- PP.Tree(PPmethod=index, i.data=data[index.boot,-1], i.class=data[index.boot,1], ...) 
    list(index.boot, pp.tree)
  })
  return(list(out, aux))        
}


