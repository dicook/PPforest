#' List with bagging PP.tree error and bagging classification.
#'
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @return list with the error and predicted classes.
#' @examples
#' data<-iris[,5:1]
#' output<-bootstrap_pp2(data,ntree=100,size.p=.9,index="LDA")  
#' bagging_pp(data,o)
bagging_pp<-function(data,boot,...){
  votes.tr <- ldply(boot[[1]],function(x) PP.classify(test.data=data[x[[1]],-1],true.class=data[x[[1]],1],x[[2]],Rule=1)$predict.class)
  error.tr <- ldply(boot[[1]],function(x) PP.classify(test.data=data[x[[1]],-1],true.class=data[x[[1]],1],x[[2]],Rule=1)$predict.error)
  max.vote <-apply(votes.tr[,-1],2,function(x) {
    t1<-table(x)
    names(t1)[which.max(t1)]
  }
  )
  error<-sum(as.numeric(max.vote)!=as.numeric(data[boot[[2]],1]))/length(boot[[2]])
  return(list(error,as.numeric(max.vote)))
} 
