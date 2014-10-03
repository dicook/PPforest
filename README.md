PPforest
========
This is going to be an R package that extends PPtree to incorporate ideas of random forests 
=======

Sample index function
--------

```{r} 
#' Index id for training set.
#'
#' @param i.class input character vector with class of the data
#' @param size.p proportion of sample in each class group
#' @return numeric vector giving number with the sample indexes for training
#' @examples
#' train_fn(iris[,5],.9)
train_fn <- function(class,size.p){
  n <- length(class)
  class.id <- data.frame(id=1:n,class=class)
  index  <- unlist(dlply(class.id,.(class),function(x) sample(x$id,round(length(x$class)*size.p,1))))
  names(index) <- NULL
  return(sort(index))
}

```




Bootstrap function
--------
```{r}
#' List of PP.Tree objets for bootstrap samples.
#'
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param ntree number of bootstrap samples or individual classifiers (trees).
#' @param size.p proportion of sample in each class group.
#' @param index PP index to run the PP.Tree, options LDA, Lp or PDA.
#' @return list with the PP.Tree output for all the bootstraps samples and the index for the initial 
#'   training set.
#' @examples
#' data<-iris[,5:1]
#' output<-bootstrap_pp(data,ntree=100,size.p=.9,index="LDA")  
bootstrap_pp <- function(data,ntree,size.p,index='LDA', ...){
  aux <- train_fn(data[,1],size.p)
  out <- mlply(data.frame(tr=1:ntree), function(tr) {
    boot <- sort(sample(aux, replace = TRUE))
    pp.tree<-PP.Tree(PPmethod=index,i.data=data[boot,-1],i.class=data[boot,1],...) 
    list(boot,pp.tree)
  })
  return(list(out,aux))        
}
```

Bagging function
---------

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
