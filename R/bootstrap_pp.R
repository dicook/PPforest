#' List of PP.Tree objects for bootstrap samples.
#'
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param scale is to standardize the data set, by default it is TRUE.
#' @param training indices of the training data set
#' @param ntree number of bootstrap samples or individual classifiers (trees).
#' @param index PP index to run the PP.Tree, options LDA, Lp or PDA, by default it is LDA.
#' @return list with the PP.Tree output for all the bootstraps samples and the index for the initial 
#'   training set.
#' @export
#' @examples
#' training<-train_fn(iris[,5],.9)
#' data1<-iris[,5:1]
#' output<-bootstrap_pp(data1,scale=TRUE,size.p=.9,training=NULL,strata=FALSE,ntree=50,index="LDA")  
 bootstrap_pp <- function(data,scale=TRUE,size.p=.9,training=NULL, strata=TRUE,ntree, index='LDA', ...){
   if(scale==TRUE) data[,-1] <- scale(data[,-1])
   names(data)[1] <-"class"
   if(is.null(training)) training <- 1:dim(data)[1]
  
   out <- plyr::mlply(data.frame(tr=1:ntree), function(tr) {
     n <- length(training)
    class.id <- data.frame(id=1:n,class=data[training,"class"])
    dat.train <- data[training,]
    
    if(strata==TRUE){
    index.boot <- unlist(plyr::dlply(class.id, plyr::.(class), function(x) sort(sample(x$id, replace=TRUE)) ))
    names(index.boot) <- NULL 
    pp.tree <-PPtree_split(PPmethod=index, size.p=.9,i.data=dat.train[index.boot,-1], i.class=dat.train[index.boot,1]) 
    list(index.boot, pp.tree)
    }
   else{
     check<-TRUE
     while(check){
    index.boot <- sort(sample(class.id$id, replace = TRUE))
    aux <-table(dat.train[index.boot,1],index.boot )
    check <- !sum((apply(aux,1,function(x) ((sum(x==0))<dim(aux)[2]-2)&sum(x)>2)))==length(unique(data[,1]))
     }
      pp.tree <- PPtree_split(PPmethod=index,size.p=.9, i.data=dat.train[index.boot,-1], i.class=dat.train[index.boot,1],...) 
    list(index.boot, pp.tree)
    }
  
      } 
    )
  
  return(list(trees=out, dat.train=training))        
}

 
  






