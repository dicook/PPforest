#' List with bagging PP.tree error and bagging classification.
#'
#' @param data is a data frame with the data to classify. Class factor in the first column
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @return list with the error, predicted classes and confusion matrix.
#' @export
#' @examples
#' data1<-iris[,5:1]
#' output<-bootstrap_pp(data1,training,ntree=50,index="LDA")     
#' bagging_pp(data1, output)
bagging_pp <- function(data, boot, ...){
  votes.tr <- ldply(boot[[1]], function(x) PP.classify(test.data=data[,-1],
                                true.class=data[,1], x[[2]], Rule=1)$predict.class)
  error.tr <- ldply(boot[[1]], function(x) PP.classify(test.data=data[,-1],
                                true.class=data[,1], x[[2]], Rule=1)$predict.error)
  max.vote <- apply(votes.tr[,-1], 2, function(x) {
        t1 <- table(x)
    names(t1)[which.max(t1)]
    }
  )
 
            tab.t <- table(Observed=data[,1],Predicted=max.vote)
  colnames(tab.t) <- rownames(tab.t)
            tab.p <- round(prop.table(tab.t,1),7)
            error <- round((dim(data)[1]-sum(diag(tab.t)))/dim(data)[1],5)
 return(list(error, as.numeric(max.vote),tab.p))
} 
