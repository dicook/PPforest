#' List with bagging PP.tree error and bagging classification.
#'
#' @param data is a data frame with the data to classify. Class factor in the first column
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @return list with the error, predicted classes and confusion matrix.
#' @export
#' @examples
#' data1<-iris[,5:1]
#' output<-bootstrap_pp(data1,training,ntree=50,index="LDA")     
#' bagging_pp(data1, output,training)
bagging_pp <- function(data, boot,training, ...){
  votes <- ldply(boot[[1]], function(x) PP.classify(test.data=data[,-1],
                                true.class=data[,1], x[[2]], Rule=1)$predict.class)[,-1]
  
  max.vote <- apply(votes, 2, function(x) {
        t1 <- table(x)
    names(t1)[which.max(t1)]
    }
  )
 
  v.train <- votes[,training]#only training votes
  l.train <- 1:length(training)
  oob.var <- ldply(boot[[1]],function(x) data.frame(t(!l.train%in%x[[1]])))[,-1]
  
  mv.train <- melt(v.train)
  moob.var <- melt(oob.var)
 oob.votes <-mv.train[moob.var$value,]
   max.oob <- ddply(oob.votes,.(variable),function(x){
        t1 <- table(x$value)
    names(t1)[which.max(t1)]
    }
  )

 tr.class <- data.frame(variable=paste(variable="V",training,sep=""),V1=as.numeric(data[training,1]))
 cond <- tr.class[max.oob[,1],]
   oob.error <- (dim(cond)[1]-sum(cond[,2]==as.numeric(max.oob[,2])))/dim(cond)[1]
  
            tab.t <- table(Observed=data[,1],Predicted=max.vote)
  colnames(tab.t) <- rownames(tab.t)
 class.error <- 1-diag(tab.t)/((addmargins(tab.t,2))[,"Sum"])
            tab.p <- cbind(round(prop.table(tab.t,1),7),class.error)
            error <- round((dim(data)[1]-sum(diag(tab.t)))/dim(data)[1],5)
 return(list(oob.error,error, as.numeric(max.vote),tab.p))
} 
