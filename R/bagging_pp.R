#' List with bagging PP.tree error and bagging classification.
#'
#' @param data is a data frame with the data to classify. Class factor in the first column
#' @param scale is to standardize the data set, by default it is TRUE.
#' @param strata is to stratify the bootstrap samples, by default is TRUE.
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @param training indices of the training data set.
#' @param ntree number of bootstrap samples or individual classifiers (trees).
#' @return list with the error, predicted classes and confusion matrix.
#' @export
#' @examples
#' data1<-iris[,5:1]
#' training<-train_fn(iris[,5],.9)
#' output<-bootstrap_pp(data1,scale=TRUE,size.p=.9,training,strata=TRUE,ntree=50,index="LDA")      
#' b.pp <- bagging_pp(data1,scale=TRUE, strata=TRUE,output,training,ntree=50)
bagging_pp <- function(data, scale=TRUE,strata=TRUE,boot,training,ntree, ...){
  if(strata==TRUE) data[,-1] <- scale(data[,-1])
  votes <- plyr::ldply(boot[[1]], function(x) PPtree::PP.classify(test.data=data[,-1],
                                true.class=data[,1], x[[2]], Rule=1)$predict.class)[,-1]
  
  max.vote <- apply(votes, 2, function(x) {
        t1 <- table(x)
    names(t1)[which.max(t1)]
    }
  )

  
  pos <- expand.grid(a=1:dim(data)[1], b=1:dim(data)[1])
  cond <- pos[,1]>= pos[,2]
  tri.low <- pos[cond,]
  same.node <- data.frame(tri.low,dif=apply(t(votes),2,function(x) x[ tri.low[,1]]-x[ tri.low[,2]]))
  proximity <- data.frame(same.node[,c(1:2)],proxi=apply(same.node[,-c(1:2)],1,function(x) sum(x==0))/ntree)
  
  
 votes.con <- apply(votes,2,table)
 votes.prop <- ldply(votes.con,function(x) as.data.frame(x/dim(votes)[1]))
    


  v.train <- votes[,training]
  l.train <- 1:length(training)
  oob.var <- plyr::ldply(boot[[1]],function(x) data.frame(t(as.numeric(!l.train%in%x[[1]]))))[,-1]
  colnames(oob.var) <- l.train
  mv.train <- reshape2::melt(v.train)
  moob.var <- reshape2::melt(oob.var)
 oob.votes <-mv.train[(moob.var$value)==1,]
 
   max.oob <- plyr::ddply(oob.votes,plyr::.(variable),function(x){
        t1 <- table(x$value)
    names(t1)[which.max(t1)]
    }
  )

 
  oob.tree.aux <- v.train*oob.var
 oob.error.tree <- plyr::adply(oob.tree.aux,1,function(x) {
   nam <- colnames(x)
  colnames(x) <- l.train
  oob.nam <- as.numeric(colnames(x)[x!=0])
  oob.obs <- as.numeric(data[training[oob.nam],1])
  oob.pred <- as.numeric(x[1,x!=0])
  1-sum(diag(table(oob.obs, oob.pred )))/length(oob.pred)
    }   
  )
 
 tr.class <- data.frame(variable=paste(variable="V",training,sep=""),V1=as.numeric(data[training,1]))
    cond <- tr.class[max.oob[,1],]
 tab.oob <-table(Observed=cond[,2],Predicted=as.numeric(max.oob[,2]))
oob.error <- 1-sum(diag(tab.oob))/length(cond[,1])
 

            tab.t <- table(Observed=data[training,1],Predicted=max.vote[training])
  colnames(tab.t) <- rownames(tab.t)
 class.error <- 1-diag(tab.t)/((addmargins(tab.t,2))[,"Sum"])
            tab.p <- cbind(round(prop.table(tab.t,1),7),class.error)
            error <- round((dim(data[training,])[1]-sum(diag(tab.t)))/dim(data)[1],5)
out <- list(oob.error,error,as.numeric(max.vote),tab.p,oob.error.tree$V1,votes,votes.prop,proximity)
names(out) <-c("OOB estimate or error rate","Training error","Predicted","Confusion matrix","OOB error Tree","Votes","Votes proportion","Proximity")
return(out)
} 
