#' List of PP.Tree objets for bootstrap sapmles.
#'
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param ntree number of bootstrap samples or individual classifiers (trees).
#' @param size.p proportion of sample in each class group.
#' @param index PP index to run the PP.Tree, options LDA, Lp or PDA.
#' @return list with the PP.Tree output for all the bootstraps samples.
#' @examples
#' data<-iris[,5:1]
#' output<-bootstrap_pp2(data,ntree=10,size.p=.9,index="LDA")  


bootstrap_pp1<-function(data,ntree,size.p,index='LDA', ...){
  aux<-train_fn(data[,1],size.p)
  bootstrap.id<-rdply(ntree,sample(aux, replace = TRUE) )[,-1]
  train.boot<-cbind(tr=rep(1:ntree,each=length(aux)),data[as.numeric(data.matrix(bootstrap.id)),] ) 
  out<-dlply(train.boot,.(tr),function(x) PP.Tree(PPmethod=index,i.data=x[,-c(1,2)],i.class=x[,2],...) )
  
}

bootstrap_pp2<-function(data,ntree,size.p,index='LDA', ...){
  aux<-train_fn(data[,1],size.p)
  out<-mlply(data.frame(tr=1:ntree), function(tr) {
    boot<- sample(aux, replace = TRUE)
    PP.Tree(PPmethod=index,i.data=data[boot,-1],i.class=data[boot,1],...) 
  })
  return(out)        
}