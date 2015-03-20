#' Vector with predicted values form a PPforest.
#'
#' @param data are the data without class we want to predict
#' @param output.tree trees classifiers from trees_pp function
#' @return predicted values form PPforest  
#' @export
#' @examples
#' training<-train_fn(iris[,5],.9)
#' data1<-iris[training,5:1]
#' iris.b <- bootstrap(data1, 5) 
#' output <- trees_pp(iris.b,scale=TRUE,size.p=.9,index="LDA") 
#' forest_ppred(iris[-training,2:4],output)
forest_ppred<- function(data,output.tree, ...){
  if(attributes(output)$scale==TRUE) {
    dat<-scale(dat)
    votes<-  output%>% do(tr = PP.classify(test.data=dat,Tree.result=.$tr, true.class=rep(0,nrow(dat), Rule=1)))
  }
  else{
    votes <- output%>% do(tr = PP.classify(test.data=dat,Tree.result=.$tr,true.class=rep(0,nrow(dat), Rule=1)))
  }
  
  out <- votes %>% do(pred = .$tr[[2]] )
  vote.mat <- matrix(unlist(out$pred),ncol=dim(dat)[[1]],byrow=T)
  max.vote <- apply(vote.mat, 2, function(x) {
    t1 <- table(x)
    names(t1)[which.max(t1)]
  }
  )
  return(as.numeric(max.vote))
}
