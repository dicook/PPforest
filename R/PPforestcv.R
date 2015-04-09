#' run a PPforest
#'
#' @param data are the data without class we want to predict
#' @param size.p proportion of sample in each class group
#' @param m number of bootstrap replicate
#' @param index to run the PPtree_plit function, options LDA or PDA, by default it is LDA.
#' @param strata identify if the bootrap samples are stratify by class
#' @param scale is to standardize the data set, by default it is TRUE.
#' @return predicted values, error, bootstrap samples, trees ,training set from PPforest  
#' @export
#' @examples
#'  tr.index <- train_fn2(NCI60[,1],2/3)
#' te.index <- as.vector(1:length(NCI60[,1]))[!(1:length(NCI60[,1])%in%(sort(tr.index$id)))]
#  train <- NCI60[sort(tr.index$id),]
#' te <- NCI60[-tr.index$id,] 
#'ppfr<- PPforestcv(NCI60,m=20,size.p=.9,std=TRUE,index="LDA",strata=TRUE,test=TRUE,te,train,tr.index)
#'ppfr2<- PPforestcv(NCI60,m=20,size.p=.9,std=TRUE,index="PDA",strata=TRUE,test=TRUE,te,train,tr.index,lambda=.14)
#'ppfr[[1]]

PPforestcv <-function(data,m,index,size.p,strata=TRUE,std=TRUE,test=TRUE,te,train,tr.index,lambda){
  colnames(data)[1]<-"class"
  colnames(train)[1]<-"class"
  
  if(strata==TRUE){
    data.b <- bootstrap( train, m,strata) 
    #check
    check <- (data.b %>% do(m=apply(X=.[,-1],2,function(x) tapply(x, .$class,mean,na.rm=T))))$m
    dupli <- (unlist(lapply(check,function(x) sum(apply(x,2,duplicated))!=0)))
    
    check2<- lapply(attributes(data.b)$indices,function(x) {
      aux <- table(data[x+1,1],x+1)
      aux2<-sum((apply(aux,1,function(x) ((sum(x==0))<dim(aux)[2]-2)&sum(x)>2)))==length(unique(data[,1]))
      })
    vargroup <- sum(unlist(check2))
    while((sum(dupli) > 0) ){
      
      data.b2 <- bootstrap( train, sum(dupli)) 
      for(i in 1:sum(dupli)){
        ind <- (1:length(dupli))[dupli]
        attributes(data.b)$indices[ind[i]]<-attributes(data.b2)$indices[i]
      }
      #check
      check <- (data.b %>% do(m=apply(X=.[,-1],2,function(x) tapply(x, .$class,mean,na.rm=T))))$m
      dupli <- (unlist(lapply(check,function(x) sum(apply(x,2,duplicated))!=0)))
      
    }
   while(vargroup>0){
     
     data.b2 <- bootstrap( train, vargroup) 
     for(i in 1:vargroup){
       ind <- (1:length(unlist(check2)))[unlist(check2)]
       attributes(data.b)$indices[ind[i]]<-attributes(data.b2)$indices[i]
     }
     
     check2<- lapply(attributes(data.b)$indices,function(x) {
       aux <- table(data[x+1,1],x+1)
       aux2<-sum((apply(aux,1,function(x) ((sum(x==0))<dim(aux)[2]-2)&sum(x)>2)))==length(unique(data[,1]))
       vargroup<-sum(unlist(check2))
       
       })
    
  }
  output <- trees_pp(data.b,size.p,index,lambda=.14) 
  
}else
  {
    data.b <- bootstrap(train, m,strata=FALSE) 
    #check
    check <- (data.b %>% do(m=apply(X=.[,-1],2,function(x) tapply(x, .$class,mean,na.rm=T))))$m
    dupli <- sum(unlist(lapply(check,function(x) sum(apply(x,2,duplicated))!=0)))
    
    while(sum(dupli) > 0){
      data.b2 <- bootstrap( train, sum(dupli),strata=FALSE) 
      for(i in 1:sum(dupli)){
        ind <- (1:length(dupli))[dupli]
        attributes(data.b)$indices[ind[i]]<-attributes(data.b2)$indices[i]
      }
      #check
      check <- (data.b %>% do(m=apply(X=.[,-1],2,function(x) tapply(x, .$class,mean,na.rm=T))))$m
      dupli <- (unlist(lapply(check,function(x) sum(apply(x,2,duplicated))!=0)))
     
      check2<- lapply(attributes(data.b)$indices,function(x) {
        aux <- table(data[x+1,1],x+1)
        aux2<-sum((apply(aux,1,function(x) ((sum(x==0))<dim(aux)[2]-2)&sum(x)>2)))==length(unique(data[,1]))
      })
    }
    output <- trees_pp(data.b,size.p,index,lambda=.14) 
  }

pred.tr<-forest_ppred(train[,-1] , output)
error.tr <- 1-sum(train[,1]==pred.tr)/length(pred.tr)
if(test==TRUE) {
  pred.te <-forest_ppred(te[,-1] , output)
  error.te <- 1-sum(te[,1]==pred.te)/length(pred.te) 
}else
{
  pred.te<-NULL
  error.te<-NULL
}

return(list(pred.tr,error.tr,pred.te,error.te,data.b,output))

}