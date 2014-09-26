sample.train<-function(data,ntree,size.p){
  n<-length(data[,1])
  nt<-round(n*size.p)
  samp<-sample(1:nt)
  train<-data[samp,]
  test<-data[-samp,]
  sam.id<-rdply(ntree,sample(1:nt, replace = TRUE) )[,-1]
  train.boot<-cbind(tr=rep(1:ntree,each=nt),train[as.numeric(data.matrix(sam.id)),] )  
  return(list(test,train.boot))
}


sample.var<-function(data,ntree,nodes,size){
  n<-length(data[1,-1])
  sam.id<-rdply(ntree*nodes,sample(1:size) )[,-1]
  cbind(tr=rep(1:ntree*nodes,each=nodes),data[as.numeric(data.matrix(sam.id)),] )  
}



pps.index<-function(data,test,i.class, i.data,m.index,PPmethod='LDA', weight = TRUE, r = NULL,  
                    lambda, cooling = 0.999, temp = 1, energy = 0.01,  ...)
{
  n<-length(i.data[, 1])
  if(m.index=="PP.Tree"){
    out<- PP.Tree(PPmethod, i.class, i.data, weight = TRUE, r = NULL,lambda = NULL, cooling = 0.999, temp = 1, energy = 0.01)
    pred<- PP.classify(test[,-1], test[,1],out, Rule=1)
  }
  if(m.index=="LDA.Tree"){
    out<-  LDA.Tree(i.class, i.data)
    pred<- PP.classify(test[,-1], test[,1],out, Rule=1)
  }
  if(m.index=="PDA.Tree"){
    out<-PDA.Tree(i.class, i.data,lambda)
    pred<- PP.classify(test[,-1], test[,1],out, Rule=1)
  }
  return(c(out,pred)) 
}


bagg.pp<-function(data,ntree,m.index,size.p,PPmethod='LDA', weight = TRUE, r = NULL,  
                  lambda, cooling = 0.999, temp = 1, energy = 0.01,  ...){
  aux<-sample.train(data,ntree,size.p)
  out<- dlply(aux[[2]],.(tr),function(x) pps.index(data,test=aux[[1]],i.class=x[,2],i.data=x[,-c(1,2)],m.index))
}



class.bagg<-function(ntree,out.pp){
  cl<-out.pp[[1]][[5]] 
  for(i in 2:ntree){
    cl<-rbind(cl,out.pp[[i]][[5]] )
  }
  aux<-unlist(apply(cl,2,table))
  apply(aux,2,which.max)   
}