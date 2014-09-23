sample.tr<-function(data,ntree){
  n<-length(data[, 1])
  sam.id<-rdply(ntree,sample(1:n, replace = TRUE) )[,-1]
  cbind(tr=rep(1:ntree,each=n),data[as.numeric(data.matrix(sam.id)),] )  
}


boots.samp<-function(i.class, i.data,m.index,PPmethod='LDA', weight = TRUE, r = NULL,  
                     lambda, cooling = 0.999, temp = 1, energy = 0.01,  ...)
{
  n<-length(i.data[, 1])
  if(m.index=="PP.Tree"){
    out<- PP.Tree(PPmethod, i.class, i.data=i.data, weight = TRUE, r = NULL,lambda = NULL, cooling = 0.999, temp = 1, energy = 0.01)
    
  }
  if(m.index=="LDA.Tree"){
    out<-  LDA.Tree(i.class, i.data=i.data)
    
  }
  if(m.index=="PDA.Tree"){
    out<-PDA.Tree(i.class, i.data=i.data,lambda)
    
  }
  return(out) 
}


bagg.pp<-function(data,ntree,m.index,PPmethod='LDA', weight = TRUE, r = NULL,  
                  lambda, cooling = 0.999, temp = 1, energy = 0.01,  ...){
  
  aux<-sample.tr(data,ntree)
  dlply(aux,.(tr),function(x) boots.samp(i.class=x[,2],i.data=x[,-c(1,2)],m.index))
}


  