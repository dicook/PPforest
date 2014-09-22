boots.samp<-function(i.class, i.data,ntree=50,m.index,PPmethod='LDA', weight = TRUE, r = NULL,  
                     lambda, cooling = 0.999, temp = 1, energy = 0.01,  ...)
{
  n<-length(i.data[, 1])
  trees<-list()
  replicate<-array(0,c(n,ntree))
  for (i in 1:ntree) {
    bootstrap  <- sample(1:n, replace = TRUE)
    if(m.index=="PP.Tree"){
      Tree.result<- PP.Tree(PPmethod, i.class, i.data=i.data[bootstrap,], weight = TRUE, r = NULL,lambda = NULL, cooling = 0.999, temp = 1, energy = 0.01)
      trees[[i]]<-Tree.result
      replicate<-bootstrap
    } 
  }
  
  if(m.index=="LDA.Tree"){
    Tree.result<- LDA.Tree(i.class, i.data=i.data[bootstrap,], weight)
    trees[[i]]<-Tree.result
    replicate<-bootstrap 
  }
  if(m.index=="PDA.Tree"){
    Tree.result<- PDA.Tree(i.class, i.data=i.data[bootstrap,],lambda)
    trees[[i]]<-Tree.result
    replicate<-bootstrap 
  }
  return(trees)  
}

