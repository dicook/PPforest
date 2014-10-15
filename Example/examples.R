
#example using Iris data

data.iris<-iris[,5:1]
training<-train_fn(iris[,5],.9)

result.boot<-mlply(data.frame(ntree=c(1,10,50,100,500)), function(ntree) {
  bootstrap_pp(data.iris,training,ntree,index="LDA") 
} )  


result.bagg<-llply(result.boot[1:5],bagging_pp,data=data.iris,result=result.boot)

error<-sapply(result.bagg,function(x) x[[1]][1] )
aux<-c(1,10,50,100,500)
(cbind(aux,error)

 
 #Example olive data
 setwd("./Data")
 load("olive.rda")
head(d.olive)

