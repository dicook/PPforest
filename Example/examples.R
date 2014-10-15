
#example using Iris data

data.iris <- iris[,5:1]
training <- train_fn(iris[,5],.9)

qplot(x=Sepal.Length,y=Sepal.Width,data=iris,color=Species,size=3)
qplot(x=Petal.Length,y=Petal.Width,data=iris,color=Species,size=3)
qplot(x=Sepal.Width,y=Petal.Width,data=iris,color=Species,size=3)


result.boot <- mlply(data.frame(ntree=c(1,10,50,100,500)), function(ntree) {
  bootstrap_pp(data.iris,training,ntree,index="LDA") 
  
 }  
)  


result.bagg <- llply(result.boot[1:5],bagging_pp,data=data.iris,result=result.boot)

error <- sapply(result.bagg,function(x) x[[1]][1] )
aux <- c(1,10,50,100,500)
cbind(aux,error)

 
 #Example olive data
 setwd("./Data")
 load("olive.rda")
 head(d.olive)
library(ggplot2)
d.olive2 <- subset(d.olive,Region%in%c(1,2))[,-2]
d.olive2$Region <- as.factor(d.olive2$Region)

qplot(x=palmitic,y=oleic,data=d.olive2,color=Region)
qplot(x=stearic,y=oleic,data=d.olive2,color=Region)
qplot(x=linoleic,y=oleic,data=d.olive2,color=Region)
qplot(x=arachidic,y=oleic,data=d.olive2,color=Region)

training <- train_fn(d.olive2[,1],.9)



result.boot2 <- mlply(data.frame(ntree=c(1,10,50,100,500)), function(ntree) {
  bootstrap_pp(d.olive2,training,ntree,index="LDA") 
}  
)  


result.bagg2 <- llply(result.boot2[1:5],bagging_pp,data=d.olive2,result=result.boot2)

error <- sapply(result.bagg2,function(x) x[[1]][1] )
aux <- c(1,10,50,100,500)
cbind(aux,error)






output<-bootstrap_pp(d.olive2,training,ntree=1,index="LDA")  
bagging_pp(d.olive2, output)


      
      training<-train_fn(d.olive2[,1],.9)
      
      result.boot<-mlply(data.frame(ntree=c(1,10,50,100,500)), function(ntree) {
        bootstrap_pp(d.olive2,training,ntree,index="LDA")
        
      }
      )    
      
      
      result.boot.PDA<-mlply(data.frame(ntree=c(1,10,50,100,500)), function(ntree) {
        bootstrap_pp(data1,training,ntree,index="PDA",lambda=1)
        
      }
      ) 
      
      result.bagg<-llply(result.boot[1:5],bagging_pp,data=d.olive2,result=result.boot)
      result.bagg.PDA<-llply(result.boot.PDA[1:5],bagging_pp,data=data,result=result.boot)
      
      error<-sapply(result.bagg,function(x) x[[1]][1] )
      error.PDA<-sapply(result.bagg.PDA,function(x) x[[1]][1] )
      
   

