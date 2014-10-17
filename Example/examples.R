
#example using Iris data
library(ggplot2)
data.iris <- iris[,5:1]
training<- train_fn(iris[,5],.9)

result.boot.st <- mlply(data.frame(nt=c(1,10,50,100,500)), function(nt) {
  bootstrap_pp(data=data.iris,training=training,ntree=nt,index="LDA") 
   }  
)  

result.boot <- mlply(data.frame(nt=c(1,10,50,100,500)), function(nt) {
  bootstrap_pp(data=data.iris,training=training,ntree=nt,strata=FALSE,index="LDA") 
  
}  
)  


result.bagg.st <- llply(result.boot.st[1:5],bagging_pp,data=data.iris,result=result.boot.st,training=training)
result.bagg <- llply(result.boot[1:5],bagging_pp,data=data.iris,result=result.boot,training=training)

error <- sapply(result.bagg,function(x) x[[1]][1] )
error.st <- sapply(result.bagg.st,function(x) x[[1]][1] )

aux <- c(1,10,50,100,500)
cbind(aux,error)
cbind(aux,error.st)


rf.iris<-randomForest(Species ~ ., data=iris[training,], importance=TRUE,
                     proximity=TRUE,strata=Specie)

 #Example olive data
 setwd("./Data")
 load("olive.rda")
 head(d.olive)
library(ggplot2)
d.olive2 <- subset(d.olive,Region%in%c(1,2))[,-2]
d.olive2$Region <- as.factor(d.olive2$Region)


training <- train_fn(d.olive2[,1],.9)



result.boot2.st <- mlply(data.frame(nt=c(1,10,50,100,500)), function(nt) {
  bootstrap_pp(data=d.olive2,training=training,ntree=nt,index="LDA") 
}  
)  

result.boot2 <- mlply(data.frame(nt=c(1,10,50,100,500)), function(nt) {
  bootstrap_pp(data=d.olive2,training=training,strata=FALSE,ntree=nt,index="LDA") 
}  
)  

result.bagg2.st <- llply(result.boot2.st[1:5],bagging_pp,data=d.olive2,result=result.boot2.st,training=training)
result.bagg2 <- llply(result.boot2[1:5],bagging_pp,data=d.olive2,result=result.boot2,training=training,strata=FALSE)

error <- sapply(result.bagg2,function(x) x[[1]][1] )
error.st <- sapply(result.bagg2.st,function(x) x[[1]][1] )

aux <- c(1,10,50,100,500)
cbind(aux,error)
cbind(aux,error.st)


library(randomForest)

rf <- randomForest(Region ~ ., data=d.olive2[training,], importance=TRUE,strata=Region,
             proximity=TRUE)



      

