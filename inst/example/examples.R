
#example using Iris data
library(ggplot2)
data.iris <- iris[,5:1]
training<- train_fn(iris[,5],.9)

result.boot.st <- mlply(data.frame(nt=c(5,10,50,100,500)), function(nt) {
  bootstrap_pp(data=data.iris,scale=TRUE,training=training,ntree=nt,index="LDA") 
   }  
)  

result.boot <- mlply(data.frame(nt=c(5,10,50,100,500)), function(nt) {
  bootstrap_pp(data=data.iris,scale=TRUE,training=training,ntree=nt,strata=FALSE,index="LDA") 
  
}  
)  

Tree.result <- PP.Tree("LDA",iris[training,5],iris[training,1:4])
PP.classify(iris[training,1:4],iris[training,5],Tree.result,1)

result.bagg.st <- llply(result.boot.st[1:5],bagging_pp,data=data.iris,result=result.boot.st,training=training)
result.bagg <- llply(result.boot[1:5],bagging_pp,data=data.iris,result=result.boot,training=training)

error <- sapply(result.bagg,function(x) x[[1]][1] )
error.st <- sapply(result.bagg.st,function(x) x[[1]][1] )

aux <- c(5,10,50,100,500)
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
d.olive2$Region <- factor(d.olive2$Region)



training <- train_fn(d.olive2[,1],.9)

Tree.result <- PP.Tree("PDA",d.olive2[training,1],d.olive2[training,2:9],lambda=1)

PP.classify(d.olive2[training,2:9],d.olive2[training,1],Tree.result,1)


result.boot2.st <- mlply(data.frame(nt=c(5,10,50,100,500)), function(nt) {
  bootstrap_pp(data=d.olive2,scale=TRUE,training=training,ntree=nt,index="LDA") 
}  
)  

result.boot2 <- mlply(data.frame(nt=c(5,10,50,100,500)), function(nt) {
  bootstrap_pp(data=d.olive2,scale=TRUE,training=training,strata=FALSE,ntree=nt,index="LDA") 
}  
)  

result.bagg2.st <- llply(result.boot2.st[1:5],bagging_pp,data=d.olive2,result=result.boot2.st,training=training)
result.bagg2 <- llply(result.boot2[1:5],bagging_pp,data=d.olive2,result=result.boot2,training=training,strata=FALSE)

error <- sapply(result.bagg2,function(x) x[[1]][1] )
error.st <- sapply(result.bagg2.st,function(x) x[[1]][1] )

aux <- c(5,10,50,100,500)
cbind(aux,error)
cbind(aux,error.st)


library(randomForest)

rf <- randomForest(Region ~ ., data=d.olive2[training,], importance=TRUE,strata=Region,
             proximity=TRUE)

rf 

######
library(mvtnorm)
library(ggplot2)

sigma.x <- matrix(c(4,3,3,4), ncol=2)
sigma.y <- matrix(c(4,3,3,4), ncol=2)

x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma.x, method="chol")
y <- rmvnorm(n=500, mean=c(5,16), sigma=sigma.y, method="chol")
auxx<-rep("x",dim(x)[1])
auxy<-rep("y",dim(x)[1])

dat.pl<-rbind(data.frame(aux=auxx,x=x),data.frame(aux=auxy,x=y))

qplot(x=x.1,y=x.2,data=dat.pl,col=aux)


#RF
rf <- randomForest(aux ~ ., data=dat.pl, importance=TRUE,
                   proximity=TRUE)

#PPtree
Tree.result <- PP.Tree("LDA",dat.pl[,1],dat.pl[,-1])
Tree.result

PP.classify(dat.pl[,-1],dat.pl[,1],Tree.result,1)

##PPbagging
training<-train_fn(dat.pl[,1],.9)
 output<-bootstrap_pp(dat.pl,scale=TRUE,training,ntree=50,index="LDA")     
 bagging_pp(dat.pl, output,training)


#Crab data
/Users/nataliadasilva/Documents/PP.forest/PP.Forest/data

crab <- read.csv("australian-crabs.csv")

rf <- randomForest(species ~ ., data=crab, importance=TRUE,
                   proximity=TRUE,strata=TRUE)
rf


#PPtree
crab$sp<-factor(paste(crab$species,crab$sex,sep="."))
Tree.result <- PP.Tree("LDA",crab[,"sp"],crab[,-c(1,2,3,9)])
#Tree.result
cl<-PP.classify(crab[,-c(1,2,3,9)],crab[,9],Tree.result,1)
table(crab$sp, cl$predict.class)


library(rpart)

# Fitting the tree
olive.rp <- rpart(Region~., d.olive2, method="class")
olive.rp

