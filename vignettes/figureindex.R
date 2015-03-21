library(plyr)

f <- function(d) {
  names(d)[1]<-"Class"
  xb <- apply(d[,-1],2,mean)
  xbi <-ddply(d, .(Class), function(x) {apply(x[,-1],2,mean)} )
  xbi
  bw <- (xbi[,-1]-matrix(xb,ncol=(dim(d)[2]-1),nrow=length(unique(d[,1])),byrow=T))
  n<-table(d[,1])
  aux <-apply(bw,1,function(x) as.numeric(t(x))%o%(as.numeric(x)))*as.numeric(n)
  sbw<-matrix(apply(aux,1,sum),nrow=dim(d[,-1])[2],byrow=T)
  wi<-daply(d,.(Class),function(x) var(x[,-1])*(length(x[,1])-1))
  
  swi<-apply(wi,c(2,3),sum)
  list(swi,sbw)  
}

library(MASS)
library(ggplot2)
bivn <- mvrnorm(50, mu = c(-1, 0.6), Sigma = matrix(c(1, 0.95, 0.95, 1), 2))
bivn2 <- mvrnorm(50, mu = c(1, -0.6), Sigma = matrix(c(1, 0.95, 0.95, 1), 2))

d1 <-data.frame(nam="n1",bivn)
d2 <-data.frame(nam="n2",bivn2)
dat.sim<-rbind(d1,d2)
qplot(data=dat.sim,x=X1,y=X2,shape=nam,ylim=c(-5,5),xlim=c(-5,5))+stat_ellipse()



res<-f(dat.sim)
swi<-res[[1]]
sbw<-res[[2]]
theta<-0:179
a<-data.frame(a1=cos(theta*pi/180),a2=sin(theta*pi/180))

ilda <-apply(a,1,function(x){
  x<-as.numeric(x)
  1-det(t(x)%*%swi%*%x)/det(t(x)%*%(swi+sbw)%*%x)
    }
  )

xx <- seq(-1,1,,length)

qplot(theta=a[,1], y=ilda)
+ coord_polar()


