#' Data frame with the 1D projection of each split and plot of importance variable.
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @return a data frame with 1D projection of each split (Alpha.Keep) and a boxplot with the importance of variable for each split.
#' @examples
#' data1<-iris[,5:1]
#' training<-train_fn(iris[,5],.9)
#' output<-bootstrap_pp(data1,scale=TRUE,training,ntree=50,index="LDA")     
#' varimp_pp(data,output)
varimp_pp <- function(data,boot){
  mat.vi <- abs(ldply(boot[[1]], function(x) x[[2]]$Alpha.Keep))
  colnames(mat.vi)[-1] <- colnames(data)[-1]
  mat.vi$sam <- rep(1: sum((mat.vi$tr)==1),dim(mat.vi)[1]/sum((mat.vi$tr)==1))
  aux <- melt(mat.vi,id.vars=c("sam","tr"))
  aux$sam <- factor(aux$sam)
print(qplot(data=aux,x=variable,y=value,geom="boxplot",facets=~sam)+ theme( legend.title=element_blank(),axis.text.y="element_blank()",axis.text.x  = element_text(angle=40, vjust=0.5, size=8),axis.title.y=element_blank(), axis.title.x=element_blank(), legend.direction='vertical' ))
return(mat.vi)
}

