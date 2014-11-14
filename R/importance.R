#' Data frame with the 1D projection of each split weighted by 1- oob error rate for each tree and plots of importance variable for each split and jointly.
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param bagg object form bagging_pp (list with bagging PP.tree error and bagging classification)
#' @return a data frame with 1D projection of each split (Alpha.Keep) and a boxplot with the importance of variable for each split.
#' @export
#' @examples
#' data1<-iris[,5:1]
#' training<-train_fn(iris[,5],.9)
#' output<-bootstrap_pp(data1,scale=TRUE,training,ntree=50,index="LDA")     
#' b.pp <- bagging_pp(data1,scale=TRUE, strata=TRUE,output,training)
#' varimp_pp(data1,output,b.pp) 
varimp_pp <- function(data,boot,bagg){
  mat.vi <- abs(plyr:: ldply(boot[[1]], function(x) x[[2]]$Alpha.Keep))
  colnames(mat.vi)[-1] <- colnames(data)[-1]
  mat.vi$sam <- rep(1: sum((mat.vi$tr)==1),dim(mat.vi)[1]/sum((mat.vi$tr)==1))
  oob.error.tree <- rep(bagg[[5]],each=length(unique(mat.vi$sam)))
  imp.weight <- mat.vi[,colnames(data1[-1])]*(1-oob.error.tree)
  aux <- reshape2::melt(mat.vi,id.vars=c("sam","tr"))
  aux$sam <- factor(aux$sam)
  aux2<- cbind(tr=mat.vi$tr,sam=mat.vi$sam,imp.weight)
  aux22 <- reshape2::melt(aux2,id.vars=c("sam","tr"))
print(ggplot2::qplot(data=aux22,x=variable,y=value,geom="boxplot",facets=~sam) + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)))
print(ggplot2::qplot(data=aux22,x=variable,y=value,geom="boxplot") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)))

       return(aux2)
}

